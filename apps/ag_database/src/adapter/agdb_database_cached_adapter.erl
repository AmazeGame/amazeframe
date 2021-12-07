%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.29
%%%-------------------------------------------------------------------
%%%

-module(agdb_database_cached_adapter).

-include_lib("ag_base/include/agb_debuglogger.hrl").

-export([init/3]).

-export([find_one/4, find_one/5]).
-export([find_keys/4]).
-export([insert_one/5]).
-export([update_one/5]).
-export([delete_one/4]).

-export([find_one_and_set_cached_time/5, find_one_and_set_cached_time/6]).
-export([
    insert_one_and_set_cached_time/6,
    update_one_and_set_cached_time/6
]).
-export([
    find_bagobj/4,
    find_bagobj_and_set_cached_time/5,
    update_one_bagobj_and_set_cached_time/6,
    insert_bagobj_and_set_cachedTime/6
]).

-export([delete_keys/4]).
-export([
    update_one_bagobj/5,
    insert_bagobj/5,
    remove_one_bagobj/4,
    clean_bagobj/4,
    remove_multi_bagobj/4
]).

-type bag_obj_select() ::
    #{
        owner_key => binary(),
        owner_k_value => binary(),
        element_key => binary(),
        element_k_value => binary()|[binary()]
    }.

-spec init(atom(), agdb_manager:driver_args(), agdb_manager:driver_args()) ->
    ok.
init(DriverDBCachedPoolName, {DBPoolName, DBDriver, DBDriverOpts}, {CachedPoolName, CachedDriver, CachedDriverOpts}) ->
    {ok, DBAdapter} = agdb_database_adapter:init(DBDriver, DBPoolName, DBDriverOpts),
    {ok, CachedAdapter} = agdb_cached_adapter:init(CachedDriver, CachedPoolName, CachedDriverOpts),
    agdb_config:put({?MODULE, DriverDBCachedPoolName, db}, {DBAdapter, DBPoolName}),
    agdb_config:put({?MODULE, DriverDBCachedPoolName, cached}, {CachedAdapter, CachedPoolName}),
    ok.

-spec db_adapter(atom()) ->
    {module(), atom()}.
db_adapter(DriverDBCachedPoolName) ->
    case agdb_config:get({?MODULE, DriverDBCachedPoolName, db}) of
        undefined ->
            undefined;
        {_, AdapterInfo} ->
            AdapterInfo
    end.

-spec cached_adapter(atom()) ->
    {module(), atom()}.
cached_adapter(DriverDBCachedPoolName) ->
    case agdb_config:get({?MODULE, DriverDBCachedPoolName, cached}) of
        undefined ->
            unedefined;
        {_, AdapterInfo} ->
            AdapterInfo
    end.

-spec find_keys(DriverPoolName :: atom(), Table :: binary(), KeyName :: binary(), KeyValues :: list()) ->
    [] | [map()].
find_keys(DriverPoolName, Table, KeyName, KeyValues) ->
    lists:filtermap(
        fun(KeyValue) ->
            case find_one(DriverPoolName, Table, KeyName, KeyValue) of
                undefined ->
                    false;
                {error, _Reason} ->
                    false;
                Obj ->
                    {true, Obj}
            end
        end,
        KeyValues
    ).

-spec find_one(DriverPoolName :: atom(), Table :: binary(), PrimaryKeyField :: binary(), PrimaryKeyValue :: term()) ->
    undefined | map() | {error, Reason :: term()}.
find_one(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue) ->
    find_one_and_set_cached_time(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, 0).

-spec find_one(DriverPoolName :: atom(), Table :: binary(), PrimaryKeyField :: binary(), PrimaryKeyValue :: term(),
    Projection :: list()) ->
    undefined | map() | {error, Reason :: term()}.
find_one(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, Projection) ->
    find_one_and_set_cached_time(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, Projection, 0).

%% add by ayongbc <ayongbc@sina.com> 2019/11/4 添加可以设置过期时间功能
-spec find_one_and_set_cached_time(
    DriverPoolName :: atom(),
    Table :: binary(),
    PrimaryKeyField :: binary(),
    PrimaryKeyValue :: term(),
    CachedExpireTime :: integer()) -> undefined | map() | {error, Reason :: term()}.
find_one_and_set_cached_time(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, CachedExpireTime) ->
    {CachedAdapter, CachedPoolName} = cached_adapter(DriverPoolName),
    CacheKey = make_key(Table, PrimaryKeyValue),
    case CachedAdapter:hgetall(CachedPoolName, CacheKey) of
        {ok, []} ->
            insert_cached_from_db(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, CachedExpireTime);
        {ok, KeyValues} ->
            if
                CachedExpireTime > 0 ->
                    {ok, set} = CachedAdapter:expire(CachedPoolName, CacheKey, CachedExpireTime);
                true ->
                    ignore
            end,
            make_kvlist_to_obj(KeyValues, #{});
        {error, Reason} ->
            {error, Reason}
    end.

-spec find_one_and_set_cached_time(
    DriverPoolName :: atom(),
    Table :: binary(),
    PrimaryKeyField :: binary(),
    PrimaryKeyValue :: term(),
    Projection :: list(),
    CachedExpireTime :: integer()) ->
    undefined | map() | {error, Reason :: term()}.
find_one_and_set_cached_time(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, Projection, CachedExpireTime) ->
    {CachedAdapter, CachedPoolName} = cached_adapter(DriverPoolName),
    CacheKey = make_key(Table, PrimaryKeyValue),
    case CachedAdapter:exists(CachedPoolName, CacheKey) of
        {ok, exist} ->
            if
                CachedExpireTime > 0 ->
                    {ok, set} = CachedAdapter:expire(CachedPoolName, CacheKey, CachedExpireTime);
                true ->
                    ignore
            end,
            find_one_for_exist(CachedAdapter, CachedPoolName, CacheKey, Projection);
        {ok, not_exist} ->
            {DBAdapter, DBPoolName} = db_adapter(DriverPoolName),
            case DBAdapter:find_one(DBPoolName, Table, #{PrimaryKeyField => PrimaryKeyValue}) of
                undefined ->
                    undefined;
                BinObj ->
                    if
                        CachedExpireTime > 0 ->
                            Command = [["hmset", CacheKey | make_obj_to_kvlist(BinObj)],
                                ["expire", CacheKey, CachedExpireTime]],
                            [{ok, _} | _] = CachedAdapter:qp(CachedPoolName, Command);
                        true ->
                            {ok, saved} = CachedAdapter:hmset(CachedPoolName, CacheKey, make_obj_to_kvlist(BinObj))
                    end,
                    maps:filter(fun(K, _) ->
                        lists:member(K, Projection) end, BinObj)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

find_one_for_exist(CachedAdapter, CachedPoolName, CacheKey, Projection) ->
    case CachedAdapter:hmget(CachedPoolName, CacheKey, Projection) of
        {ok, Values} ->
            KeyValues = lists:flatten(lists:zipwith(fun(A, B) ->
                [agb_convertor:to_binary(A), agb_convertor:to_binary(B)] end, Projection, Values)),
            make_kvlist_to_obj(KeyValues, #{});
        {error, Reason1} ->
            {error, Reason1}
    end.

-spec find_bagobj(
    DriverPoolName :: atom(),
    Table :: binary(),
    BagName :: binary(),
    BagObjSel :: bag_obj_select()
) ->
    list() | {error, Reason :: term()}.
find_bagobj(DriverPoolName, Table, BagName, BagObjSel) ->
    find_bagobj_and_set_cached_time(DriverPoolName, Table, BagName, BagObjSel, 0).

%% add by ayongbc <ayongbc@sina.com> 2019/11/4 增加查找数组对象并且设置对象cached的生成时间函数
-spec find_bagobj_and_set_cached_time(
    DriverPoolName :: atom(),
    Table :: binary(),
    BagName :: binary(),
    BagObjSel :: bag_obj_select(),
    ExpireTime :: integer()
) ->
    list() | {error, Reason :: term()}.
find_bagobj_and_set_cached_time(DriverPoolName, Table, BagName, BagObjSel, ExpireTime) ->
    #{
        owner_key := OwnerKeyField,
        owner_k_value := OwnerKeyValue,
        element_key := ElementKey
    } = BagObjSel,
    {CachedAdapter, CachedPoolName} = cached_adapter(DriverPoolName),
    CacheBagKey = make_bag_key(Table, BagName, OwnerKeyValue),
    ?LOG_DEBUG("-----find_bagobj CacheBagKey:~p", [CacheBagKey]),
    case CachedAdapter:smembers(CachedPoolName, CacheBagKey) of
        {ok, []} ->
            {DBAdapter, DBPoolName} = db_adapter(DriverPoolName),
            case DBAdapter:find_bag(DBPoolName, Table, BagName, #{OwnerKeyField => OwnerKeyValue}) of
                [] ->
                    [];
                BinObjs ->
                    ?LOG_DEBUG("-----find_bagobj BinObjs:~p", [BinObjs]),
                    lists:map(
                        fun(BinObj) ->
                            BinObj1 = maps:remove(OwnerKeyField, BinObj),
                            ElementKeyV = maps:get(ElementKey, BinObj1),
                            CacheKey = make_bagobj_key(Table, BagName, OwnerKeyValue, ElementKeyV),
                            Command =
                                if
                                    ExpireTime == 0 ->
                                        [["sadd", CacheBagKey, CacheKey],
                                            ["hmset", CacheKey | make_obj_to_kvlist(BinObj1)]];
                                    true ->
                                        [
                                            ["sadd", CacheBagKey, CacheKey],
                                            ["hmset", CacheKey | make_obj_to_kvlist(BinObj1)],
                                            ["EXPIRE", CacheBagKey, ExpireTime],
                                            ["EXPIRE", CacheKey, ExpireTime]
                                        ]
                                end,
                            [{ok, _} | _] = CachedAdapter:qp(CachedPoolName, Command),
                            BinObj1
                        end, BinObjs)
            end;
        {ok, Values} ->
            ?LOG_DEBUG("-----find_bagobj Values:~p", [Values]),
            GetCommand = [["hgetall", Key] || Key <- Values],
            Result = CachedAdapter:qp(CachedPoolName, GetCommand),
            if
                ExpireTime > 0 ->
                    SetExpireTimeCommand = [["EXPIRE", Key, ExpireTime] || Key <- Values],
                    CachedAdapter:qp(
                        CachedPoolName,
                        SetExpireTimeCommand ++ [["EXPIRE", CacheBagKey, ExpireTime]]
                    );
                true ->
                    ignore
            end,
            ?LOG_DEBUG("-----find_bagobj Result:~p", [Result]),
            lists:map(fun({ok, BagObjKV}) ->
                make_kvlist_to_obj(BagObjKV, #{}) end, Result);
        {error, Reason} ->
            {error, Reason}
    end.

-spec make_obj_to_kvlist(map()) ->
    list().
make_obj_to_kvlist(ObjMap) ->
    lists:concat([[agb_convertor:to_binary(K), term_to_binary(V)] || {K, V} <- maps:to_list(ObjMap)]).

-spec make_kvlist_to_obj(list(), map()) ->
    map().
make_kvlist_to_obj([], ObjMap) ->
    ObjMap;
make_kvlist_to_obj([Key, <<"undefined">> | T], ObjMap) ->
    make_kvlist_to_obj(T, maps:put(agb_convertor:to_binary(Key), <<>>, ObjMap));
make_kvlist_to_obj([Key, Value | T], ObjMap) ->
    make_kvlist_to_obj(T, maps:put(agb_convertor:to_binary(Key), binary_to_term(Value), ObjMap)).

-spec insert_one(DriverPoolName :: atom(), Table :: binary(), PrimaryKeyField :: binary(), PrimaryKeyValue :: term(),
    Object :: map()) ->
    ok | {error, Reason :: term()}.
insert_one(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, Object) ->
    insert_one_and_set_cached_time(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, Object, 0).

%add by ayongbc <ayongbc@sina.com> 2019/11/4 插入函数加入cached过期时间参数
-spec insert_one_and_set_cached_time(
    DriverPoolName :: atom(),
    Table :: binary(),
    PrimaryKeyField :: binary(),
    PrimaryKeyValue :: term(),
    Object :: map(),
    CachedExpireTime :: integer()) ->
    ok | {error, Reason :: term()}.
insert_one_and_set_cached_time(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, Object, CachedExpireTime) ->
    {DBAdapter, DBPoolName} = db_adapter(DriverPoolName),
    case DBAdapter:insert(DBPoolName, Table, Object#{PrimaryKeyField => PrimaryKeyValue}) of
        ok ->
            CacheKey = make_key(Table, PrimaryKeyValue),
            {CachedAdapter, CachedPoolName} = cached_adapter(DriverPoolName),
            if
                CachedExpireTime > 0 ->
                    Command =
                        [
                            ["hmset", CacheKey | make_obj_to_kvlist(Object#{PrimaryKeyField => PrimaryKeyValue})],
                            ["expire", CacheKey, CachedExpireTime]
                        ],
                    [{ok, _} | _] = CachedAdapter:qp(CachedPoolName, Command);
                true ->
                    {ok, saved} = CachedAdapter:hmset(CachedPoolName, CacheKey,
                        make_obj_to_kvlist(Object#{PrimaryKeyField => PrimaryKeyValue}))
            end,
            ok;
        {error, Reason} ->
            ?LOG_ERROR("agdb_database_cached_adapter insert_one error Reason~p~n", [Reason]),
            {error, Reason}
    end.

-spec insert_bagobj(
    DriverPoolName :: atom(),
    Table :: binary(),
    BagName :: binary(),
    BagObjSel :: bag_obj_select(),
    Obj :: map()) -> ok | {error, Reason :: term()}.
insert_bagobj(DriverPoolName, Table, BagName, BagObjSel, Obj) ->
    update_one_bagobj(DriverPoolName, Table, BagName, BagObjSel, Obj).

-spec insert_bagobj_and_set_cachedTime(
    Pool :: atom(),
    Table :: binary(),
    BagName :: binary(),
    BagObjSel :: bag_obj_select(),
    Obj :: map(),
    ExpireTime :: integer()) -> ok | {error, Reason :: term()}.
insert_bagobj_and_set_cachedTime(Pool, Table, BagName, BagObjSel, Obj, ExpireTime) ->
    update_one_bagobj_and_set_cached_time(Pool, Table, BagName,BagObjSel, Obj, ExpireTime).

-spec update_one(DriverPoolName :: atom(), Table :: binary(), PrimaryKeyField :: binary(), PrimaryKeyValue :: term(),
    Object :: map()) ->
    ok | {error, Reason :: term()}.
update_one(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, Object) ->
    update_one_and_set_cached_time(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, Object, 0).

%% add by ayongbc <ayongbc@sina.com> 2019/11/4 添加更新内容同时更新cached的存活时间函数
-spec update_one_and_set_cached_time(
    DriverPoolName :: atom(),
    Table :: binary(),
    PrimaryKeyField :: binary(),
    PrimaryKeyValue :: term(),
    Object :: map(),
    CachedExpireTime :: integer()
) ->
    ok | {error, Reason :: term()}.
update_one_and_set_cached_time(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, Object, CachedExpireTime) ->
    CacheKey = make_key(Table, PrimaryKeyValue),
    {DBAdapter, DBPoolName} = db_adapter(DriverPoolName),
    {CachedAdapter, CachedPoolName} = cached_adapter(DriverPoolName),
    case CachedAdapter:exists(CachedPoolName, CacheKey) of
        {ok, exist} ->
            ignore;
        {ok, not_exist} ->
            case insert_cached_from_db(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, CachedExpireTime) of
                undefined ->
                    agb_error:error("insert_cached_from_db error notfount PrimaryKeyValue:~p~n", [PrimaryKeyValue]);
                _ ->
                    ignore
            end
    end,
    case DBAdapter:update(DBPoolName, Table, #{PrimaryKeyField => PrimaryKeyValue}, Object) of
        ok ->
            {CachedAdapter, CachedPoolName} = cached_adapter(DriverPoolName),
            if
                CachedExpireTime > 0 ->
                    Command = [
                        ["hmset", CacheKey | make_obj_to_kvlist(Object)],
                        ["expire", CacheKey, CachedExpireTime]
                    ],
                    [{ok, _} | _] = CachedAdapter:qp(CachedPoolName, Command);
                true ->
                    {ok, saved} = CachedAdapter:hmset(CachedPoolName, CacheKey, make_obj_to_kvlist(Object))
            end,
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

insert_cached_from_db(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue, CachedExpireTime) ->
    CacheKey = make_key(Table, PrimaryKeyValue),
    {DBAdapter, DBPoolName} = db_adapter(DriverPoolName),
    {CachedAdapter, CachedPoolName} = cached_adapter(DriverPoolName),
    case DBAdapter:find_one(DBPoolName, Table, #{PrimaryKeyField => PrimaryKeyValue}) of
        undefined ->
            undefined;
        BinObj ->
            if
                CachedExpireTime > 0 ->
                    Command = [["hmset", CacheKey | make_obj_to_kvlist(BinObj)],
                        ["expire", CacheKey, CachedExpireTime]],
                    [{ok, _} | _] = CachedAdapter:qp(CachedPoolName, Command);
                true ->
                    {ok, saved} = CachedAdapter:hmset(CachedPoolName, CacheKey, make_obj_to_kvlist(BinObj))
            end,
            BinObj
    end.

-spec update_one_bagobj(
    Pool :: atom(),
    Table :: binary(),
    BagName :: binary(),
    BagObjSel::bag_obj_select(),
    Obj :: map()) -> ok | {error, Reason :: term()}.
update_one_bagobj(Pool, Table, BagName, BagObjSel, Obj) ->
    update_one_bagobj_and_set_cached_time(Pool, Table, BagName, BagObjSel, Obj, 0).

-spec update_one_bagobj_and_set_cached_time(
    Pool :: atom(),
    Table :: binary(),
    BagName :: binary(),
    BagObjSel::bag_obj_select(),
    Obj :: map(),
    ExpireTime :: integer()) -> ok | {error, Reason :: term()}.
update_one_bagobj_and_set_cached_time(Pool, Table, BagName, BagObjSel, Obj, ExpireTime) ->
    #{
        owner_key := OwnerField,
        owner_k_value := OwnerVal,
        element_key := ElemK,
        element_k_value := ElemKV
    } = BagObjSel,
    CacheKey = make_bagobj_key(Table, BagName, OwnerVal, ElemKV),
    CacheBagKey = make_bag_key(Table, BagName, OwnerVal),
    {CachedAdapter, CachedPoolName} = cached_adapter(Pool),
    {DBAdapter, DBPoolName} = db_adapter(Pool),
    ?LOG_DEBUG("agdb_database_cached_adapter update_one_bagobj CacheKey:[~p]~n", [{ElemKV, OwnerVal, CacheKey}]),
    case CachedAdapter:exists(CachedPoolName, CacheKey) of
        {ok, not_exist} ->
            %% add
            ?LOG_DEBUG("~p add_one_bagobj Object:[~p]~n", [?MODULE, {OwnerField, OwnerVal, Obj}]),
            TMap = #{
                adapter => DBAdapter, poolname => DBPoolName, table => Table, bagname => BagName, field => OwnerField,
                val => OwnerVal, expiretime => ExpireTime, cachebagkey => CacheBagKey, cachekey => CacheKey,
                cached_adapter => CachedAdapter, cached_poolname => CachedPoolName
            },
            update_one_bagobj_for_exist(TMap, Obj);
        {ok, exist} ->
            %% update
            ?LOG_DEBUG("~p update_one_bagobj Object:[~p]~n", [?MODULE, {OwnerField, OwnerVal, Obj}]),
            PriMap = #{ElemK => ElemKV},
            OwnerMap = #{OwnerField => OwnerVal},
            TMap1 = #{
                adapter => DBAdapter, poolname => DBPoolName, table => Table, bagname => BagName, pri_map => PriMap,
                owner_map => OwnerMap, expiretime => ExpireTime, cachebagkey => CacheBagKey, cachekey => CacheKey,
                cached_adapter => CachedAdapter, cached_poolname => CachedPoolName
            },
            update_one_bagobj_for_not_exist(TMap1, Obj);
        {error, Reason} ->
            {error, Reason}
    end.

update_one_bagobj_for_exist(TMap, Obj) ->
    #{
        adapter := DBAdapter, poolname := DBPoolName, table := Table, bagname := BagName, field := OwnerField,
        val := OwnerVal, expiretime := ExpireTime, cachebagkey := CacheBagKey, cachekey := CacheKey,
        cached_adapter := CachedAdapter, cached_poolname := CachedPoolName
    } = TMap,
    case DBAdapter:add_one_bagobj(DBPoolName, Table, BagName, #{OwnerField => OwnerVal}, Obj) of
        ok ->
            Command =
                if
                    ExpireTime > 0 ->
                        [
                            ["sadd", CacheBagKey, CacheKey],
                            ["hmset", CacheKey | make_obj_to_kvlist(Obj)],
                            ["expire", CacheKey, ExpireTime],
                            ["expire", CacheBagKey, ExpireTime]
                        ];
                    true ->
                        [
                            ["sadd", CacheBagKey, CacheKey],
                            ["hmset", CacheKey | make_obj_to_kvlist(Obj)]
                        ]
                end,
            [{ok, _} | _] = CachedAdapter:qp(CachedPoolName, Command),
            ok;
        {error, Reason1} ->
            ?LOG_DEBUG("agdb_database_cached_adapter add_one_bagobj error ~p~n", [Reason1]),
            {error, Reason1}
    end.

update_one_bagobj_for_not_exist(TMap1, Obj) ->
    #{
        adapter := DBAdapter, poolname := DBPoolName, table := Table, bagname := BagName, pri_map := PriMap,
        owner_map := OwnerMap, expiretime := ExpireTime, cachebagkey := CacheBagKey, cachekey := CacheKey,
        cached_adapter := CachedAdapter, cached_poolname := CachedPoolName
    } = TMap1,
    case DBAdapter:update_one_bagobj(DBPoolName, Table, BagName, PriMap, OwnerMap, Obj) of
        ok ->
            if
                ExpireTime > 0 ->
                    Command = [
                        ["hmset", CacheKey | make_obj_to_kvlist(Obj)],
                        ["expire", CacheKey, ExpireTime],
                        ["expire", CacheBagKey, ExpireTime]
                    ],
                    [{ok, _} | _] = CachedAdapter:qp(CachedPoolName, Command);
                true ->
                    {ok, saved} = CachedAdapter:hmset(CachedPoolName, CacheKey, make_obj_to_kvlist(Obj))
            end,
            ok;
        {error, Reason1} ->
            {error, Reason1}
    end.

-spec remove_one_bagobj(
    DriverPoolName :: atom(),
    Table :: binary(),
    BagName :: binary(),
    BagObjSel :: bag_obj_select()) ->
    ok | {error, Reason :: term()}.
remove_one_bagobj(DriverPoolName, Table, BagName, BagObjSel) ->
    #{
        owner_key := OwnerField,
        owner_k_value := OwnerVal,
        element_key := ElemK,
        element_k_value := ElemKV
    } = BagObjSel,
    {DBAdapter, DBPoolName} = db_adapter(DriverPoolName),
    PriMap = #{ElemK => ElemKV},
    OwnerMap = #{OwnerField => OwnerVal},
    case DBAdapter:remove_bagobj(DBPoolName, Table, BagName, PriMap, OwnerMap) of
        ok ->
            CacheKey = make_bagobj_key(Table, BagName, OwnerVal, ElemKV),
            CacheBagKey = make_bag_key(Table, BagName, OwnerVal),
            {CachedAdapter, CachedPoolName} = cached_adapter(DriverPoolName),
            CachedAdapter:srem(CachedPoolName, CacheBagKey, [CacheKey]),
            {ok, _Count} = CachedAdapter:del(CachedPoolName, CacheKey),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec remove_multi_bagobj(
    DriverPoolName :: atom(),
    Table :: binary(),
    BagName :: binary(),
    BagObjSel :: bag_obj_select()) -> ok | {error, Reason :: term()}.
remove_multi_bagobj(DriverPoolName, Table, BagName, BagObjSel) ->
    #{
        owner_key := OwnerKey,
        owner_k_value := OwnerKV,
        element_key := ElemK,
        element_k_value := ElemKVs
    } = BagObjSel,
    {DBAdapter, DBPoolName} = db_adapter(DriverPoolName),
    Where1 = [{ElemK, in, ElemKVs}],
    Where2 = #{OwnerKey => OwnerKV},
    case DBAdapter:remove_bagobj(DBPoolName, Table, BagName, Where1, Where2) of
        ok ->
            {CachedAdapter, CachedPoolName} = cached_adapter(DriverPoolName),
            Command =
                lists:foldl(
                    fun(PrimaryKeyValue, P) ->
                        CacheKey = make_bagobj_key(Table, BagName, OwnerKV, PrimaryKeyValue),
                        CacheBagKey = make_bag_key(Table, BagName, OwnerKV),
                        P ++ [["srem", CacheBagKey, CacheKey], ["del", CacheKey]]
                    end,
                    [],
                    ElemKVs
                ),
            [{ok, _} | _] = CachedAdapter:qp(CachedPoolName, Command),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec clean_bagobj(
    DriverPoolName :: atom(),
    Table :: binary(),
    BagName :: binary(),
    BagObjSel :: bag_obj_select()) -> ok | {error, Reason :: term()}.
clean_bagobj(DriverPoolName, Table, BagName, BagObjSel) ->
    #{
        owner_key := OwnerKey,
        owner_k_value := OwnerKV
    } = BagObjSel,
    {DBAdapter, DBPoolName} = db_adapter(DriverPoolName),
    case DBAdapter:delete_bagobj(DBPoolName, Table, BagName, #{OwnerKey => OwnerKV}) of
        ok ->
            CacheBagKey = make_bag_key(Table, BagName, OwnerKV),
            {CachedAdapter, CachedPoolName} = cached_adapter(DriverPoolName),
            case CachedAdapter:smembers(CachedPoolName, CacheBagKey) of
                {ok, []} ->
                    ok;
                {ok, Values} ->
                    P = [["del", Id] || Id <- Values],
                    [{ok, _} | _] = CachedAdapter:qp(CachedPoolName, P),
                    ok;
                {error, Reason1} ->
                    {error, Reason1}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete_one(DriverPoolName :: atom(), Table :: binary(), PrimaryKeyField :: binary(), PrimaryKeyValue :: term()) ->
    ok | {error, Reason :: term()}.
delete_one(DriverPoolName, Table, PrimaryKeyField, PrimaryKeyValue) ->
    {DBAdapter, DBPoolName} = db_adapter(DriverPoolName),
    case DBAdapter:delete(DBPoolName, Table, #{PrimaryKeyField => PrimaryKeyValue}) of
        ok ->
            CacheKey = make_key(Table, PrimaryKeyValue),
            {CachedAdapter, CachedPoolName} = cached_adapter(DriverPoolName),
            {ok, _Count} = CachedAdapter:del(CachedPoolName, CacheKey),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec delete_keys(DriverPoolName :: atom(), Table :: binary(), KeyName :: binary(), KeyValues :: list()) ->
    ok.
delete_keys(DriverPoolName, Table, KeyName, KeyValues) ->
    lists:foreach(
        fun(KeyValue) ->
            delete_one(DriverPoolName, Table, KeyName, KeyValue)
        end,
        KeyValues
    ).

make_key(Table, Value) ->
    <<(agb_convertor:to_binary(Table))/binary, $., (agb_convertor:to_binary(Value))/binary, $.>>.

make_bagobj_key(Table, BagName, Owner, Key) ->
    <<(agb_convertor:to_binary(Table))/binary, $., (agb_convertor:to_binary(BagName))/binary, $.,
        (agb_convertor:to_binary(Owner))/binary, $., (agb_convertor:to_binary(Key))/binary, $.>>.

make_bag_key(Table, BagName, Owner) ->
    <<(agb_convertor:to_binary(Table))/binary, $., (agb_convertor:to_binary(BagName))/binary, $.,
        (agb_convertor:to_binary(Owner))/binary>>.