%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_cluster_mongodb_adapter).

-include("ag_cluster.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").

%% API
-export([check_table/1]).
-export([size/1]).
-export([
    init/0,
    add_table/4
]).
-export([
    write/1,
    write_dirty/1,
    write_single/2
]).
-export([
    read/1, read/2,
    read_dirty/2,
    index_read/3
]).
-export([
    update/3,
    update_dirty/3,
    update_counter_dirty/3
]).
-export([
    delete/2,
    delete_dirty/2,
    delete_index/3,
    delete_object/1
]).

-define(NodeTable, <<"nodeTable">>).

-spec init() ->
    supervisor:startchild_ret().
init() ->
    ag_cluster_sup:start_child(ag_cluster_mongodb_worker).

%% API
-spec add_table(Table :: atom(), Attributes :: list(), Indices :: list(), TTL :: non_neg_integer()) ->
    ok.
add_table(Table, Attributes, Indices, TTL) ->
    create_index(Table, Indices, TTL),
    agdb_database_adapter_mongodb:update(
        ?MONGODB_POOL,
        ?NodeTable,
        #{<<"tablename">>=>agb_convertor:to_binary(Table)},
        #{<<"tablename">>=>agb_convertor:to_binary(Table)},
        true, false),
    ag_cluster_mongodb_worker:add_table(Table, Attributes, Indices, TTL).

%%把对象写入数据库
-spec write(Object :: tuple()) ->
    ok | {failed, Reason :: term()}.
write(Object) when is_tuple(Object), tuple_size(Object) > 2 ->
    try
        TableName = element(1, Object),
        Key = element(2, Object),
        TableInfo = ag_cluster_mongodb_worker:get_table(TableName),
        KeyField = get_keyfield_bypos(TableName, 2),
        ObjPropList = record_to_proplist(Object, TableName),
        PropListObject =
            if
                TableInfo#table_config.ttl == 0 ->

                    normalize_obj([node], ObjPropList);
                true ->
                    normalize_obj([node, {expireTime, TableInfo#table_config.ttl}], ObjPropList)
            end,
        Selector = #{agb_convertor:to_binary(KeyField)=>obj_to_binary(Key)},
        Doc = maps:from_list(PropListObject),
        agdb_database_adapter_mongodb:update(?MONGODB_POOL, TableName, Selector, Doc, true, false)
    catch
        T:R:S ->
            ?LOG_ERROR("ag_cluster_redis_adapter write obj:~p error ~p~n", [Object, {T, R, S}]),
            {failed, R}
    end;
write(_) ->
    {failed, bad_type}.

normalize_obj([], ObjPropList) ->
    ObjPropList;
normalize_obj([node | T], ObjPropList) ->
    case proplists:get_value(<<"node">>, ObjPropList) of
        undefined ->
            normalize_obj(T, ObjPropList ++ [{<<"node">>, obj_to_binary(node())}]);
        _ ->
            normalize_obj(T, ObjPropList)
    end;
normalize_obj([{expireTime, TTL} | T], ObjPropList) ->
    case proplists:get_value(<<"expireTime">>, ObjPropList) of
        undefined ->
            {Mega, Sec, Micro} = os:timestamp(),
            ExpireTime = {Mega, Sec + TTL, Micro},
            normalize_obj(T, ObjPropList ++ [{<<"expireTime">>, ExpireTime}]);
        _ ->
            normalize_obj(T, ObjPropList)
    end.

% 暂时不实现
-spec write_single(Object :: tuple(), CheckFun :: atom()) ->
    ok | {failed, Reason :: term()}.
%write_single(Object, {CheckKey, Operator, CheckValue}) ->
write_single(Object, _) ->
    write(Object).

-spec write_dirty(Object :: tuple()) ->
    ok.
write_dirty(Object) ->
    write(Object).

-spec read(atom()) ->
    {ok, [tuple()]} | {failed, Reason :: term()}.
read(TableName) ->
    try
        read_mongodb(TableName)
    catch
        T:R:S ->
            ?LOG_ERROR("ag_cluster_redis_adapter read table:~s error ~p~n", [TableName, {T, R, S}]),
            {failed, R}
    end.

-spec read(atom(), Key :: term()) ->
    {ok, [tuple()]} | {failed, Reason :: term()}.
read(TableName, Key) ->
    try
        read_mongodb_by_key(TableName, Key)
    catch
        T:R:S ->
            ?LOG_ERROR("ag_cluster_redis_adapter read table:~s key:~p error ~p~n", [TableName, Key, {T, R, S}]),
            {failed, R}
    end.

-spec read_dirty(atom(), Key :: term()) ->
    {ok, []} | {ok, tuple()}.
read_dirty(TableName, Key) ->
    read(TableName, Key).

-spec index_read(atom(), IndexValue :: term(), IndexFieldPos :: non_neg_integer()) ->
    {ok, []} | {ok, tuple()} | {failed, Reason :: term()}.
index_read(TableName, IndexValue, IndexFieldPos) ->
    try
        read_mongodb_by_index(TableName, IndexValue, IndexFieldPos)
    catch
        T:R:S ->
            ?LOG_DEBUG("ag_cluster_redis_adapter index_read  error:~p~n", [{TableName, IndexValue, IndexFieldPos, S, T}]),
            {failed, R}
    end.

-spec delete(Table :: atom(), Key :: term()) ->
    ok | {failed, Reason :: term()}.
delete(TableName, Key) ->
    try
        delete_mongodb(TableName, Key)
    catch
        T:R:S ->
            ?LOG_DEBUG("ag_cluster_redis_adapter delete  error:~p~n", [{TableName, Key, R, S, T}]),
            {failed, R}
    end.

-spec delete_dirty(Tab :: atom(), Key :: term()) ->
    'ok'.
delete_dirty(Table, Key) ->
    delete_mongodb(Table, Key).

-spec delete_object(Object :: tuple()) ->
    ok | {failed, Reason :: term()}.
delete_object(Object) ->
    try
        delete_mongodb(Object)
    catch
        T:R:S ->
            ?LOG_ERROR("ag_cluster_redis_adapter delete_object error ~p~n", [{Object, R, S, T}]),
            {failed, R}
    end.

-spec delete_index(atom(), SecondaryKey :: term(), Pos :: non_neg_integer()) ->
    ok | {failed, Reason :: term()}.
delete_index(Table, SecondaryKey, Pos) ->
    try
        delete_mongodb_by_index(Table, SecondaryKey, Pos)
    catch
        T:R:S ->
            ?LOG_DEBUG("ag_cluster_redis_adapter delete_index error:~p~n", [{Table, SecondaryKey, Pos, R, S, T}]),
            {failed, R}
    end.

-spec update(atom(), TableKey :: term(), UpdateList :: [tuple()]) ->
    ok | {failed, Reason :: term()}.
update(Table, TableKey, UpdateList) ->
    try
        update_mongodb(Table, TableKey, UpdateList)
    catch
        T:R:S ->
            ?LOG_DEBUG("ag_cluster_redis_adapter update error:~p~n", [{Table, TableKey, UpdateList, R, S, T}]),
            {failed, R}
    end.

-spec update_dirty(Table :: atom(), TableKey :: term(), UpdateList :: [tuple()]) ->
    ok | error.
update_dirty(Table, TableKey, UpdateList) ->
    try
        update_mongodb(Table, TableKey, UpdateList)
    catch
        T:R:S ->
            ?LOG_DEBUG("ag_cluster_redis_adapter udpate_dirty error:~p~n", [{Table, TableKey, UpdateList, R, S, T}]),
            error
    end.

-spec update_counter_dirty(Table :: atom(), Key :: term(), Incr :: integer()) ->
    NewVal :: integer().
update_counter_dirty(Table, Key, Incr) ->
    update_counter_mongodb(Table, Key, Incr).

-spec(check_table(Table :: [atom()]) ->
    boolean()).
check_table(Table) ->
    ag_cluster_mongodb_worker:check_table(Table).

-spec size(atom()) ->
    non_neg_integer().
size(Table) ->
    try
        size_mongodb(Table)
    catch
        T:R:S ->
            ?LOG_DEBUG("ag_cluster_redis_adapter get size error:~p~n", [{R, S, T}]),
            0
    end.

%%----------------------------------------------------------------------------------------------------------------------
%%----------------------------------------------------------------------------------------------------------------------
get_keyfield_bypos(TableName, Pos) ->
    TableInfo = ag_cluster_mongodb_worker:get_table(TableName),
    Attribute = TableInfo#table_config.attribute,
    lists:nth(Pos - 1, Attribute).

delete_mongodb(TableName, Key) ->
    KeyField = get_keyfield_bypos(TableName, 2),
    Selector = #{agb_convertor:to_binary(KeyField)=>obj_to_binary(Key)},
    agdb_database_adapter_mongodb:delete(?MONGODB_POOL, TableName, Selector).

delete_mongodb(Object) ->
    TableName = erlang:element(1, Object),
    Key = erlang:element(2, Object),
    delete_mongodb(TableName, Key).

delete_mongodb_by_index(TableName, IndexValue, IndexFieldPos) ->
    IndexField = get_keyfield_bypos(TableName, IndexFieldPos),
    agdb_database_adapter_mongodb:delete(?MONGODB_POOL, TableName, #{IndexField=>obj_to_binary(IndexValue)}).

read_mongodb(TableName) ->
    read_mongodb(TableName, #{}).

read_mongodb(TableName, Select) ->
    case agdb_database_adapter_mongodb:find(?MONGODB_POOL, TableName, Select) of
        [] ->
            {ok, []};
        Objs ->
            update_objs_ttl(TableName, Objs),
            {ok, [map_to_record(TableName, Obj) || Obj <- Objs]}
    end.

read_mongodb_by_key(TableName, Key) ->
    KeyField = get_keyfield_bypos(TableName, 2),
    Selector = #{agb_convertor:to_binary(KeyField)=>obj_to_binary(Key)},
    case agdb_database_adapter_mongodb:find_one(?MONGODB_POOL, TableName, Selector) of
        undefined ->
            {ok, []};
        Obj ->
            update_objs_ttl(TableName, Obj),
            {ok, [map_to_record(TableName, Obj)]}
    end.

read_mongodb_by_index(TableName, IndexValue, IndexFieldPos) ->
    IndexField = get_keyfield_bypos(TableName, IndexFieldPos),
    read_mongodb(TableName, #{agb_convertor:to_binary(IndexField)=>obj_to_binary(IndexValue)}).

%%获取单个表的长度，有多少这个表的kv对象
size_mongodb(TableName) ->
    agdb_database_adapter_mongodb:count(?MONGODB_POOL, TableName, #{}).

update_mongodb(TableName, Key, UpdateList) ->
    KeyField = get_keyfield_bypos(TableName, 2),
    UpdateList1 =
        lists:map(
            fun({A, B}) ->
                {agb_convertor:to_binary(get_keyfield_bypos(TableName, A)), obj_to_binary(B)}
            end,
            UpdateList
        ),
    Selector = #{agb_convertor:to_binary(KeyField)=>obj_to_binary(Key)},
    agdb_database_adapter_mongodb:update(?MONGODB_POOL, TableName, Selector, maps:from_list(UpdateList1)).

%  修改自增数据 这是兼容mnesia自增id特殊写的，attribue只有2个属性，第二个就是自增id
update_counter_mongodb(TableName, Key, Incr) ->
    TableInfo = ag_cluster_mongodb_worker:get_table(TableName),
    Attribute = TableInfo#table_config.attribute,
    [KeyField, CountField | _] = Attribute,
    Selector = #{agb_convertor:to_binary(KeyField)=>obj_to_binary(Key)},
    Doc = #{agb_convertor:to_binary(CountField)=>Incr},
    agdb_database_adapter_mongodb:inc(?MONGODB_POOL, TableName, Selector, Doc),
    0.

update_objs_ttl(TableName, Obj) when is_map(Obj) ->
    TableInfo = ag_cluster_mongodb_worker:get_table(TableName),
    if
        TableInfo#table_config.ttl == 0 ->
            ignore;
        true ->
            #{<<"expireTime">>:=ObjExpireTime} = Obj,
            {Mega, Sec, Micro} = os:timestamp(),
            TimeDiff = timer:now_diff({Mega, Sec, Micro}, ObjExpireTime),
            if
                TimeDiff > 10 * 1000 * 1000 ->
                    KeyField = get_keyfield_bypos(TableName, 2),
                    Key = maps:get(agb_convertor:to_binary(KeyField), Obj),
                    ExpireTime = {Mega, Sec + TableInfo#table_config.ttl, Micro},
                    Selector = #{agb_convertor:to_binary(KeyField)=>obj_to_binary(Key)},
                    Doc = #{<<"expireTime">>=>ExpireTime},
                    agdb_database_adapter_mongodb:update(?MONGODB_POOL, TableName, Selector, Doc);
                true ->
                    ignore
            end
    end;
update_objs_ttl(TableName, Objs) when is_list(Objs) ->
    [update_objs_ttl(TableName, Obj) || Obj <- Objs].

create_index(Table, Indices, 0) ->
    Index = [{<<"key">>, {<<"node">>, 1}, <<"name">>, <<"index_node">>}],
    agdb_database_adapter_mongodb:ensure_index(?MONGODB_POOL, Table, Index),
    lists:foreach(
        fun(Index) ->
            IndexBin = agb_convertor:to_binary(Index),
            agdb_database_adapter_mongodb:ensure_index(?MONGODB_POOL, Table,
                [{<<"key">>, {IndexBin, 1}, <<"name">>, <<"index_", IndexBin/binary>>}])
        end,
        Indices
    );
create_index(Table, Indices, _) ->
    IndexSpec = [{<<"key">>, {<<"expireTime">>, 1}, <<"name">>, <<"index_expireTime">>, <<"expireAfterSeconds">>, 0}],
    agdb_database_adapter_mongodb:ensure_index(?MONGODB_POOL, Table, IndexSpec),
    create_index(Table, Indices, 0).

obj_to_binary(Obj) ->
    if
        is_integer(Obj) ->
            Obj;
        true ->
            term_to_binary(Obj)
    end.

to_obj(Value) when is_integer(Value) ->
    Value;
to_obj(Bin) ->
    try
        binary_to_term(Bin)
    catch error:badarg ->
        try
            binary_to_integer(Bin)
        catch error:badarg ->
            error
        end
    end.

record_to_proplist(RecordObj, TableName) ->
    [_ | RecordObjList] = tuple_to_list(RecordObj),
    RecordObjStrList =
        lists:map(
            fun(Obj) ->
                obj_to_binary(Obj)
            end,
            RecordObjList
        ),
    TableInfo = ag_cluster_mongodb_worker:get_table(TableName),
    Attribute = TableInfo#table_config.attribute,
    AttributeBin = [agb_convertor:to_binary(Field) || Field <- Attribute],
    lists:zip(AttributeBin, RecordObjStrList).

map_to_record(TableName, Map) ->
    TableInfo = ag_cluster_mongodb_worker:get_table(TableName),
    Attribute = TableInfo#table_config.attribute,
    RecordList =
        [TableName] ++ lists:map(
        fun(Field) ->
            to_obj(maps:get(agb_convertor:to_binary(Field), Map))
        end,
        Attribute
    ),
    list_to_tuple(RecordList).

