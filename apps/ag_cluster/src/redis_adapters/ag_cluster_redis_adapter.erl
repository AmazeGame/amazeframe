%%%-------------------------------------------------------------------
%%% @author ayongbc <ayongbc@sina.com> 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_cluster_redis_adapter).


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
    index_read/3]
).
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

-define(ADD_SLOT_KEY(Key), "{ag_cluster}" ++ agb_convertor:to_string(Key)).
-define(VERIFICATION_WRITE, "{ag_cluster}verification_write").
-define(WRITE, "{ag_cluster}write").
-define(INDEX_READ, "{ag_cluster}index_read").
-define(READ, "{ag_cluster}read").
-define(UPDATE, "{ag_cluster}update").
-define(DELETE_BY_KEY, "{ag_cluster}delete_by_key").
-define(DELETE_BY_INDEX, "{ag_cluster}delete_by_index").

-spec init() ->
    supervisor:startchild_ret().
init() ->
    ag_cluster_sup:start_child(ag_cluster_redis_worker).

%% API
-spec add_table(Table :: atom(), Attributes :: [any()], Indices :: list(), TTL :: non_neg_integer()) ->
    ok.
add_table(Table, Attributes, Indices, TTL) ->
    ag_cluster_redis_worker:add_table(Table, Attributes, Indices, TTL).

%%把对象写入数据库
-spec write(Object :: tuple()) ->
    ok | {failed, Reason :: term()}.
write(Object) when is_tuple(Object), tuple_size(Object) > 2 ->
    try
        TableName = element(1, Object),
        LuaArgs = init_lua_args(TableName, 2),
        eval_redis_lua(?WRITE, Object, LuaArgs)
    catch
        T:R:S ->
            ?LOG_ERROR("ag_cluster_redis_adapter write obj:~p error ~p~n", [Object, {T, R, S}]),
            {failed, R}
    end;
write(_) ->
    {failed, bad_type}.

-spec write_single(Object :: tuple(), CheckFun :: tuple()) ->
    ok | {failed, Reason :: term()}.
write_single(Object, {CheckKey, Operator, CheckValue}) ->
    try
        TableName = element(1, Object),
        LuaArgs = init_lua_args(TableName, 2),
        LuaArgsT = #{<<"checkKey">> => CheckKey, <<"operator">> => Operator, <<"checkValue">> => CheckValue},
        LuaArgs1 = maps:merge(LuaArgs, LuaArgsT),
        eval_redis_lua(?VERIFICATION_WRITE, Object, LuaArgs1)
    catch
        T:R:S ->
            ?LOG_ERROR("ag_cluster_redis_adapter write obj:~p error ~p~n", [Object, {T, R, S}]),
            {failed, R}
    end.

-spec write_dirty(Object :: tuple()) ->
    ok.
write_dirty(Object) ->
    write(Object).

-spec read(atom()) ->
    {ok, [tuple()]} | {failed, Reason :: term()}.
read(TableName) ->
    try
        read_redis(TableName)
    catch
        T:R:S ->
            ?LOG_ERROR("ag_cluster_redis_adapter read table:~s error ~p~n", [TableName, {T, R, S}]),
            {failed, R}
    end.

-spec read(atom(), Key :: term()) ->
    {ok, [tuple()]} | {failed, Reason :: term()}.
read(TableName, Key) ->
    try
        read_redis(TableName, Key)
    catch
        T:R:S ->
            ?LOG_ERROR("ag_cluster_redis_adapter read table:~s key:~s error ~p~n", [TableName, Key, {T, R, S}]),
            {failed, R}
    end.

-spec read_dirty(atom(), Key :: term()) ->
    {ok, []} | {ok, tuple()}.
read_dirty(TableName, Key) ->
    read(TableName, Key).

-spec index_read(atom(), SecondaryKey :: term(), Pos :: non_neg_integer()) ->
    {ok, []} | {ok, tuple()} | {failed, Reason :: term()}.
index_read(TableName, SecondaryKey, Pos) ->
    try
        read_redis_by_index(TableName, SecondaryKey, Pos)
    catch
        T:R:S ->
            ?LOG_DEBUG("ag_cluster_redis_adapter index_read  error:~p~n", [{TableName, SecondaryKey, Pos, S, T}]),
            {failed, R}
    end.

-spec delete(Table :: atom(), Key :: term()) ->
    ok | {failed, Reason :: term()}.
delete(TableName, Key) ->
    try
        delete_redis(TableName, Key)
    catch
        T:R:S ->
            ?LOG_DEBUG("ag_cluster_redis_adapter delete  error:~p~n", [{TableName, Key, R, S, T}]),
            {failed, R}
    end.

-spec delete_dirty(Tab :: atom(), Key :: term()) ->
    'ok'.
delete_dirty(Table, Key) ->
    delete_redis(Table, Key).

-spec delete_object(Object :: tuple()) ->
    ok | {failed, Reason :: term()}.
delete_object(Object) ->
    try
        delete_redis(Object)
    catch
        T:R:S ->
            ?LOG_ERROR("ag_cluster_redis_adapter delete_object error ~p~n", [{Object, R, S, T}]),
            {failed, R}
    end.

-spec delete_index(atom(), SecondaryKey :: term(), Pos :: non_neg_integer()) ->
    ok | {failed, Reason :: term()}.
delete_index(Table, SecondaryKey, Pos) ->
    try
        delete_redis_by_index(Table, SecondaryKey, Pos)
    catch
        T:R:S ->
            ?LOG_DEBUG("ag_cluster_redis_adapter delete_index error:~p~n", [{Table, SecondaryKey, Pos, R, S, T}]),
            {failed, R}
    end.

-spec update(atom(), TableKey :: term(), UpdateList :: [tuple()]) ->
    ok | {failed, Reason :: term()}.
update(Table, TableKey, UpdateList) ->
    try
        update_redis(Table, TableKey, UpdateList)
    catch
        T:R:S ->
            ?LOG_DEBUG("ag_cluster_redis_adapter update error:~p~n", [{Table, TableKey, UpdateList, R, S, T}]),
            {failed, R}
    end.

-spec update_dirty(Table :: atom(), TableKey :: term(), UpdateList :: [tuple()]) ->
    ok | error.
update_dirty(Table, TableKey, UpdateList) ->
    try
        update_redis(Table, TableKey, UpdateList)
    catch
        T:R:S ->
            ?LOG_DEBUG("ag_cluster_redis_adapter udpate_dirty error:~p~n", [{Table, TableKey, UpdateList, R, S, T}]),
            error
    end.

-spec update_counter_dirty(Table :: atom(), Key :: term(), Incr :: integer()) ->
    NewVal :: integer().
update_counter_dirty(Table, Key, Incr) ->
    update_counter_redis(Table, Key, Incr).

-spec(check_table(Table :: [atom()]) ->
    boolean()).
check_table(Table) ->
    ag_cluster_redis_worker:check_table(Table).

-spec size(atom()) ->
    non_neg_integer().
size(Table) ->
    try
        size_redis(Table)
    catch
        T:R:S ->
            ?LOG_DEBUG("ag_cluster_redis_adapter get size error:~p~n", [{R, S, T}]),
            0
    end.

%%----------------------------------------------------------------------------------------------------------------------
%%----------------------------------------------------------------------------------------------------------------------
build_rediskey(TableName, Key) ->
    TableInfo = ag_cluster_redis_worker:get_table(TableName),
    [KeyField | _] = TableInfo#table_config.attribute,
    TableNameB = agb_convertor:to_binary(TableName),
    KeyFieldB = agb_convertor:to_binary(KeyField),
    KeyB = agb_convertor:to_binary(Key),
    <<"{ag_cluster}", TableNameB/binary, ":", KeyFieldB/binary, ":", KeyB/binary>>.

record_to_kvlist(Object) ->
    ObjList = tuple_to_list(Object),
    [TableName, Key | SubValue] = ObjList,
    TableInfo = ag_cluster_redis_worker:get_table(TableName),
    SubKeyList = TableInfo#table_config.attribute,
    RedisValue = lists:flatten(lists:zipwith(
        fun(X, Y) ->
            TY = obj_to_binary(Y),
            [X, TY]
        end,
        SubKeyList, [Key] ++ SubValue
    )),
    RedisValue.

obj_to_binary(Obj) ->
    if
        is_integer(Obj) ->
            Obj;
        true ->
            term_to_binary(Obj)
    end.

binary_to_obj(Bin) ->
    try
        binary_to_term(Bin)
    catch error:badarg ->
        try
            binary_to_integer(Bin)
        catch error:badarg ->
            error
        end
    end.

get_keyfield_bypos(TableName, Pos) ->
    TableInfo = ag_cluster_redis_worker:get_table(TableName),
    Attribute = TableInfo#table_config.attribute,
    lists:nth(Pos - 1, Attribute).

redisObj_to_record(TableName, BinList) ->
    RecordList =
        [TableName] ++ lists:map(
        fun(Bin) ->
            binary_to_obj(Bin)
        end,
        BinList
    ),
    list_to_tuple(RecordList).

delete_redis(TableName, Key) ->
    Args = init_lua_args(TableName, 2),
    eval_redis_lua(?DELETE_BY_KEY, Key, Args).

delete_redis(Object) ->
    TableName = erlang:element(1, Object),
    Key = erlang:element(2, Object),
    delete_redis(TableName, Key).

delete_redis_by_index(TableName, SecondaryKey, KeyFieldPos) ->
    Args = init_lua_args(TableName, KeyFieldPos),
    eval_redis_lua(?DELETE_BY_INDEX, SecondaryKey, Args).

read_redis(TableName) ->
    TableInfo = ag_cluster_redis_worker:get_table(TableName),
    [KeyField | _] = TableInfo#table_config.attribute,
    TableNameStr = agb_string:to_string(TableName),
    KeyFieldStr = agb_string:to_string(KeyField),
    KeyPattern = agb_string:sprintf("{ag_cluster}~s:~s:*", [TableNameStr, KeyFieldStr]),
    case read_redis_keys(KeyPattern) of
        [] ->
            [];
        Keys ->
            P = [["hvals", K] || K <- Keys],
            case agdb_cached_adapter:qp(?REDIS_POOL, P) of
                {error, Reason} ->
                    agb_error:error("read_redis error resaon:~p~n", [Reason]);
                Result ->
                    {ok, lists:foldl(
                        fun
                            ({ok, R}, Acc) ->
                                case R of
                                    [] ->
                                        Acc;
                                    _ ->
                                        Acc ++ [redisObj_to_record(TableName, R)]
                                end;
                            ({error, _}, Acc) ->
                                Acc
                        end,
                        [],
                        Result
                    )}
            end
    end.

read_redis_keys(KeyPattern) ->
    case agdb_cached_adapter:scan(?REDIS_POOL, 0, KeyPattern, 200) of
        {error, _} ->
            [];
        {ok, [Cursor, Keys]} ->
            read_redis_keys(Cursor, KeyPattern, Keys)
    end.

read_redis_keys(<<"0">>, _, Keys) ->
    Keys;
read_redis_keys(Cursor, KeyPattern, Keys) ->
    case agdb_cached_adapter:scan(?REDIS_POOL, Cursor, KeyPattern, 200) of
        {error, _} ->
            [];
        {ok, [NewCursor, NewKeys]} ->
            read_redis_keys(NewCursor, KeyPattern, Keys ++ NewKeys)
    end.

read_redis(TableName, Key) ->
    Args = init_lua_args(TableName, 2),
    eval_redis_lua(?READ, TableName, Key, Args).

read_redis_by_index(TableName, SecondaryKey, Pos) ->
    read_redis_by_index(TableName, SecondaryKey, Pos, 0, 1000).

read_redis_by_index(TableName, SecondaryKey, Pos, BeginCursor, Count) ->
    Args = maps:merge(init_lua_args(TableName, Pos), #{<<"beginCursor">> => BeginCursor, <<"count">> => Count}),
    case eval_redis_lua(?INDEX_READ, TableName, SecondaryKey, Args) of
        {ok, {Objects, 0}} ->
            {ok, Objects};
        {ok, {Objects, Cursor}} ->
            {ok, Objects ++ read_redis_by_index(TableName, SecondaryKey, Pos, Cursor, Count)};
        {failed, _} ->
            {ok, []}
    end.

%%获取单个表的长度，有多少这个表的kv对象
size_redis(TableName) ->
    TableInfo = ag_cluster_redis_worker:get_table(TableName),
    [KeyField | _] = TableInfo#table_config.attribute,
    Key = agb_string:sprintf("{ag_cluster}~s:~s:*", [agb_string:to_string(TableName), agb_string:to_string(KeyField)]),
    {ok, Keys} = agdb_cached_adapter:keys(?REDIS_POOL, agb_convertor:to_binary(Key)),
    erlang:length(Keys).

update_redis(TableName, TableKey, UpdateList) ->
    Args = init_lua_args(TableName, 2),
    UpdateList1 =
        lists:flatmap(
            fun({A, B}) ->
                [get_keyfield_bypos(TableName, A), obj_to_binary(B)]
            end,
            UpdateList
        ),
    ?LOG_DEBUG("update_redis UpdateList:~p~n", [UpdateList1]),
    eval_redis_lua(?UPDATE, UpdateList1, TableKey, Args).

%  修改自增数据 这是兼容mnesia自增id特殊写的，attribue只有2个属性，第二个就是自增id
update_counter_redis(TableName, Key, Incr) ->
    TableInfo = ag_cluster_redis_worker:get_table(TableName),
    RedisKey = build_rediskey(TableName, Key),
    [KeyField | _] = lists:reverse(TableInfo#table_config.attribute),
    {ok, Num} = agdb_cached_adapter:hincrby(?REDIS_POOL, RedisKey, agb_convertor:to_binary(KeyField), Incr),
    Num.

init_lua_args(TableName, KeyFieldPos) ->
    io:format("init_lua_args ~p ~p~n",[TableName, KeyFieldPos]),
    TableInfo = ag_cluster_redis_worker:get_table(TableName),
    io:format("====>ag_cluster_redis_worker:get_table ~p -> ~p~n",[TableName, TableInfo]),
    KeyField = get_keyfield_bypos(TableName, KeyFieldPos),
    io:format("====>ag_cluster_redis_worker:get_table ~p -> ~p~n",[TableName, TableInfo]),
    #{
        <<"objectName">> => agb_convertor:to_binary(TableName),
        <<"keyField">> => agb_convertor:to_binary(KeyField),
        <<"keyPos">> => 2,
%%        <<"nodeKey">> => get_node_set_key(node()),
        <<"ttl">> => TableInfo#table_config.ttl,
        <<"indexs">> => [agb_convertor:to_binary(Index) || Index <- TableInfo#table_config.index]
    }.

eval_redis_lua(CMD, Object, Args) when CMD == ?WRITE; CMD == ?VERIFICATION_WRITE ->
    Sha = ag_cluster_variable:getv(<<"cluster_reids.lua">>),
    Key = erlang:element(2, Object),
    Json = agb_json:encode(Args),
    Command = ["EVALSHA", agb_convertor:to_string(Sha), 2, CMD, ?ADD_SLOT_KEY(Key), Json | record_to_kvlist(Object)],
    case eval_sha_lua(<<"cluster_reids.lua">>, Command) of
        {ok, <<"ok">>} ->
            ok;
        {ok, Res} ->
            {failed, Res};
        {error, Info} ->
            ?LOG_ERROR("eval_redis_lua error: ~p~n", [Info]),
            {failed, <<"eval redis lua error">>}
    end;
eval_redis_lua(?DELETE_BY_KEY, Key, Args) ->
    Sha = ag_cluster_variable:getv(<<"cluster_reids.lua">>),
    Json = agb_json:encode(Args),
    Command = ["EVALSHA", agb_convertor:to_string(Sha), 2, ?DELETE_BY_KEY, ?ADD_SLOT_KEY(Key), Json],
    case eval_sha_lua(<<"cluster_reids.lua">>, Command) of
        {ok, <<"ok">>} ->
            ok;
        {error, Info} ->
            ?LOG_ERROR("eval_redis_lua error: ~p~n", [Info]),
            {failed, <<"eval redis lua error">>}
    end;
eval_redis_lua(?DELETE_BY_INDEX, Key, Args) ->
    Sha = ag_cluster_variable:getv(<<"cluster_reids.lua">>),
    Json = agb_json:encode(Args),
    Command = ["EVALSHA", agb_convertor:to_string(Sha), 2, ?DELETE_BY_INDEX, ?ADD_SLOT_KEY(obj_to_binary(Key)), Json],
    case eval_sha_lua(<<"cluster_reids.lua">>, Command) of
        {ok, <<"ok">>} ->
            ok;
        {error, Info} ->
            ?LOG_ERROR("eval_redis_lua error: ~p~n", [Info]),
            {failed, <<"eval redis lua error">>}
    end.

eval_redis_lua(?UPDATE, Update, Key, Args) ->
    Sha = ag_cluster_variable:getv(<<"cluster_reids.lua">>),
    io:format("~p~n",[Sha]),
    Json = agb_json:encode(Args),
    Command = ["EVALSHA", agb_convertor:to_string(Sha), 2, ?UPDATE, ?ADD_SLOT_KEY(Key), Json | Update],
    case eval_sha_lua(<<"cluster_reids.lua">>, Command) of
        {ok, <<"ok">>} ->
            ok;
        {ok, <<"read table failed when update">>} ->
            {failed, <<"read table failed when update">>};
        {error, Info} ->
            ?LOG_ERROR("eval_redis_lua error: ~p~n", [Info]),
            {failed, <<"eval redis lua error">>}
    end;
eval_redis_lua(?READ, TableName, Key, Args) ->
    Sha = ag_cluster_variable:getv(<<"cluster_reids.lua">>),
    Json = agb_json:encode(Args),
    case eval_sha_lua(<<"cluster_reids.lua">>, ["EVALSHA", agb_convertor:to_string(Sha), 2, ?READ, ?ADD_SLOT_KEY(Key), Json]) of
        {ok, []} ->
            {ok, []};
        {ok, R} ->
            {ok, [redisObj_to_record(TableName, get_value_list(TableName, R))]};
        {error, Info} ->
            ?LOG_ERROR("eval_redis_lua error: ~p~n", [Info]),
            {failed, <<"eval redis lua error">>}
    end;
eval_redis_lua(?INDEX_READ, TableName, Key, Args) ->
    Sha = ag_cluster_variable:getv(<<"cluster_reids.lua">>),
    Json = agb_json:encode(Args),
    Command = ["EVALSHA", agb_convertor:to_string(Sha), 2, ?INDEX_READ, ?ADD_SLOT_KEY(obj_to_binary(Key)), Json],
    case eval_sha_lua(<<"cluster_reids.lua">>, Command) of
        {ok, [_, Cursor]} when Cursor == 0 ->
            {ok, {[], 0}};
        {ok, [ReadList, Cursor]} ->
            CursorVal = agb_convertor:to_integer(Cursor),
            {ok, {[redisObj_to_record(TableName, get_value_list(TableName, Info)) || Info <- ReadList], CursorVal}};
        {error, Info} ->
            ?LOG_ERROR("eval_redis_lua error: ~p~n", [Info]),
            {failed, <<"eval redis lua error">>};
        _ ->
            ?LOG_DEBUG("eval_redis_lua index_read:~p~n", [{[], 0}]),
            {ok, {[], 0}}
    end.

eval_sha_lua(Lua, Command) ->
    case agdb_cached_adapter:q(?REDIS_POOL, Command) of
        {error, <<"NOSCRIPT", _/binary>>} ->
            ag_cluster_redis_worker:reload_lua_file(),
            {_, Sha} = ag_cluster_variable:get(Lua),
            [_, _ | T] = Command,
            NewCommand = ["EVALSHA", Sha] ++ T,
            agdb_cached_adapter:q(?REDIS_POOL, NewCommand);
        Result ->
            Result
    end.

get_value_list(TableName, KVList) ->
    {_, KList, VList} =
        lists:foldl(
            fun(V, {Index, KList, VList}) ->
                case Index rem 2 of
                    1 ->
                        {Index + 1, KList ++ [V], VList};
                    0 ->
                        {Index + 1, KList, VList ++ [V]}
                end
            end,
            {1, [], []},
            KVList
        ),
    Map = maps:from_list(lists:zip(KList, VList)),
    TableInfo = ag_cluster_redis_worker:get_table(TableName),
    SubKeyList = TableInfo#table_config.attribute,
    lists:map(
        fun(Key) ->
            maps:get(agb_convertor:to_binary(Key), Map)
        end,
        SubKeyList
    ).
