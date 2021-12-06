%%%-------------------------------------------------------------------
%%% @author yuanhefu
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 六月 2019 15:57
%%%-------------------------------------------------------------------
-module(ag_cluster_SUITE).

%% common_test suite for mymodule
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([suite/0, groups/0, all/0, init_per_suite/1, end_per_suite/1, end_per_group/2, init_per_group/2]).

-export([test_cluster/1, test_cluster_online_player/1]).
-export([ag_cluster_config/1]).


-define(ONLINE_PLAYER_INDEX, [session, agent_pid, agent_node, archive]).

%% ···································································
%%    以下内容自 "ag_engine/include/ag_engine.hrl" 复制而来

-record(online_player, {
    id :: term(),
    idtype :: term(),
    agent_pid :: pid(),
    agent_node :: node(),
    gate_pid :: pid(),
    gate_node :: node(),
    archive :: string(),
    session :: string(),
    security :: string(),
    login_time :: integer()
}).

-record(gate_node, {
    node :: node(),
    gate_count :: integer()
}).

-record(agent_node, {
    node :: node(),
    agent_count :: integer()
}).

-define(AUTH_SYSTEM_TYPE, seconds).

-define(ENGINE_CACHE_POOL, engine_cache_pool).
%%    以上内容自 "ag_engine/include/ag_engine.hrl" 复制而来
%% ···································································

suite() -> [{timetrap, {seconds, 20}}].

groups() ->
    [
        {redis, [sequence], [test_cluster, test_cluster_online_player]},
        {mongodb, [sequence], [test_cluster, test_cluster_online_player]},
        {memdb, [sequence], [test_cluster, test_cluster_online_player]}
    ].

all() ->
    [
        {group, redis},
        {group, mongodb},
        {group, memdb}
    ].

init_per_suite(Config) ->
    ct:pal("SNode :~p~n", [node()]),

    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(redis, Config) ->
    application:set_env(ag_cluster, adapter, ag_cluster_redis_adapter),
    agb_application:check_started(ag_cluster),

    ag_cluster_manager:add_table(gate_node, record_info(fields, gate_node), [], 0),
    ag_cluster_manager:add_table(online_player, record_info(fields, online_player), ?ONLINE_PLAYER_INDEX, 300),
    Config ++ [{type, redis}];
init_per_group(mongodb, Config) ->
    application:set_env(ag_cluster, adapter, ag_cluster_mongodb_adapter),
    agb_application:check_started(ag_cluster),

    ag_cluster_manager:add_table(gate_node, record_info(fields, gate_node), [], 0),
    ag_cluster_manager:add_table(online_player, record_info(fields, online_player), ?ONLINE_PLAYER_INDEX, 300),
    Config ++ [{type, mongodb}];

init_per_group(memdb, Config) ->
    application:set_env(ag_cluster, adapter, ag_cluster_memdb_adapter),
    agb_application:check_started(ag_cluster),

    ag_cluster_manager:add_table(gate_node, record_info(fields, gate_node), [], 0),
    ag_cluster_manager:add_table(online_player, record_info(fields, online_player), ?ONLINE_PLAYER_INDEX, 300),
    %%等待mnesia初始化比较慢
    timer:sleep(3000),
    Config ++ [{type, memdb}].

end_per_group(redis, _Config) ->
    agdb_cached_adapter_redis:flushdb(cluster_redis_pool),
    application:stop(ag_cluster),
    ok;
end_per_group(mongodb, _Config) ->
    application:stop(ag_cluster),
    ok;
end_per_group(_TestCase, _Config) ->
    application:stop(ag_cluster),
    ok.

ag_cluster_config(_Config) ->
    ?assertEqual(undefined, ag_cluster_config:get(test)),
    ?assertEqual(undefined, ag_cluster_config:getv(test)),
    ok.

test_cluster(_Config) ->
    Node = 'ag_cluster@127.0.0.1',
    %%检查table是否加载成功
    ?assertEqual(true, ag_cluster_manager:check_table([gate_node])),
    ?assertEqual(false, ag_cluster_manager:check_table([gate_node_test])),

    %增加
    ?assertEqual(ok, ag_cluster_manager:write(#gate_node{node = Node, gate_count = 0})),

    %数量
    ?assertEqual(true, ag_cluster_manager:size(gate_node) > 0),

    %%查询
    ?assertEqual({ok, [{gate_node, 'ag_cluster@127.0.0.1', 0}]}, ag_cluster_manager:read(gate_node, Node)),

    ?assertEqual({ok, []}, ag_cluster_manager:read(gate_node, <<"nonode@nohost">>)),
    ?assertEqual({ok, []}, ag_cluster_manager:read_dirty(gate_node, <<"nonode@nohost">>)),

    %%修改更新
    ag_cluster_manager:update_counter_dirty(gate_node, Node, 1),

    %%修改完查询
    ct:pal("==============~p~n", [ag_cluster_manager:read(gate_node)]),
    ?assertEqual({ok, [{gate_node, 'ag_cluster@127.0.0.1', 1}]}, ag_cluster_manager:read(gate_node)),

    %%删除
    ?assertEqual(ok, ag_cluster_manager:delete(gate_node, Node)),
    ?assertEqual(ok, ag_cluster_manager:delete_dirty(gate_node, Node)),
    ok.

test_write_single(redis, Obj) ->
    ?assertEqual(ok, ag_cluster_manager:write_single(Obj, {<<"login_time">>, <<"<=">>, agb_time:get_utc_time_seconds() + 1}));
test_write_single(mongodb, Obj) ->
    ?assertEqual(ok, ag_cluster_manager:write_single(Obj, {<<"login_time">>, <<"<=">>, agb_time:get_utc_time_seconds() + 1}));
test_write_single(memdb, Obj) ->
    ?assertEqual({failed, <<"lockerror">>}, ag_cluster_manager:write_single(Obj, fun(#online_player{login_time = Time}) ->
        agb_time:get_utc_time_seconds() - Time > 5000 end)),
    ?assertEqual(ok, ag_cluster_manager:write_single(Obj, fun(#online_player{login_time = Time}) ->
        agb_time:get_utc_time_seconds() - Time >= 0 end)).

%%online_player测试
test_cluster_online_player(Config) ->
    logger:set_primary_config(level, all),
    Node = 'ag_cluster@127.0.0.1',
    Obj = #online_player{id = "AmazeGame", idtype = "facebook", agent_pid = self(), agent_node = Node, gate_pid = self(), gate_node = Node,
        archive = "12345", session = "abcdef", security = 111111, login_time = agb_time:get_utc_time_seconds()},

    Obj1 = #online_player{id = "AmazeGame1", idtype = "facebook", agent_pid = self(), agent_node = Node, gate_pid = self(), gate_node = Node,
        archive = "1234", session = "abcdef11", security = 111111, login_time = agb_time:get_utc_time_seconds()},

    %%插入
    ?assertEqual(ok, ag_cluster_manager:write(Obj)),
    ?assertEqual(ok, ag_cluster_manager:write_dirty(Obj1)),
    %?assertEqual({failed, <<"lockerror">>}, ag_cluster_manager:write_single(Obj1, {<<"login_time">>, <<"<=">>, agb_time:get_utc_time_seconds() - 5000})),
    %?assertEqual(ok, ag_cluster_manager:write_single(Obj1, {<<"login_time">>, <<"<=">>, agb_time:get_utc_time_seconds() + 1})),
    Type = proplists:get_value(type, Config),
    test_write_single(Type, Obj1),
    %%添加index
    %?assertEqual(ok, ag_cluster_manager:add_index_to_node(Obj)),
    %%查询
    ?assertEqual({ok, [Obj]}, ag_cluster_manager:index_read(online_player, "12345", #online_player.archive)),
    ct:pal("~n+++++++++++++++++++++>Obj : ~p~n", [Obj]),

    %%更新
    ?assertEqual(ok, ag_cluster_manager:update(online_player, "AmazeGame", [{#online_player.security, 44444}, {#online_player.idtype, "google"}])),
    ?assertEqual(ok, ag_cluster_manager:update_dirty(online_player, "AmazeGame", [{#online_player.security, 111111}, {#online_player.idtype, "facebook"}])),
    ct:pal("~n===================>~p~n", [ag_cluster_manager:update_dirty(online_player, "AmazeGame", [{#online_player.archive, "12345"}, {#online_player.idtype, "facebook"}])]),
    ?assertEqual(ok, ag_cluster_manager:update_dirty(online_player, "AmazeGame", [{#online_player.session, "123456"}, {#online_player.idtype, "facebook"}])),


    UpdateResult = ag_cluster_manager:update(online_player, "AmazeGame_None", [{#online_player.security, 111111}, {#online_player.idtype, "facebook"}]),
    UpdateDirtyResult = ag_cluster_manager:update_dirty(online_player, "AmazeGame_None", [{#online_player.security, 111111}, {#online_player.idtype, "facebook"}]),
    %%redis 更新时  没有key自动插入
    case application:get_env(ag_cluster, adapter) of
        {ok, ag_cluster_memdb_adapter} ->
            ?assertEqual({failed, "read table failed when update"}, UpdateResult),
            ?assertEqual(error, UpdateDirtyResult);
        {ok, ag_cluster_redis_adapter} ->
            ?assertEqual({failed, <<"read table failed when update">>}, UpdateResult),
            ?assertEqual({failed, <<"read table failed when update">>}, UpdateDirtyResult);
        {ok, ag_cluster_mongodb_adapter} ->
            ?assertEqual(ok, UpdateResult),
            ?assertEqual(ok, UpdateDirtyResult)
    end,
    %%删除
    ?assertEqual(ok, ag_cluster_manager:delete_object(Obj)),
    ?assertEqual(ok, ag_cluster_manager:delete_index(online_player, "1234", #online_player.archive)),
    ?assertEqual(ok, ag_cluster_manager:write(Obj)),
    ?assertEqual(ok, ag_cluster_manager:write_dirty(Obj1)),
    ok.

