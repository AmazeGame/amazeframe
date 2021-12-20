%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(test_cluster_SUITE).


%% API
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    test_cluster/1
]).

all()->
    [test_cluster].

init_per_suite(Config) ->
    logger:set_primary_config(level, all),
    test_utils:load_config(Config),
    application:ensure_all_started(ts_database),
    timer:sleep(5000),
    application:ensure_all_started(ag_engine_cluster),
    application:ensure_all_started(ag_idcreator),
    Config.

end_per_suite(_Config)->
    agdb_cached_adapter_redis:flushdb(cluster_redis_pool),
    application:stop(ag_idcreator),
    application:stop(ag_engine_cluster),
    application:stop(ts_database),
    ok.

test_cluster(_Config)->
    ag_engine_cluster:init(),
    ?assertEqual(true,          ag_engine_cluster:check_cluster()),
    ag_engine_cluster:register_as_gate_manager(node()),
    ag_engine_cluster:register_as_playeragent_manager(node()),
    ag_engine_cluster:increase_gate(node()),
    ag_engine_cluster:decrease_gate(node()),
    ag_engine_cluster:gate_offline(),
    ag_engine_cluster:register_player(<<"id">>,<<"fb">>,self(),node(),self(),node(),<<"session">>,<<"security">>,<<"archive">>),
    ?assertEqual(undefined,                         ag_engine_cluster:player_from_id(undefined)),
    ?assertNotEqual(undefined,                      ag_engine_cluster:player_from_id(<<"id">>)),
    ?assertEqual(undefined,                         ag_engine_cluster:player_from_session(undefined)),
    ?assertEqual(undefined,                         ag_engine_cluster:player_from_session(<<"undefined">>)),
    ?assertNotEqual(undefined,                      ag_engine_cluster:player_from_session(<<"session">>)),
    ?assertEqual(undefined,                         ag_engine_cluster:player_from_archive(undefined)),
    ?assertEqual(undefined,                         ag_engine_cluster:player_from_archive(<<"undefined">>)),
    ?assertNotEqual(undefined,                      ag_engine_cluster:player_from_archive(<<"archive">>)),
    ag_engine_cluster:update_online_player_info(<<"id">>,self(),node()),
    ag_engine_cluster:update_online_player_info(<<"id">>,self(),node(),<<"123456789">>),
    ag_engine_cluster:unregister_player(<<"id">>),
    ag_engine_cluster:unregister_player(undefined),
    ag_engine_cluster:put_temporary_security(<<"id">>,<<"TemporarySecurity">>),
    ?assertEqual(undefined,                         ag_engine_cluster:get_temporary_security(undefined)),
    ?assertNotEqual(undefined,                      ag_engine_cluster:get_temporary_security(<<"id">>)),
    a.