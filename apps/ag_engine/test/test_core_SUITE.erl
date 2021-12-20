%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(test_core_SUITE).


%% API
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").
-include_lib("ag_engine/include/ag_engine_code_defines.hrl").
-export([
    all/0,
    groups/0,
    init_per_group/2,
    init_per_suite/1,
    init_per_testcase/2,
    end_per_group/2,
    end_per_suite/1,
    end_per_testcase/2,
    test_core_forbid_login_roleworker/1,
    test_core_replace_login_roleworker/1,
    test_core_start_roleworker/1,
    test_core_multi_login_roleworker/1,
    test_core_instantaneous_login/1
]).

all() ->
    [{group, use_worker},  {group, no_worker}].

groups() ->
    Run = [
        test_core_start_roleworker,
        test_core_replace_login_roleworker,
        test_core_forbid_login_roleworker,
        test_core_multi_login_roleworker
    ],
    [
        {use_worker, [], Run},
        {no_worker, [], Run}
    ].

init_per_suite(Config) ->
    test_utils:init_suite(Config).

end_per_suite(_Config) ->
    ok.

init_per_group(use_worker, Config) ->
    application:set_env(ag_engine, is_use_roleworker, true),
    agb_application:check_started(ag_engine),
    Config;
init_per_group(no_worker, Config) ->
    application:set_env(ag_engine, is_use_roleworker, false),
    agb_application:check_started(ag_engine),
    Config.

end_per_group(use_worker, _Config) ->
    agdb_cached_adapter_redis:flushdb(cluster_redis_pool),
    application:stop(ag_engine_cluster),
    application:stop(ag_engine),
    ok;
end_per_group(no_worker, _Config) ->
    agdb_cached_adapter_redis:flushdb(cluster_redis_pool),
    application:stop(ag_engine_cluster),
    application:stop(ag_engine),
    ok.

init_per_testcase(Case, Config) ->
    ct:pal("=== ~p begin ===", [Case]),
    process_flag(trap_exit, true),
    Config.

end_per_testcase(Case, _Config) ->
    ct:pal("=== ~p end ===", [Case]),
    ok.

test_core_start_roleworker(_Config) ->
    {true, #{?INTERNAL_DEFINE_SESSION := Session1}} = ag_engine_core:register_withoutid(#{}),
    {true, #{?INTERNAL_DEFINE_SESSION := Session2}} = ag_engine_core:register_withid(#{?INTERNAL_DEFINE_ID => <<"Id0">>, ?INTERNAL_DEFINE_ID_TYPE=><<"fb">>}),
    ag_engine_core:register_withid(#{?INTERNAL_DEFINE_ID => <<"Id1">>, ?INTERNAL_DEFINE_ID_TYPE=><<"fb">>}),
    ag_engine_core:login(#{?INTERNAL_DEFINE_ID => <<"Id2">>}),
    ag_engine_core:login(#{?INTERNAL_DEFINE_ID => <<"Id3">>, ?INTERNAL_DEFINE_ID_TYPE=><<"fb">>}),
    ag_engine_core:login(#{?INTERNAL_DEFINE_ID => <<"Id4">>, ?INTERNAL_DEFINE_ID_TYPE=><<"fb">>}),
    ag_engine_core:enter_with_security(#{?INTERNAL_DEFINE_SESSION => Session1}),
    ag_engine_core:enter_with_temporary(#{?INTERNAL_DEFINE_SESSION => Session2}),
    a.

test_core_replace_login_roleworker(_Config) ->
    meck:new(ag_engine_login_event, [unstick, passthrough]),
    meck:expect(ag_engine_login_event, on_login_online_player, 1, {kick_online, #{msg=><<"OtherLogin">>}}),
    Pid1 = spawn(fun() -> gate_process(#{?INTERNAL_DEFINE_ID => <<"Id5">>, ?INTERNAL_DEFINE_ID_TYPE=><<"fb">>}) end),
    timer:sleep(1000),
    Pid2 = spawn(fun() -> gate_process(#{?INTERNAL_DEFINE_ID => <<"Id5">>, ?INTERNAL_DEFINE_ID_TYPE=><<"fb">>}) end),
    timer:sleep(1000),
    ?assertEqual(false, is_process_alive(Pid1)),
    ?assertEqual(true, is_process_alive(Pid2)),
    meck_proc:stop(ag_engine_login_event),
    ok.

test_core_instantaneous_login(_Config) ->
    meck:new(test_archive, [unstick, passthrough]),
    meck:expect(test_archive, on_repetitive_login, 1, {<<"killonlineplayer">>, #{msg=><<"OtherLogin">>}}),
    Pid1 = spawn(fun() -> gate_process(#{?INTERNAL_DEFINE_ID => <<"Id8">>, ?INTERNAL_DEFINE_ID_TYPE=><<"fb">>}) end),
    Pid2 = spawn(fun() -> gate_process(#{?INTERNAL_DEFINE_ID => <<"Id8">>, ?INTERNAL_DEFINE_ID_TYPE=><<"fb">>}) end),
    Pid3 = spawn(fun() -> gate_process(#{?INTERNAL_DEFINE_ID => <<"Id8">>, ?INTERNAL_DEFINE_ID_TYPE=><<"fb">>}) end),
    timer:sleep(1000),
    ?assertEqual(true, is_process_alive(Pid1)),
    ?assertEqual(true, is_process_alive(Pid2)),
    ?assertEqual(true, is_process_alive(Pid3)),
    meck_proc:stop(test_archive),
    ok.

test_core_forbid_login_roleworker(_Config) ->
    meck:new(ag_engine_login_event, [unstick, passthrough]),
    meck:expect(ag_engine_login_event, on_login_online_player, 1, {forbid_login, #{msg=><<"IsOnline">>}}),
    Pid1 = spawn(fun() -> gate_process(#{?INTERNAL_DEFINE_ID => <<"Id6">>, ?INTERNAL_DEFINE_ID_TYPE=><<"fb">>}) end),
    timer:sleep(1000),
    Pid2 = spawn(fun() -> gate_process(#{?INTERNAL_DEFINE_ID => <<"Id6">>, ?INTERNAL_DEFINE_ID_TYPE=><<"fb">>}) end),
    timer:sleep(1000),
    ?assertEqual(true, is_process_alive(Pid1)),
    ?assertEqual(false, is_process_alive(Pid2)),
    meck_proc:stop(ag_engine_login_event),
    ok.

test_core_multi_login_roleworker(_Config) ->
    meck:new(ag_engine_login_event, [unstick, passthrough]),
    meck:expect(ag_engine_login_event, on_login_online_player, 1, {multi_login, #{}}),
    Pid1 = spawn(fun() -> gate_process(#{?INTERNAL_DEFINE_ID => <<"Id7">>, ?INTERNAL_DEFINE_ID_TYPE=><<"fb">>}) end),
    timer:sleep(1000),
    Pid2 = spawn(fun() -> gate_process(#{?INTERNAL_DEFINE_ID => <<"Id7">>, ?INTERNAL_DEFINE_ID_TYPE=><<"fb">>}) end),
    timer:sleep(1000),
    ?assertEqual(true, is_process_alive(Pid1)),
    ?assertEqual(true, is_process_alive(Pid2)),
    meck_proc:stop(ag_engine_login_event),
    ok.

gate_process(Context) ->
    {Result, _} = ag_engine_core:login(Context),
    case Result of
        true ->
            gate_msg_loop();
        false ->
            stop
    end.

gate_msg_loop() ->
    receive
        {kick_user, kick_user_other_login, Msg} ->
            ct:pal("pid:~p receive kickuser:~p", [self(), Msg]),
            self() ! kick_stop,
            gate_msg_loop();
        kick_user ->
            ct:pal("pid:~p receive kickuser", [self()]),
            self() ! kick_stop,
            gate_msg_loop();
        {'EXIT', Pid, _} ->
            ct:pal("roleworker pid:~p stop", [Pid]);
        kick_stop ->
            stop
    after 1000 ->
        gate_msg_loop()
    end.

