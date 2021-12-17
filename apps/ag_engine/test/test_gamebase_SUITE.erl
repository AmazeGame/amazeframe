%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @copyright (C) 2019, Harbour Studios
%%% @doc
%%%
%%% @end
%%% Created : 05. 七月 2019 12:07
%%%-------------------------------------------------------------------
-module(test_gamebase_SUITE).


%% API
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").
-include("meck_module/msg_field.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    test_msg_metrics_timeout/1,
    test_gate_terminate/1,
    test_async_message_handler/1,
    test_carrier_message_handler/1,
    test_sync_message_handler/1,
    test_push_message/1,
    metrics_exec_call_back/1
]).

all() ->
    [
        test_msg_metrics_timeout,
        test_gate_terminate,
        test_carrier_message_handler,
        test_async_message_handler,
        test_sync_message_handler,
        test_push_message
    ].

init_per_suite(Config) ->
    logger:set_primary_config(level, all),
%%    net_kernel:start(['rumata@127.0.0.1', longnames]),
    test_utils:load_config(Config),
    agb_application:check_started(ag_engine_cluster),
    agb_application:check_started(ag_idcreator),
    application:set_env(ag_engine, is_use_roleworker, true),
    agb_application:check_started(ag_engine),
    Config.

end_per_suite(_Config) ->
    agdb_cached_adapter_redis:flushdb(cluster_redis_pool),
    application:stop(ag_engine_cluster),
    application:stop(ag_idcreator),
    application:stop(ag_engine).

test_carrier_message_handler(_Config) ->
    meck:new(ag_idcreator, [non_strict]),
    meck:expect(ag_idcreator, gen_newid, 1, fun gen_newid/1),
    MsgList1 =
        [
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST1, <<"b">> => 12345},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST2, <<"d">> => 2},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST3, <<"d">> => 3},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST4, <<"d">> => 4},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST5, <<"d">> => 5},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST6, <<"d">> => 6}
        ],
    MsgList2 =
        [
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST1, <<"b">> => 12345},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST2, <<"d">> => 2},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST3, <<"d">> => 3},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST5, <<"d">> => 5},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST4, <<"d">> => 4},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST6, <<"d">> => 6}
        ],
    MsgList3 =
        [
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST1, <<"b">> => 12345},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST2, <<"d">> => 2},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST3, <<"d">> => 3},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST6, <<"d">> => 6},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST4, <<"d">> => 4},
            #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST5, <<"d">> => 5}
        ],
    ?assertEqual({false, [
        #{<<"Result">> => <<"ok">>, ?MESSAGE_NAME_KEY => ?MSG_GATE_TEST1_REPLY},
        #{<<"Result">> => <<"error">>, ?MESSAGE_NAME_KEY => ?MSG_GATE_TEST2_REPLY}
    ]}, ag_engine_msg_executer:msg_carrier_apply(MsgList1)),

    ?assertEqual({false, [
        #{<<"Result">> => <<"ok">>, ?MESSAGE_NAME_KEY => ?MSG_GATE_TEST1_REPLY}]},
        ag_engine_msg_executer:msg_carrier_apply(undefined, MsgList2)),

    ?assertEqual({false, [
        #{<<"Result">> => <<"ok">>, ?MESSAGE_NAME_KEY => ?MSG_GATE_TEST1_REPLY}]},
        ag_engine_msg_executer:msg_carrier_apply(undefined, MsgList3)),

    WorkPid = test_meck_worker:quick_start_work(self()),
    ct:pal("quick_start_work WorkPid:~p~n", [WorkPid]),
    ag_engine_storage:set_roleworkerpid(WorkPid),

    ct:pal("is use roleworker:~p~n", [agb_ets:get(ag_engine_worker_module, worker_module)]),
    ?assertEqual({true, [
        #{<<"Result">> => <<"ok">>, ?MESSAGE_NAME_KEY => ?MSG_GATE_TEST1_REPLY},
        #{?MESSAGE_NAME_KEY => 1},
        #{?MESSAGE_NAME_KEY => 2},
        #{?MESSAGE_NAME_KEY => 3},
        #{?MESSAGE_NAME_KEY => 4}
    ]}, ag_engine_msg_executer:msg_carrier_apply(WorkPid, [#{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST1, <<"b">> => 12345}])),
    meck:unload().

test_async_message_handler(_Config) ->
    logger:set_primary_config(level, all),
    meck:new(ag_idcreator, [non_strict]),
    meck:expect(ag_idcreator, gen_newid, 1, fun gen_newid/1),
    Msg1 = #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST1, <<"b">> => 12345},
    Msg2 = #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST2, <<"b">> => 12345},
    Msg3 = #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST3, <<"b">> => 12345},
    Msg4 = #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST4, <<"b">> => 12345},
    Msg5 = #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST5, <<"b">> => 12345},
    Msg6 = #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST6, <<"b">> => 12345},
    Msg7 = #{?MESSAGE_NAME_KEY => ?MSG_ROLEWORKER_TEST, <<"b">> => 12345},

    Msg8 = #{?MESSAGE_NAME_KEY => ?MSG_ROLEWORKER_TEST1, <<"b">> => 12345},
    Msg9 = #{?MESSAGE_NAME_KEY => ?MSG_ROLEWORKER_TEST2, <<"b">> => 12345},

    Reply1 = ag_engine_msg_executer:msg_single_apply(Msg1, true),
    Reply2 = ag_engine_msg_executer:msg_single_apply(Msg2, true),
    Reply3 = ag_engine_msg_executer:msg_single_apply(Msg3, true),
    Reply4 = ag_engine_msg_executer:msg_single_apply(Msg4, true),
    Reply5 = ag_engine_msg_executer:msg_single_apply(Msg5, true),
    Reply6 = ag_engine_msg_executer:msg_single_apply(Msg6, true),

    ct:pal("Reply1:~p", [{Reply1, Reply2, Reply3, Reply4, Reply5, Reply6}]),
    Reply7 = ag_engine_msg_executer:msg_single_apply(Msg7, true),
    ct:pal("Reply7:~p", [Reply7]),
    ?assertMatch({false, #{<<"code">> := -1, <<"name">> := <<"InError">>}}, Reply7),
    WorkPid = test_meck_worker:quick_start_work(self()),
    ct:pal("quick_start_work WorkPid:~p~n", [WorkPid]),
    ag_engine_storage:set_roleworkerpid(WorkPid),
    Fun = fun() ->
        receive
            {async_handle_result, Msg, Echo} ->
                ag_engine_msg_executer:reply(Msg, Echo)
        after 5000 ->
            ?assertEqual(true, false)
        end
          end,
    ag_engine_msg_executer:msg_single_apply(Msg7, true),
    Fun(),
    ag_engine_msg_executer:msg_single_apply(Msg8, true),
    Fun(),
    ag_engine_msg_executer:msg_single_apply(Msg9, true),
    Fun(),
    meck:unload().

test_msg_metrics_timeout(_Config) ->
    logger:set_primary_config(level, all),
    meck:new(ag_idcreator, [non_strict]),
    meck:expect(ag_idcreator, gen_newid, 1, fun gen_newid/1),
    meck:new(test_gateway, [unstick, passthrough]),
    meck:expect(test_gateway, send_async_handle_result, 3, fun(_, _, _) -> ok end),

    Msg7 = #{?MESSAGE_NAME_KEY => ?MSG_ROLEWORKER_TIMEOUT_TEST1, <<"b">> => 12345},
    Msg8 = #{?MESSAGE_NAME_KEY => ?MSG_ROLEWORKER_TIMEOUT_TEST2, <<"b">> => 12345},
    Msg9 = #{?MESSAGE_NAME_KEY => ?MSG_ROLEWORKER_TIMEOUT_TEST3, <<"b">> => 12345},

    WorkPid = test_meck_worker:quick_start_work(self()),
    ct:pal("quick_start_work WorkPid:~p~n", [WorkPid]),
    ag_engine_storage:set_roleworkerpid(WorkPid),

    ag_engine_msg_executer:msg_single_apply(Msg7, true),
    ag_engine_msg_executer:msg_single_apply(Msg8, true),
    ag_engine_msg_executer:msg_single_apply(Msg9, true),
    timer:sleep(5000),
    ag_engine_msg_executer:on_ping(),
    meck:unload().

test_gate_terminate(_Config) ->
    logger:set_primary_config(level, all),
    meck:new(ag_idcreator, [non_strict]),
    meck:expect(ag_idcreator, gen_newid, 1, fun gen_newid/1),
    meck:new(test_gateway, [unstick, passthrough]),
    meck:expect(test_gateway, send_async_handle_result, 3, fun(_, _, _) -> ok end),

    Msg7 = #{?MESSAGE_NAME_KEY => ?MSG_ROLEWORKER_TIMEOUT_TEST1, <<"b">> => 12345},
    Msg8 = #{?MESSAGE_NAME_KEY => ?MSG_ROLEWORKER_TIMEOUT_TEST2, <<"b">> => 12345},
    Msg9 = #{?MESSAGE_NAME_KEY => ?MSG_ROLEWORKER_TIMEOUT_TEST3, <<"b">> => 12345},

    WorkPid = test_meck_worker:quick_start_work(self()),
    ct:pal("quick_start_work WorkPid:~p~n", [WorkPid]),
    ag_engine_storage:set_roleworkerpid(WorkPid),
    ag_engine_msg_executer:msg_single_apply(Msg7, true),
    ag_engine_msg_executer:msg_single_apply(Msg8, true),
    ag_engine_msg_executer:msg_single_apply(Msg9, true),
    ag_engine_msg_executer:on_terminate(),
    meck:unload().

test_sync_message_handler(_Config) ->
    meck:new(ag_idcreator, [non_strict]),
    meck:expect(ag_idcreator, gen_newid, 1, fun gen_newid/1),
    Msg1 = #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST1, <<"b">> => 12345},
    Msg2 = #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST2, <<"b">> => 12345},
    Msg3 = #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST3, <<"b">> => 12345},
    Msg4 = #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST4, <<"b">> => 12345},
    Msg5 = #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST5, <<"b">> => 12345},
    Msg6 = #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST6, <<"b">> => 12345},
    Msg7 = #{?MESSAGE_NAME_KEY => ?MSG_ROLEWORKER_TEST, <<"b">> => 12345},

    Reply1 = ag_engine_msg_executer:msg_single_apply(Msg1,true),
    Reply2 = ag_engine_msg_executer:msg_single_apply(Msg2,true),
    Reply3 = ag_engine_msg_executer:msg_single_apply(Msg3,true),
    Reply4 = ag_engine_msg_executer:msg_single_apply(Msg4,true),
    Reply5 = ag_engine_msg_executer:msg_single_apply(Msg5,true),
    Reply6 = ag_engine_msg_executer:msg_single_apply(Msg6,true),

    ct:pal("Reply1:~p", [{Reply1, Reply2, Reply3, Reply4, Reply5, Reply6}]),
    Reply7 = ag_engine_msg_executer:msg_single_apply(Msg7,true),
    ct:pal("Reply7:~p", [Reply7]),
    ?assertMatch({false, #{<<"code">> := -1, <<"name">> := <<"InError">>}}, Reply7),
    meck:unload().

test_push_message(_Config) ->
    ag_engine_msg_executer:message_to_client(self(), <<"abcde">>),
    ag_engine_msg_executer:message_to_client(self(), "abcde"),
    ag_engine_msg_executer:message_to_client(self(), #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST1, <<"b">> => 12345}),
    ag_engine_msg_executer:text_message_to_client(self(), <<"abcde">>),
    ag_engine_msg_executer:text_message_to_client(self(), "abcde"),
    ag_engine_msg_executer:text_message_to_client(self(), #{?MESSAGE_NAME_KEY => ?MSG_GATE_TEST1, <<"b">> => 12345}).

gen_newid(_) ->
    erlang:monotonic_time() * 1000 + random:uniform(999).

metrics_exec_call_back(Info) ->
    ct:pal("metrics_exec_call_back Info:~p~n", [Info]).