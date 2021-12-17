%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
%%%
-module(test_handle_SUITE).

%% API
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ag_monitor_msg_tracking_struct.hrl").

-record(state, {
    biz_name,               %% bizlogger的bizname名称
    analyze_frequency,      %% 分析记录信息的频率
    threshold,                 %% 打印警告log的阀值
    record_message_map = #{}      %% 记录的消息信息
}).

all() ->
    [test_warning_write_handle, test_on_record_message, test_init].

init_per_suite(_Config) ->
    logger:set_primary_config(level, all),
    application:ensure_all_started(ag_monitor),
    _Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.
end_per_testcase(_, _Config) ->
    ok.

init_per_group(_, Config) ->
    Config.
end_per_group(_) ->
    ok.

test_warning_write_handle(_Config) ->
    RecordMap = #{"127.0.0.1"=>#{"a"=>1, "b"=>100, "c"=>200}, "10.0.111.230"=>#{"aa"=>1, "bb"=>100, "cc"=>200}},
    meck:new(ag_bizlogger, [unstick, passthrough]),
    meck:expect(ag_bizlogger, write_log, 2, fun write/2),
    ag_monitor_warning_write_handle:do_analyze(RecordMap, aaa, 0),
    meck_proc:stop(ag_bizlogger).

test_on_record_message(_Config) ->
    State = #state{},
    Time = erlang:monotonic_time(),
    State1 = ag_monitor_warning_write_handle:on_record_message(
        #{
            ?TRACKING_MSG_TYPE => sync_request,?TRACKING_CLIENT_IP => "127.0.0.1",
            ?TRACKING_STATUS => fin,?TRACKING_USER_ID => <<"abc">>,
            ?TRACKING_REQUEST_NAME => <<"test">> , ?TRACKING_RESPONSE_NAME => <<"testReply">>,
            ?TRACKING_IN_MSG => #{<<"name">>=><<"test">>,<<"a">>=>1},
            ?TRACKING_OUT_MSG => #{<<"name">>=><<"testReply">>,<<"a">>=>1},
            ?TRACKING_REQ_START=> Time,?TRACKING_REQ_END=>Time,
            ?TRACKING_EXEC_START => Time,?TRACKING_EXEC_END => Time
        }, State),
    State2 = ag_monitor_warning_write_handle:on_record_message(
        #{
            ?TRACKING_MSG_TYPE => async_request,?TRACKING_CLIENT_IP => "127.0.0.1",
            ?TRACKING_STATUS => fin,?TRACKING_USER_ID => <<"abc">>,
            ?TRACKING_REQUEST_NAME => <<"test">> , ?TRACKING_RESPONSE_NAME => <<"testReply">>,
            ?TRACKING_IN_MSG => #{<<"name">>=><<"test">>,<<"a">>=>1},
            ?TRACKING_OUT_MSG => #{<<"name">>=><<"testReply">>,<<"a">>=>1},
            ?TRACKING_REQ_START=> Time,?TRACKING_REQ_END=>Time+550,
            ?TRACKING_EXEC_START => Time+200,?TRACKING_EXEC_END => Time+300
        }, State1),
    State3 = ag_monitor_warning_write_handle:on_record_message(
        #{
            ?TRACKING_MSG_TYPE => push,?TRACKING_CLIENT_IP => "127.0.0.1",
            ?TRACKING_RESPONSE_NAME => <<"testReply">>,?TRACKING_USER_ID => <<"abc">>,
            ?TRACKING_OUT_MSG => #{<<"name">>=><<"testReply">>,<<"a">>=>1}
        }, State2),
    ct:pal("State2:~p", [State3]),
    ok.


test_init(_) ->
    ag_monitor_warning_write_handle:init([#{bizlogger_bizname=>test}]),
    ag_monitor_warning_write_handle:init([#{bizlogger_bizname=>test, binarymaxbytesize =>2}]),
    ag_monitor_warning_write_handle:init([#{bizlogger_bizname=>test, unmatchlist =>[]}]),

    ag_monitor_detailed_write_handle:init([#{bizlogger_bizname=>test}]),
    ag_monitor_detailed_write_handle:init([#{bizlogger_bizname=>test, threshold=>1}]),
    ag_monitor_detailed_write_handle:init([#{bizlogger_bizname=>test, frequency=>1}]),
    ok.

write(_, Log) ->
    ct:pal("Log:~p", [Log]).

