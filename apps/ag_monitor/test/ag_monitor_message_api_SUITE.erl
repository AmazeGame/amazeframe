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
-module(ag_monitor_message_api_SUITE).

%% API
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ag_monitor_msg_tracking_struct.hrl").

all() ->
    [observe].

init_per_suite(_Config) ->
    logger:set_primary_config(level, all),
    meck:new(ag_bizlogger, [non_strict]),
    meck:expect(ag_bizlogger, write_log, 2, fun write/2),
    application:start(prometheus),
    meck:new(prometheus_counter, [non_strict]),
    meck:expect(prometheus_counter, declare, 1, fun declare/1),
    meck:expect(prometheus_counter, remove, 2, fun remove/2),
    meck:expect(prometheus_counter, inc, 4, fun inc/4),
%%    prometheus_counter:incprometheus_histogram
    meck:new(prometheus_histogram, [non_strict]),
    meck:expect(prometheus_histogram, declare, 1, fun declare/1),
    meck:expect(prometheus_histogram, remove, 2, fun remove/2),
    meck:expect(prometheus_histogram, observe, 4, fun observe/4),

    _Config.

end_per_suite(_Config) ->
%%    meck_proc:stop(ag_bizlogger),
%%    meck_proc:stop(prometheus_histogram),
%%    meck_proc:stop(prometheus_counter),
    meck:unload(),
    ok.


init_per_testcase(_, Config) ->
    application:start(ag_monitor),
    Config.

end_per_testcase(_, _Config) ->
    ok.

init_per_group(_, Config) ->
    Config.
end_per_group(_) ->
    ok.

observe(_) ->
    ag_monitor_manager:close_system_monitor_handle(ag_monitor_detailed_write_handle),
    ag_monitor_manager:close_system_monitor_handle(ag_monitor_warning_write_handle),
%%    ag_monitor_manager:close_system_monitor_handle(ag_monitor_prometheus_instrumenter_handle),
    ag_monitor_manager:open_system_monitor_handle(ag_monitor_detailed_write_handle),
    ag_monitor_manager:open_system_monitor_handle(ag_monitor_warning_write_handle),
    ag_monitor_manager:open_system_monitor_handle(ag_monitor_prometheus_instrumenter_handle),
    Time = erlang:monotonic_time(),
    InfoList =
        [
            #{
                ?TRACKING_MSG_TYPE => sync_request, ?TRACKING_CLIENT_IP => "127.0.0.1",
                ?TRACKING_STATUS => fin,?TRACKING_USER_ID => <<"abc">>,
                ?TRACKING_REQUEST_NAME => <<"test">>, ?TRACKING_RESPONSE_NAME => <<"testReply">>,
                ?TRACKING_IN_MSG => #{<<"name">> => <<"test">>, <<"a">> => 1},
                ?TRACKING_OUT_MSG => #{<<"name">> => <<"testReply">>, <<"a">> => 1},
                ?TRACKING_REQ_START => Time, ?TRACKING_REQ_END => Time,
                ?TRACKING_EXEC_START => Time, ?TRACKING_EXEC_END => Time
            },
            #{
                ?TRACKING_MSG_TYPE => sync_request, ?TRACKING_CLIENT_IP => "127.0.0.1",
                ?TRACKING_STATUS => timeout,?TRACKING_USER_ID => <<"abc">>,
                ?TRACKING_REQUEST_NAME => <<"test">>, ?TRACKING_RESPONSE_NAME => <<"testReply">>,
                ?TRACKING_IN_MSG => #{<<"name">> => <<"test">>, <<"a">> => 1},
                ?TRACKING_OUT_MSG => #{<<"name">> => <<"testReply">>, <<"a">> => 1},
                ?TRACKING_REQ_START => Time
            },
            #{
                ?TRACKING_MSG_TYPE => sync_request, ?TRACKING_CLIENT_IP => "127.0.0.1",
                ?TRACKING_STATUS => exit,?TRACKING_USER_ID => <<"abc">>,
                ?TRACKING_REQUEST_NAME => <<"test">>, ?TRACKING_RESPONSE_NAME => <<"testReply">>,
                ?TRACKING_IN_MSG => #{<<"name">> => <<"test">>, <<"a">> => 1},
                ?TRACKING_OUT_MSG => #{<<"name">> => <<"testReply">>, <<"a">> => 1},
                ?TRACKING_REQ_START => Time
            },
            #{
                ?TRACKING_MSG_TYPE => async_request, ?TRACKING_CLIENT_IP => "127.0.0.1",
                ?TRACKING_STATUS => fin,?TRACKING_USER_ID => <<"abc">>,
                ?TRACKING_REQUEST_NAME => <<"test">>, ?TRACKING_RESPONSE_NAME => <<"testReply">>,
                ?TRACKING_IN_MSG => #{<<"name">> => <<"test">>, <<"a">> => 1},
                ?TRACKING_OUT_MSG => #{<<"name">> => <<"testReply">>, <<"a">> => 1},
                ?TRACKING_REQ_START => Time, ?TRACKING_REQ_END => Time + 550,
                ?TRACKING_EXEC_START => Time + 200, ?TRACKING_EXEC_END => Time + 300
            },
            #{
                ?TRACKING_MSG_TYPE => async_request, ?TRACKING_CLIENT_IP => "127.0.0.1",
                ?TRACKING_STATUS => timeout,?TRACKING_USER_ID => <<"abc">>,
                ?TRACKING_REQUEST_NAME => <<"test">>, ?TRACKING_RESPONSE_NAME => <<"testReply">>,
                ?TRACKING_IN_MSG => #{<<"name">> => <<"test">>, <<"a">> => 1},
                ?TRACKING_OUT_MSG => #{<<"name">> => <<"testReply">>, <<"a">> => 1},
                ?TRACKING_REQ_START => Time
            },
            #{
                ?TRACKING_MSG_TYPE => async_request, ?TRACKING_CLIENT_IP => "127.0.0.1",
                ?TRACKING_STATUS => exit,?TRACKING_USER_ID => <<"abc">>,
                ?TRACKING_REQUEST_NAME => <<"test">>, ?TRACKING_RESPONSE_NAME => <<"testReply">>,
                ?TRACKING_IN_MSG => #{<<"name">> => <<"test">>, <<"a">> => 1},
                ?TRACKING_OUT_MSG => #{<<"name">> => <<"testReply">>, <<"a">> => 1},
                ?TRACKING_REQ_START => Time
            },
            #{
                ?TRACKING_MSG_TYPE => push, ?TRACKING_CLIENT_IP => "127.0.0.1",
                ?TRACKING_RESPONSE_NAME => <<"testReply">>,?TRACKING_USER_ID => <<"abc">>,
                ?TRACKING_OUT_MSG => #{<<"name">> => <<"testReply">>, <<"a">> => 1}
            }
        ],
    [ag_monitor_message_api:observe(Info) || Info <- InfoList],
    ok.


write(_, Log) ->
    ct:pal("Log:~p", [Log]).

declare(Args) ->
    ct:pal("declare Args :~p~n", [Args]).

remove(Reg, Name) ->
    ct:pal("remove Reg :~p Name:~p~n", [Reg, Name]).

inc(Reg, Name, Labels, Count) ->
    ct:pal("inc Reg :~p Name:~p~n", [Reg, {Name, Labels, Count}]).

observe(Reg, Name, Labels, Count) ->
    ct:pal("observe Reg :~p Name:~p~n", [Reg, {Name, Labels, Count}]).