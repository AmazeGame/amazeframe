%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(test_msg_handle_metrics_SUITE).


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ag_engine_core_defines.hrl").

-define(TRACKING_INFO_KEY, <<"##tracking_Info##">>).
-define(TRACKING_ID, <<"tracking_id">>).
-define(TRACKING_EXEC_START, <<"tracking_exec_start">>).
-define(TRACKING_EXEC_END, <<"tracking_exec_end">>).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    test_pack/1
]).

all() ->
    [test_pack].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

test_pack(_Config) ->
    MsgObject = #{<<"name">> => name},
    MetricsInfo =
        #{
            requestname => name
        },
    MetricsInfo0 =
        #{
            requestname => name,
            status => fin
        },
    MsgObject0 = ag_engine_msg_metrics_executer:pack_tracking_info_in_echo(MsgObject, MetricsInfo),
    ?assertEqual(
        #{<<"name">> => name, ?INTERNAL_DEFINE_ECHO=>#{?TRACKING_INFO_KEY=>MetricsInfo}}, MsgObject0),
    MsgObject1 = ag_engine_msg_metrics_executer:pack_tracking_info_in_echo(MsgObject, MetricsInfo0),
    ?assertEqual(
        #{<<"name">> => name, ?INTERNAL_DEFINE_ECHO=>#{?TRACKING_INFO_KEY=>MetricsInfo0}}, MsgObject1),

    MsgObject2 = #{<<"name">> => name, ?INTERNAL_DEFINE_ECHO => "abc"},
    MsgObject3 = ag_engine_msg_metrics_executer:pack_tracking_info_in_echo(MsgObject2, MetricsInfo),
    ?assertEqual(
        #{<<"name">> => name, ?INTERNAL_DEFINE_ECHO=>#{?INTERNAL_DEFINE_ECHO=>"abc", ?TRACKING_INFO_KEY=>MetricsInfo}}, MsgObject3),
    MsgObject4 = ag_engine_msg_metrics_executer:pack_tracking_info_in_echo(MsgObject3, MetricsInfo0),
    ?assertEqual(
        #{<<"name">> => name, ?INTERNAL_DEFINE_ECHO=>#{?INTERNAL_DEFINE_ECHO=>"abc", ?TRACKING_INFO_KEY=>MetricsInfo0}}, MsgObject4),

    MsgObject5 = #{<<"name">> => name, ?INTERNAL_DEFINE_ECHO => #{p=>123456}},
    MsgObject6 = ag_engine_msg_metrics_executer:pack_tracking_info_in_echo(MsgObject5, MetricsInfo),
    ?assertEqual(
        #{<<"name">> => name, ?INTERNAL_DEFINE_ECHO=>#{?INTERNAL_DEFINE_ECHO=>#{p=>123456}, ?TRACKING_INFO_KEY=>MetricsInfo}}, MsgObject6),
    MsgObject7 = ag_engine_msg_metrics_executer:pack_tracking_info_in_echo(MsgObject6, MetricsInfo0),
    ?assertEqual(
        #{<<"name">> => name, ?INTERNAL_DEFINE_ECHO=>#{?INTERNAL_DEFINE_ECHO=>#{p=>123456}, ?TRACKING_INFO_KEY=>MetricsInfo0}}, MsgObject7),

    MsgObject8 = ag_engine_msg_metrics_executer:unpack_tracking_info_from_echo(MsgObject7),
    ?assertEqual(
        #{<<"name">> => name, ?INTERNAL_DEFINE_ECHO=>#{p=>123456}}, MsgObject8).