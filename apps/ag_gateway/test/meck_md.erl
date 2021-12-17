%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(meck_md).
-include_lib("ag_engine/include/ag_engine.hrl").
-include("test_msg_info.hrl").
-include_lib("ag_engine/include/ag_engine_code_defines.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").
%% API
-compile(export_all).
-compile({no_auto_import, [apply/2]}).

meck_cluster() ->
    meck:new(ag_engine_cluster, [passthrough]),
    meck:new(ag_idcreator, [non_strict, passthrough]),
    meck:expect(ag_idcreator, gen_newid, 1, fun gen_newid/1),
    meck:expect(ag_engine_cluster, check_cluster, 0, true),
    meck:expect(ag_engine_cluster, init, 0, ok),
    meck:expect(ag_engine_cluster, register_as_gate_manager, 1, ok),
    meck:expect(ag_engine_cluster, gate_offline, 0, ok),
    meck:expect(ag_engine_cluster, increase_gate, 1, ok),
    meck:expect(ag_engine_cluster, register_as_playeragent_manager, 1, ok),
    meck:expect(ag_engine_cluster, player_from_session, 1, #online_player{id = <<"111111">>, archive = <<"222222">>, security = <<"afafadfadf">>, agent_pid = 111}).


meck_message_handler() ->
    meck:new(ag_engine_msg_executer, [non_strict, passthrough]),
    meck:expect(ag_engine_msg_executer, msg_single_apply, 2, fun msg_single_apply/2),
    meck:expect(ag_engine_msg_executer, msg_carrier_apply, 2, fun msg_carrier_apply/2),
    meck:expect(ag_engine_msg_executer, msg_carrier_apply, 1, fun msg_carrier_apply/1).

%meck:expect(ag_message_handler, send_s2c_msg, 1, fun send_s2c_msg/1).

msg_single_apply(InMsgObject, _IsUseAsync) ->
    apply(InMsgObject).

msg_carrier_apply(InMsgObjects) ->
    Rs = [apply(InMsgObject) || InMsgObject <- InMsgObjects],
    {true, [Result || {_, Result} <- Rs, Result =/= <<>>, Result =/= []]}.
msg_carrier_apply(_, InMsgObjects) ->
    Rs = [apply(InMsgObject) || InMsgObject <- InMsgObjects],
    {true, [Result || {_, Result} <- Rs, Result =/= <<>>, Result =/= []]}.

apply(InMsgObject) ->
    case maps:find(?MESSAGE_NAME_KEY, InMsgObject) of
        {ok, ?MSG_TEST_1} ->
            ct:pal("InMsgObject ~p~n", [InMsgObject]),
            {true, #{?MESSAGE_NAME_KEY => ?MSG_TEST_REPLY_1}};
        {ok, ?MSG_TEST_2} ->
            {false, <<"11111">>};
        {ok, ?MSG_TEST_3} ->
            {false, [#{?MESSAGE_NAME_KEY => ?MSG_TEST_REPLY_3}, #{?MESSAGE_NAME_KEY => ?MSG_TEST_REPLY_3}]};
        {ok, ?MSG_TEST_4} ->
            {false, #{?MESSAGE_NAME_KEY => ?MSG_TEST_REPLY_4}};
        {ok, ?MSG_TEST_6} ->
            ct:pal("InMsgObject ~p~n", [InMsgObject]),
            {true, [#{?MESSAGE_NAME_KEY => ?MSG_TEST_REPLY_6}]};
        {ok, ?MSG_TEST_7} ->
            {true, []};
        {ok, ?MSG_TEST_8} ->
            {true, <<>>};
        {ok, ?MSG_TEST_9} ->
            send_s2c_msg(InMsgObject),
            {true, <<>>};
        {ok, ?MSG_TEST_10} ->
            ag_gateway:on_bad_request(self()),
            {true, <<>>};
%%        {ok,?MSG_TEST_11}->
%%            ag_gateway:on_playeragent_exit(self()),
%%            {true,<<>>};
        {ok, ?MSG_TEST_12} ->
            self() ! kick_user,
            {true, <<>>};
        {ok, ?MSG_TEST_14} ->
            self() ! {kick_user, kick_user_other_login, #{code => 10002}},
            {true, <<>>};
        {ok, ?MSG_TEST_13} ->
            ct:pal("InMsgObject ~p~n", [InMsgObject]),
            self() ! {'CARRIER_CHECK'},
            {true, #{?MESSAGE_NAME_KEY => ?MSG_TEST_REPLY_13}};
        _ ->
            OutMsgObj = ag_engine_message_helper:pack_code(?MSG_INTERNAL_ERROR, ?ERROR_CODE_INTERNAL_SERVER_ERROR),
            {false, OutMsgObj}
    end.

send_s2c_msg(#{<<"Type">> := 1}) ->
    ag_gateway:text_message_to_client(self(), #{?MESSAGE_NAME_KEY => <<"text_message_to_client">>});
send_s2c_msg(#{<<"Type">> := 2}) ->
    ag_gateway:text_message_to_client(self(), "text_message_to_client");
send_s2c_msg(#{<<"Type">> := 3}) ->
    ag_gateway:message_to_client(self(), #{?MESSAGE_NAME_KEY => <<"message_to_client">>});
send_s2c_msg(#{<<"Type">> := 4}) ->
    ag_gateway:message_to_client(self(), "message_to_client").

gen_newid(_) ->
    erlang:monotonic_time().