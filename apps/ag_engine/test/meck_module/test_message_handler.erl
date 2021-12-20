%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(test_message_handler).
-author("Adrianx Lau <adrianx.lau@gmail.com>").
-behaviour(ag_engine_message_handler).

-include_lib("ag_engine/include/ag_engine_core_defines.hrl").
-include("msg_field.hrl").

%% API
-export([get_messages/0, handle/1]).
-spec(get_messages() -> [binary()]).
get_messages() ->
	{
		[
			?MSG_GATE_TEST1,
			?MSG_GATE_TEST2,
			?MSG_GATE_TEST3,
			?MSG_GATE_TEST4,
			?MSG_GATE_TEST5,
			?MSG_GATE_TEST6

		],
		[
			?MSG_ROLEWORKER_TEST,
			?MSG_ROLEWORKER_TEST1,
			?MSG_ROLEWORKER_TEST2,
			?MSG_ROLEWORKER_TIMEOUT_TEST1,
			?MSG_ROLEWORKER_TIMEOUT_TEST2,
			?MSG_ROLEWORKER_TIMEOUT_TEST3
		]
	}.



-spec(handle(Msg :: map()) -> {true, Reply :: map()} | {false, Reason :: term()}).
handle(#{?MESSAGE_NAME_KEY := ?MSG_GATE_TEST1}) ->
	{true,ag_engine_message_helper:pack_name(?MSG_GATE_TEST1_REPLY, #{<<"Result">> => <<"ok">>})};
handle(#{?MESSAGE_NAME_KEY := ?MSG_GATE_TEST2}) ->
	{true,[]};
handle(#{?MESSAGE_NAME_KEY := ?MSG_GATE_TEST3}) ->
	{true,<<>>};
handle(#{?MESSAGE_NAME_KEY := ?MSG_GATE_TEST5}) ->
	{false,[]};
handle(#{?MESSAGE_NAME_KEY := ?MSG_GATE_TEST6}) ->
	{false,<<>>};
handle(#{?MESSAGE_NAME_KEY := ?MSG_GATE_TEST4}) ->
	{false,ag_engine_message_helper:pack_name(?MSG_GATE_TEST2_REPLY, #{<<"Result">> => <<"error">>})};
handle(#{?MESSAGE_NAME_KEY := ?MSG_ROLEWORKER_TEST}) ->
	{true,ag_engine_message_helper:pack_name(?MSG_ROLEWORKER_TEST_REPLY, #{<<"Result">> => <<"ok">>})};
handle(#{?MESSAGE_NAME_KEY := ?MSG_ROLEWORKER_TEST2}) ->
	{true,<<>>};
handle(#{?MESSAGE_NAME_KEY := ?MSG_ROLEWORKER_TEST1}) ->
	{true,[]};
handle(#{?MESSAGE_NAME_KEY := ?MSG_ROLEWORKER_TIMEOUT_TEST1}) ->
	{true,[]};
handle(#{?MESSAGE_NAME_KEY := ?MSG_ROLEWORKER_TIMEOUT_TEST2}) ->
	{true,[]};
handle(#{?MESSAGE_NAME_KEY := ?MSG_ROLEWORKER_TIMEOUT_TEST3}) ->
	{true,[]}.


