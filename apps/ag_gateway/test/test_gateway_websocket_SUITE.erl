%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(test_gateway_websocket_SUITE).

%% API
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").
-include("test_msg_info.hrl").

all() ->
    [test_websocket_okmsg, test_websocket_errmsg1, test_websocket_errmsg2, test_websocket_errmsg3,
        test_websocket_errmsg4, test_websocket_errmsg6, test_websocket_errmsg7, test_websocket_msgcarrier].

init_per_suite(_Config) ->
    meck_md:meck_cluster(),
    logger:set_primary_config(level, all),
    application:start(ag_engine),
    application:start(ag_gateway),
    application:start(gun),
    meck:unload(),
    _Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    meck_md:meck_cluster(),
    meck_md:meck_message_handler(),
    Config.

end_per_testcase(Case, _Config) ->
    ct:pal("=== ~p end ===", [Case]),
    meck:unload(),
    ok.

test_websocket_msgcarrier(_Config) ->
    {ok, Pid, StreamRef} = new_websocket([{<<"msgcarrier">>, "true"}]),
    timer:sleep(3500),
    {ok, {binary, Reply}} =
        send_msg(Pid, StreamRef,
            {
                binary, ag_engine_protocol_codec:encode(<<"json">>,
                [
                    #{<<"name">> => ?MSG_TEST_13},
                    #{<<"name">> => ?MSG_TEST_13}
                ], [])
            }, 1),
    ReplyObject = ag_engine_protocol_codec:decode(<<"json">>, true, Reply, []),
    ?assertEqual([#{<<"name">> => <<"test_reply_13">>}, #{<<"name">> => <<"test_reply_13">>}], ReplyObject),

    % 测试服务器主动推送的消息
    {ok, {text, Reply1}} =
        send_msg(Pid, StreamRef,
            {text,ag_engine_protocol_codec:encode(<<"json">>,[#{<<"name">> => ?MSG_TEST_9, <<"Type">> => 1}],[])},
            1),
    ReplyObject1 = ag_engine_protocol_codec:decode(<<"json">>, true, Reply1, []),
    ?assertEqual([#{<<"name">> => <<"text_message_to_client">>}], ReplyObject1),
    ?assertEqual(
        {ok, {text, <<"text_message_to_client">>}},
        send_msg(Pid, StreamRef,
            {text, ag_engine_protocol_codec:encode(<<"json">>,[#{<<"name">> => ?MSG_TEST_9, <<"Type">> => 2}],[])},
            1)),

    {ok, {binary, Reply2}} =
        send_msg(Pid, StreamRef,
            {binary,ag_engine_protocol_codec:encode(<<"json">>,[#{<<"name">> => ?MSG_TEST_9, <<"Type">> => 3}],[])},
            1),
    ReplyObject2 = ag_engine_protocol_codec:decode(<<"json">>, true, Reply2, []),
    ?assertEqual([#{<<"name">> => <<"message_to_client">>}], ReplyObject2),
    ?assertEqual(
        {ok, {binary, <<"message_to_client">>}},
        send_msg(Pid, StreamRef,
            {binary, ag_engine_protocol_codec:encode(<<"json">>,[#{<<"name">> => ?MSG_TEST_9, <<"Type">> => 4}],[])},
            1)),
    ?assertEqual({ok, close}, send_msg(Pid, StreamRef, close, 1)).

test_websocket_okmsg(_Config) ->
    {ok, Pid, StreamRef} = new_websocket(),
    send_msg(Pid, StreamRef, {ping, <<"1232">>}, 1),
    % 测试返回成功的消息
    ?assertEqual({ok, {binary, <<"{\"name\":\"test_reply_1\"}">>}}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_1})}, 1)),
    % 这两条信息没有返回值，也没有发生错误，所以返回值为timeout
    ?assertEqual({error, timeout}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_7})}, 1)),
    ?assertEqual({error, timeout}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_8})}, 1)),
    % 测试服务器主动推送的消息
    ?assertEqual({ok, {text, <<"{\"name\":\"text_message_to_client\"}">>}}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_9, <<"Type">> => 1})}, 1)),
    ?assertEqual({ok, {text, <<"text_message_to_client">>}}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_9, <<"Type">> => 2})}, 1)),
    ?assertEqual({ok, {binary, <<"{\"name\":\"message_to_client\"}">>}}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_9, <<"Type">> => 3})}, 1)),
    ?assertEqual({ok, {binary, <<"message_to_client">>}}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_9, <<"Type">> => 4})}, 1)),
    ?assertEqual({ok, close}, send_msg(Pid, StreamRef, close, 1)).

test_websocket_errmsg1(_Config) ->
    {ok, Pid, StreamRef} = new_websocket(),
    ?assertEqual({ok, {close, 1000, <<>>}}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_2})}, 2)).

test_websocket_errmsg2(_Config) ->
    {ok, Pid, StreamRef} = new_websocket(),
    ?assertEqual({ok, {close, 1000, <<>>}}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_4})}, 2)).

test_websocket_errmsg3(_Config) ->
    {ok, Pid, StreamRef} = new_websocket(),
    ?assertEqual({ok, {close, 1000, <<>>}}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_5})}, 2)).

test_websocket_errmsg4(_Config) ->
    {ok, Pid, StreamRef} = new_websocket(),
    ?assertEqual({ok, {close, 1000, <<>>}}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_10})}, 1)).

test_websocket_errmsg5(_Config) ->
    {ok, Pid, StreamRef} = new_websocket(),
    ?assertEqual({ok, {close, 1000, <<>>}}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_11})}, 1)).

test_websocket_errmsg6(_Config) ->
    {ok, Pid, StreamRef} = new_websocket(),
    ?assertEqual({ok, {close, 1000, <<>>}}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_12})}, 1)).

test_websocket_errmsg7(_Config) ->
    {ok, Pid, StreamRef} = new_websocket(),
    ?assertEqual({ok, {binary, <<"{\"code\":10002}">>}}, send_msg(Pid, StreamRef, {binary, agb_json:encode(#{<<"name">> => ?MSG_TEST_14})}, 1)).

new_websocket() ->
    new_websocket([]).

new_websocket(Headers) ->
    {ok, Pid} = gun:open("localhost", 10101, #{retry => 0, transport => tcp}),
    {ok, http} = gun:await_up(Pid),
    put(msgqueue, []),
    gun:ws_upgrade(Pid, "/", Headers, #{compress => true}),
    receive
        {gun_upgrade, Pid1, StreamRef1, [<<"websocket">>], _} ->
            {ok, Pid1, StreamRef1};
        {gun_response, _Pid1, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, _Pid1, _StreamRef1, Reason} ->
            ct:pal("ws_upgrade error Reason:~p", [Reason]),
            exit({ws_upgrade_failed, Reason})
    after 1000 ->
        error(timeout)
    end.

send_msg(Pid, StreamRef, Msg, ReceiveNum) ->
    gun:ws_send(Pid, Msg),
    Data = receive_ws(Pid, StreamRef, ReceiveNum),
    Data.

receive_ws(ConnPid, StreamRef, 1) ->
    receive
        {gun_ws, ConnPid, StreamRef, Frame} ->
            ct:pal("receive_ws Data:~p", [Frame]),
            {ok, Frame}
    after 1000 ->
        {error, timeout}
    end;
receive_ws(ConnPid, StreamRef, Count) ->
    receive
        {gun_ws, ConnPid, StreamRef, Frame} ->
            ct:pal("receive_ws Data:~p", [Frame]),
            receive_ws(ConnPid, StreamRef, Count - 1)
    after 1000 ->
        {error, timeout}
    end.
