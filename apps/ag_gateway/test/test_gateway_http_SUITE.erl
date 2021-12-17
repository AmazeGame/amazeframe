%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(test_gateway_http_SUITE).


%% API
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").
-include("test_msg_info.hrl").
-include_lib("ag_engine/include/ag_engine.hrl").

all() ->
    [test_http, test_custom_http_handle, test_custom_http_path].

init_per_suite(_Config) ->
    %load_mock_modules:compile_mock("../../test/SUITE_mock/"),
    logger:set_primary_config(level, all),
%%    meck_md:meck_cluster(),
    net_kernel:start(['ag_gateway@127.0.0.1', longnames]),
    agb_application:check_started(ag_gateway),
    _Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    meck_md:meck_cluster(),
    meck_md:meck_message_handler(),
    Config.

end_per_testcase(Case, _Config) ->
    meck:unload(),
    ct:pal("=== ~p end ===", [Case]),
    ok.

test_http(_Config) ->

    {ok, ConnPid} = gun:open("localhost", 10101, #{
        retry => 0,
        transport => tcp,
        protocols => [http]
    }),
    {ok, http} = gun:await_up(ConnPid),

    %ag_engine_cluster:player_from_session(a),
    Msg = #{<<"name">> => ?MSG_TEST_1},
    Ref1 = gun:post(ConnPid, "/", [{<<"content-type">>, <<"text/plain">>}], agb_json:encode(Msg)),
    ?assertEqual({ok, <<"{\"name\":\"test_reply_1\"}">>}, gun:await_body(ConnPid, Ref1, 10000)),

    Ref2 = gun:post(ConnPid, "/", [{<<"content-type">>, <<"text/plain">>}, {<<"proto">>, <<"json">>}, {<<"msgcarrier">>, <<"false">>}], agb_json:encode(Msg)),
    ?assertEqual({ok, <<"{\"name\":\"test_reply_1\"}">>}, gun:await_body(ConnPid, Ref2, 10000)),

    % 模拟auth失败
    Ref3 = gun:post(ConnPid, "/", [{<<"validate">>, <<"false">>}], agb_json:encode(Msg)),
    ?assertEqual({ok, <<"{\"name\":\"InError\",\"code\":-4}">>}, gun:await_body(ConnPid, Ref3, 10000)),
    % 模拟消息时间检测失败
    Ref4 = gun:post(ConnPid, "/", [{<<"validate">>, <<"time_out">>}], agb_json:encode(Msg)),
    ?assertEqual({ok, <<"{\"name\":\"InError\",\"code\":-14}">>}, gun:await_body(ConnPid, Ref4, 10000)),
    % 模拟版本号检测失败
    Ref5 = gun:post(ConnPid, "/", [{<<"validate">>, <<"require_version">>}], agb_json:encode(Msg)),
    ?assertEqual({ok, <<"{\"name\":\"InError\",\"code\":-13}">>}, gun:await_body(ConnPid, Ref5, 10000)),

    Ref6 = gun:post(ConnPid, "/", [{<<"content-type">>, <<"text/plain">>}], agb_json:encode(#{<<"name">> => ?MSG_TEST_2})),
    ct:pal("test agb_json:encode:~p~n", [agb_json:decode2map(<<"11111">>)]),
    ?assertEqual({ok, <<"\"11111\"">>}, gun:await_body(ConnPid, Ref6, 10000)),

    Ref7 = gun:post(ConnPid, "/", [{<<"content-type">>, <<"text/plain">>}], agb_json:encode(#{<<"name">> => ?MSG_TEST_3})),
    ?assertMatch({ok, _}, gun:await_body(ConnPid, Ref7, 10000)),

    Ref8 = gun:post(ConnPid, "/", [{<<"content-type">>, <<"text/plain">>}], agb_json:encode(#{<<"name">> => ?MSG_TEST_4})),
    ?assertEqual({ok, <<"{\"name\":\"test_reply_4\"}">>}, gun:await_body(ConnPid, Ref8, 10000)),

    Ref9 = gun:post(ConnPid, "/", [{<<"content-type">>, <<"text/plain">>}], agb_json:encode(#{<<"name">> => ?MSG_TEST_5})),
    {ok, Result} = gun:await_body(ConnPid, Ref9, 10000),
    ?assertMatch(#{<<"name">> := <<"InError">>, <<"code">> := -1}, agb_json:decode2map(Result)),

    Ref10 = gun:post(ConnPid, "/",
        [{<<"content-type">>, <<"text/plain">>}, {<<"proto">>, <<"json">>}, {<<"session">>, <<"1111">>}, {<<"msgcarrier">>, <<"true">>}],
        ag_engine_protocol_codec:encode(<<"json">>, [#{<<"name">> => ?MSG_TEST_6}], [])),
    ?assertMatch({_, _, 200, _}, gun:await(ConnPid, Ref10, 10000)),

    Ref11 = gun:post(ConnPid, "/",
        [{<<"content-type">>, <<"text/plain">>}, {<<"proto">>, <<"json">>}, {<<"msgcarrier">>, <<"true">>}],
        ag_engine_protocol_codec:encode(<<"json">>, [#{<<"name">> => ?MSG_TEST_6}], [])),
    ?assertMatch({_, _, 200, _}, gun:await(ConnPid, Ref11, 10000)).

test_custom_http_handle(_Config) ->
    {ok, ConnPid} = gun:open("localhost", 10102, #{
        retry => 0,
        transport => tcp,
        protocols => [http]
    }),
    {ok, http} = gun:await_up(ConnPid),
    Ref1 = gun:post(ConnPid, "/", [{<<"content-type">>, <<"text/plain">>}], <<"hello">>),
    ?assertEqual({ok, <<"{}">>}, gun:await_body(ConnPid, Ref1, 10000)).

test_custom_http_path(_Config) ->
    {ok, ConnPid} = gun:open("localhost", 10101, #{
        retry => 0,
        transport => tcp,
        protocols => [http]
    }),
    {ok, http} = gun:await_up(ConnPid),
    Ref1 = gun:post(ConnPid, "/heartbeat", [{<<"content-type">>, <<"text/plain">>}], <<"hello">>),
    ?assertEqual({ok, <<"{}">>}, gun:await_body(ConnPid, Ref1, 10000)).








