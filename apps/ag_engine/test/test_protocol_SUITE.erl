%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(test_protocol_SUITE).


%% API
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    test_protocol_gpb/1,
    test_protocol_json/1,
    test_protocol_msgpack/1,
    msg_change_echo/1
]).

all() ->
    [
        test_protocol_json,
        test_protocol_msgpack,
        test_protocol_gpb
    ].

init_per_suite(Config) ->
%%    net_kernel:start(['game_master@127.0.0.1', longnames]),
    test_utils:load_config(Config),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(test_protocol_json, _Config) ->
    agb_application:check_started(ag_engine),
    _Config;
init_per_testcase(test_protocol_msgpack, _Config) ->
    agb_application:check_started(ag_engine),
    _Config;
init_per_testcase(test_protocol_gpb, _Config) ->
    application:set_env(ag_engine, gpb_msg_name_type, int32),
    agb_application:check_started(ag_engine),
    _Config;
init_per_testcase(Case, Config) ->
    ct:pal("=== ~p begin ===", [Case]),
    Config.

end_per_testcase(Case, _Config) ->
    ct:pal("=== ~p end ===", [Case]),
    agdb_cached_adapter_redis:flushdb(cluster_redis_pool),
    application:stop(ag_engine),
    ok.

test_protocol_json(_Config) ->
    ?assertEqual(<<"jsx_json">>, ag_engine_json_codec:protocol_name()),
    ?assertEqual(ok, ag_engine_json_codec:init()),
    JsonStr1 = ag_engine_json_codec:encode(#{<<"a">>=>1}),
    JsonStr2 = ag_engine_json_codec:encode(#{<<"b">>=>2}, abc),
    ?assertEqual(#{<<"a">>=>1}, ag_engine_json_codec:decode(JsonStr1)),
    ?assertEqual(#{<<"b">>=>2}, ag_engine_json_codec:decode(JsonStr2, abc)).

test_protocol_msgpack(_Config) ->
    ?assertEqual(<<"msgpack">>, ag_engine_msgpack_codec:protocol_name()),
    ?assertEqual(ok, ag_engine_msgpack_codec:init()),
    Msg1 = ag_engine_msgpack_codec:encode(#{<<"a">>=>1}),
    Msg2 = ag_engine_msgpack_codec:encode(#{<<"b">>=>2}, [{allow_atom, pack}]),
    ?assertEqual(#{<<"a">>=>1}, ag_engine_msgpack_codec:decode(Msg1)),
    ?assertEqual(#{<<"b">>=>2}, ag_engine_msgpack_codec:decode(Msg2, [{allow_atom, pack}])).

test_protocol_gpb(_Config) ->
%%    ct:pal("info :~p~n",[ message_pb:module_info()]),
%%    observer:start(),
    %ag_engine_gpb_codec:init(),
%%    ct:log("test_protocol_gpb application get_all_env:~p",[{agb_behaviour:get_function_modules("*_pb.beam", [{encode_msg, 2}, {decode_msg, 2}, {get_msg_names, 0}, {get_msg_defs, 0}])
%%    ,ets:tab2list('##$?ets_base_message_codec')}]),
    Msg1 = ag_engine_gpb_codec:encode(#{name => 1001, "echo" => 0, <<"token">> => <<"token">>}),
    Msg2 = ag_engine_gpb_codec:encode(#{<<"name">> => 1001, <<"echo">> => 0, <<"token">> => <<"token">>}),
    Msg3 = ag_engine_gpb_codec:encode(#{<<"name">> => 1001, <<"echo">> => 0, <<"token">> => <<"token">>}, {?MODULE, msg_change_echo}),
    ct:pal("test_protocol_gpb msg1:~p", [Msg1]),
    ?assertEqual(#{<<"name">> => 1001, <<"echo">> => 0, <<"token">> => <<"token">>}, ag_engine_gpb_codec:decode(Msg1)),
    ?assertEqual(#{<<"name">> => 1001, <<"echo">> => 0, <<"token">> => <<"token">>}, ag_engine_gpb_codec:decode(Msg2)),
    ?assertEqual(#{<<"name">> => 1001, <<"echo">> => 2, <<"token">> => <<"token">>}, ag_engine_gpb_codec:decode(Msg3, {?MODULE, msg_change_echo})),
    a.

msg_change_echo(Msg) ->
    Msg#{<<"echo">>=> 2}.
