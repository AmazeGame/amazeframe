%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @copyright (C) 2019, Harbour Studios
%%% @doc
%%%
%%% @end
%%% Created : 17. 七月 2019 17:37
%%%-------------------------------------------------------------------
-module(test_message_helper_SUITE).


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    test_message_helper/1
]).

all() ->
    [test_message_helper].

init_per_suite(Config) ->
    test_utils:load_config(Config),
    Config.

end_per_suite(_Config) ->
    ok.

test_message_helper(_Config) ->
    ag_engine_message_wrap_code:init(),
    ?assertEqual(#{?MESSAGE_NAME_KEY=><<"Test">>},      ag_engine_message_helper:pack_name(<<"Test">>, #{})),
    ?assertEqual(#{?MESSAGE_NAME_KEY=><<"Test">>},      ag_engine_message_helper:pack_name("Test", #{})),
    ?assertEqual(#{?MESSAGE_NAME_KEY=><<"Test">>},      ag_engine_message_helper:pack_name('Test', #{})),
    ?assertEqual(
        #{<<"code">> => 200,<<"info">> => <<"200">>,<<"name">> => <<"Test">>},
        ag_engine_message_helper:pack_code('Test', 200)
    ),
    ?assertEqual(
        #{<<"code">> => 200,<<"info">> => <<"200">>,<<"name">> => <<"Test">>,<<"a">> => 1},
        ag_engine_message_helper:pack_code('Test', 200, #{<<"a">>=>1})
    ),
    ?assertEqual(<<"Test">>,                            ag_engine_message_helper:get_name(#{?MESSAGE_NAME_KEY=><<"Test">>})),
    ?assertError(noname_message,                        ag_engine_message_helper:get_name(#{})),
    ?assertEqual(<<"Echo">>,                            ag_engine_message_helper:get_echo(#{?INTERNAL_DEFINE_ECHO=><<"Echo">>})),
    ?assertEqual(<<>>,                                  ag_engine_message_helper:get_echo(#{})),
    ?assertEqual(#{?INTERNAL_DEFINE_ECHO=><<"Echo">>},  ag_engine_message_helper:pack_echo(<<"Echo">>,#{})),
    ?assertEqual(#{},                                   ag_engine_message_helper:pack_echo(<<>>,#{})),
    ?assertEqual(<<"Value">>,                           ag_engine_message_helper:get_value(<<"key">>,#{<<"key">> => <<"Value">>})),
    ?assertEqual(<<"Value">>,                           ag_engine_message_helper:get_value(<<"Key">>,#{<<"key">> => <<"Value">>})),
    ?assertEqual(<<"Value">>,                           ag_engine_message_helper:get_value("key",#{<<"key">> => <<"Value">>})),
    ?assertEqual(<<"Value">>,                           ag_engine_message_helper:get_value(key,#{<<"key">> => <<"Value">>})),
    ?assertEqual(<<"Value">>,                           ag_engine_message_helper:get_value(key,[{<<"key">> , <<"Value">>}])),
    ?assertEqual(<<"Value">>,                           ag_engine_message_helper:get_session(#{<<"session">> => <<"Value">>})).