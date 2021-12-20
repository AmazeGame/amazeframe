%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(test_storage_SUITE).


%% API
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    test_storage/1
]).

all()->
    [test_storage].

init_per_suite(Config) ->
    test_utils:load_config(Config),
    Config.

end_per_suite(_Config)->
    ok.

test_storage(_Config)->
    ag_engine_storage:set_archive(<<"abcde">>),
    ?assertEqual(<<"abcde">>,           ag_engine_storage:get_archive()),
    ag_engine_storage:set_gatenode(node()),
    ?assertEqual(node(),                ag_engine_storage:get_gatenode()),
    ag_engine_storage:set_gatepid(self()),
    ?assertEqual(self(),                ag_engine_storage:get_gatepid()),
    ag_engine_storage:set_gatepid(undefined),
    ag_engine_storage:set_id(<<"abcde">>),
    ?assertEqual(<<"abcde">>,           ag_engine_storage:get_id()),
    ag_engine_storage:set_idtype(test),
    ?assertEqual(test,                  ag_engine_storage:get_idtype()),
    ag_engine_storage:set_roleworkerpid(self()),
    ?assertEqual(self(),                ag_engine_storage:get_roleworkerpid()),
    ag_engine_storage:set_security(<<"zzzzzz">>),
    ?assertEqual(<<"zzzzzz">>,          ag_engine_storage:get_security()),
    ag_engine_storage:set_session(<<"bbbbbb">>),
    ?assertEqual(<<"bbbbbb">>,          ag_engine_storage:get_session()),
    ag_engine_storage:put(key,<<"Value">>),
    ?assertEqual(<<"Value">>,           ag_engine_storage:get(key)),

    ?assertEqual(undefined,                   ag_engine_storage:get_msg_metrics_info(key)),
    ag_engine_storage:update_msg_metrics_info(key,#{aa=>111}),
    ?assertEqual(#{aa=>111},           ag_engine_storage:get_msg_metrics_info(key)),
    ag_engine_storage:update_msg_metrics_info(key2,#{bb=>222}),
    ?assertEqual(#{bb=>222},           ag_engine_storage:get_msg_metrics_info(key2)),
    ag_engine_storage:update_msg_metrics_info(key,#{aa=>222}),
    ?assertEqual(#{aa=>222},           ag_engine_storage:get_msg_metrics_info(key)),
    ag_engine_storage:remove_msg_metrics_info(key),
    ?assertEqual(undefined,                  ag_engine_storage:get_msg_metrics_info(key)),
    ok.