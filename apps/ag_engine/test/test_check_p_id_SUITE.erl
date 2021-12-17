%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @copyright (C) 2019, Harbour Studios
%%% @doc
%%%
%%% @end
%%% Created : 13. 十一月 2019 17:10
%%%-------------------------------------------------------------------
-module(test_check_p_id_SUITE).


%% API
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").

-export([
    all/0,
    init_per_suite/1,
    init_per_testcase/2,
    end_per_suite/1,
    end_per_testcase/2,
    test_close_check_package_number/1,
    test_open_check_package_number/1
]).

all() ->
    [
        test_close_check_package_number,
        test_open_check_package_number
    ].

init_per_suite(Config) ->
    test_utils:init_suite(Config),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(test_close_check_package_number, Config) ->
    application:ensure_all_started(ag_engine),
    ct:pal("init_per_testcase test_close_check_package_number"),
    Config;
init_per_testcase(test_open_check_package_number, Config) ->
    application:set_env(ag_engine, check_packagenumber,
        #{
            database =>     %% 保存包号的db，现只支持redis
            [
                {pools, packagenumber_redis_pool},
                %{driver, redis_cluster},
                {driver, redis},
                {option,
                    {
                        [{size, 5}, {max_overflow, 0}],
                        [{host, "10.0.115.216"}, {port, 6379}, {database, 15}, {password, ""}, {reconnect_sleep, 100}]
                    }
                }
            ],
            maxdiff => 10   %% 比较包号时最大的间隔,默认为1
        }),
    application:ensure_all_started(ag_engine),
    ct:pal("init_per_testcase test_open_check_package_number"),
    Config;

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(test_open_check_package_number, _Config) ->
    ct:pal("end_per_testcase test_open_check_package_number"),
    agdb_cached_adapter_redis:flushdb(packagenumber_redis_pool),
    agdb_cached_adapter_redis:flushdb(cluster_redis_pool),
    application:stop(ag_engine),
    ok;
end_per_testcase(test_close_check_package_number, _Config) ->
    ct:pal("end_per_testcase test_close_check_package_number"),
    agdb_cached_adapter_redis:flushdb(cluster_redis_pool),
    application:stop(ag_engine),
    ok;
end_per_testcase(_Case, _Config) ->
    ok.

test_close_check_package_number(_Config) ->
    ?assertEqual(true, ag_engine_inner_check_packagenumber:check_package_number(#{}, #{})),
    ?assertEqual(true, ag_engine_inner_check_packagenumber:reset_package_number(<<"123456">>)).

test_open_check_package_number(_Config) ->
    test_reset_package_number(),
    test_check_package_number().

test_reset_package_number() ->
    Id = <<"123456">>,
    ag_engine_inner_check_packagenumber:reset_package_number(Id),
    {ok, NumBin} = agdb_cached_adapter:get(packagenumber_redis_pool, cache_key(<<"packagenumber">>, Id)),
    ?assertEqual(0, agb_convertor:to_integer(NumBin)),
    agdb_cached_adapter:set(packagenumber_redis_pool, cache_key(<<"packagenumber">>, Id), 5),
    ag_engine_inner_check_packagenumber:reset_package_number(Id),
    {ok, NumBin} = agdb_cached_adapter:get(packagenumber_redis_pool, cache_key(<<"packagenumber">>, Id)),
    ?assertEqual(0, agb_convertor:to_integer(NumBin)).

test_check_package_number() ->
    Id = <<"123456">>,
    ag_engine_inner_check_packagenumber:reset_package_number(Id),
    ?assertEqual(true, ag_engine_inner_check_packagenumber:check_package_number(#{?INTERNAL_DEFINE_PACKAGE_NUMBER => 1}, #{?INTERNAL_DEFINE_ID=>Id})),
    ?assertEqual(p_no, ag_engine_inner_check_packagenumber:check_package_number(#{?INTERNAL_DEFINE_PACKAGE_NUMBER => 1}, #{?INTERNAL_DEFINE_ID=>Id})),
    ?assertEqual(true, ag_engine_inner_check_packagenumber:check_package_number(#{?INTERNAL_DEFINE_PACKAGE_NUMBER => 2}, #{?INTERNAL_DEFINE_ID=>Id})),
    ?assertEqual(p_no, ag_engine_inner_check_packagenumber:check_package_number(#{?INTERNAL_DEFINE_PACKAGE_NUMBER => 20}, #{?INTERNAL_DEFINE_ID=>Id})),
    agdb_cached_adapter:set(packagenumber_redis_pool, cache_key(<<"packagenumber">>, Id), 16#FFFFFFFF - 2),
    ?assertEqual(true, ag_engine_inner_check_packagenumber:check_package_number(#{?INTERNAL_DEFINE_PACKAGE_NUMBER => 1}, #{?INTERNAL_DEFINE_ID=>Id})).

cache_key(Prefix, Key) ->
    binary:list_to_bin([Prefix, "_", Key]).