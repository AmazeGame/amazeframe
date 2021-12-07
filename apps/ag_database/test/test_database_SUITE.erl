%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.29
%%%-------------------------------------------------------------------
%%%
-module(test_database_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(ct_helper, [config/2]).
%% API
-compile(export_all).

all() ->
    [test_add_pool].

init_per_suite(Config) ->
    logger:set_primary_config(level, all),
    application:ensure_all_started(ag_database),
    Config.

end_per_suite(_Config) ->
    ok.

test_add_pool(_Config) ->
    agdb_manager:add_pool(redis2, redis, {
        [{size, 5}, {max_overflow, 10}],
        [{host, "10.0.205.189"}, {port, 6010}, {database, 4}, {password, ""}, {reconnect_sleep, 100}]
    }),
    agdb_manager:add_pool(mongodb2, mongodb, {
        [],
        {unknown, ["10.0.205.189:27017"]},
        [{pool_size, 50}, {max_overflow, 10}],
        [{database, <<"maze_server1">>}, {w_mode, safe}, {r_mode, slave_ok}]
    }),
    agdb_manager:add_cached_pool(cached_pool2,
        {mongodb2, mongodb, {
            [],
            {unknown, ["10.0.205.189:27017"]},
            [{pool_size, 50}, {max_overflow, 10}],
            [{database, <<"maze_server1">>}, {w_mode, safe}, {r_mode, slave_ok}]
        }},
        {redis2, redis, {
            [{size, 5}, {max_overflow, 10}],
            [{host, "10.0.205.189"}, {port, 6010}, {database, 5}, {password, ""}, {reconnect_sleep, 100}]
        }}),
    timer:sleep(5000).