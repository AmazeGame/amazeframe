%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.28
%%%-------------------------------------------------------------------
%%%
-module(ag_config_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    all/0
]).

%% Test cases
-export([test_localfile/1]).

init_per_suite(Config) ->
    logger:set_primary_config(level, all),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(test_localfile, Config) ->
    agb_application:check_started(ag_config),
    Config;
init_per_testcase(test_network, Config) ->
    agb_application:check_started(ag_config),
    Config;
init_per_testcase(test_network_error1, Config) ->
    agb_application:check_started(ag_config),
    Config;
init_per_testcase(test_network_error2, Config) ->
    agb_application:check_started(ag_config),
    Config;
init_per_testcase(test_network_error3, Config) ->
    agb_application:check_started(ag_config),
    Config.

end_per_testcase(_, _Config) ->
    application:stop(ag_config),
    ok.

all() ->
    [test_localfile].


%%%_* Test cases ===============================================================
test_localfile(Config) ->
    Dir = proplists:get_value(data_dir, Config),
    file:set_cwd(Dir),
    {_, AppConfig} = application:get_env(ag_config, ag_config_adapter_localfile),
    ag_config_adapter_localfile:init_config(AppConfig),
    assert_config().

assert_config() ->
    ?assertEqual(application:get_env(ag_node, master), {ok, game_master}),
    ?assertEqual(application:get_env(ag_node, cookie), {ok, 'c360a105019091e0f5f07bb6c0194a356e3ca7ba'}),
    ?assertEqual(application:get_env(ag_database, mysql_pool), {ok, {mysql, mysql_option}}).