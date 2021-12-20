%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(test_utils).


%% API
-export([load_config/1,init_suite/1]).

load_config(Config) ->
    [].

init_suite(Config)->
    logger:set_primary_config(level, all),
    net_kernel:start(['ag_engine@127.0.0.1', longnames]),
%%    test_utils:load_config(Config),
    agb_application:check_started(ag_engine_cluster),
    agb_application:check_started(ag_idcreator),
    Config.