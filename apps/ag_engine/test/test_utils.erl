%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @copyright (C) 2020, Harbour Studios
%%% @doc
%%%
%%% @end
%%% Created : 14. 一月 2020 17:29
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