%%%-------------------------------------------------------------------
%%% @author ayongbc <ayongbc@sina.com> 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_cluster_config).
-behaviour(agb_appsetting).
-export([table/0]).
-include_lib("ag_base/include/agb_appsetting.hrl").
%% API

-spec table() -> atom().
table() ->
    'AMAZEGAME_CLUSTER_CONFIG'.
