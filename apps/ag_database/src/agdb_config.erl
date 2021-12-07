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
-module(agdb_config).
-behaviour(agb_appsetting).
-export([table/0]).
-include_lib("ag_base/include/agb_appsetting.hrl").
%% API

-spec table() -> atom().
table() ->
    'AMAZEGAME_DB_CONFIG'.

