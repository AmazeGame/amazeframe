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
-module(ag_database_variable).
-behaviour(agb_variable).
-export([table/0]).
-include_lib("ag_base/include/agb_variable.hrl").
%% API

-spec table() -> atom().
table() ->
    'AMAZEGAME_DB_VARIABLE'.

