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
-module(ag_logger_config).
-behaviour(agb_appsetting).
%% API
-export([table/0]).
-include_lib("ag_base/include/agb_appsetting.hrl").

-spec table() -> atom().
table() ->
    'AMAZEGAME_LOGGER_CONFIG'.
