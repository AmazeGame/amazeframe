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
-module(ag_config_adapter_localfile).

-include_lib("ag_base/include/agb_debuglogger.hrl").
-behaviour(ag_config_adapter).

%% API
-export([init_config/1]).

-spec init_config(ConfigFile :: map()) ->
    ok.
init_config(#{path:=ConfigFile}) ->
    ?LOG_INFO(" ag_adapter_localfile init ~p~n", [ConfigFile]),
    agb_application:config_file(ConfigFile),
    ok.