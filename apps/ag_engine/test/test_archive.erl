%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @copyright (C) 2019, Harbour Studios
%%% @doc
%%%
%%% @end
%%% Created : 05. 七月 2019 17:27
%%%-------------------------------------------------------------------
-module(test_archive).

-behaviour(ag_engine_archive).
-include_lib("ag_engine/include/ag_engine.hrl").
-include_lib("ag_engine/include/ag_engine_code_defines.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").

%% API
-export([create_user/1, load_user/1]).

create_user(_Content) ->
    NewSecurity = "12345678",
    Archive = "22345678",
    {Archive, NewSecurity}.

load_user(_Content) ->
    NewSecurity = "12345678",
    Archive = "22345678",
    {Archive, NewSecurity}.

