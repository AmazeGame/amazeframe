%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
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

