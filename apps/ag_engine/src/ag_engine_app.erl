%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_app).
-behaviour(application).

%% Application callbacks
-export([
    start/2,
    stop/1
]).
-export([get_inerror_name_key/0]).

-include("ag_engine_core_defines.hrl").
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ag_engine_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% API
%%%===================================================================
-spec get_inerror_name_key() ->
    term().
get_inerror_name_key() ->
    case application:get_env(ag_engine, inerror_name_key) of
        undefined ->
            <<"InError">>;
        {ok, Value} ->
Value
    end.