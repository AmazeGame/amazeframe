%%%-------------------------------------------------------------------
%% @doc ag_metable public API
%% @end
%%%-------------------------------------------------------------------

-module(ag_metable_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ag_metable_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
