%%%-------------------------------------------------------------------
%% @doc ag_initiator public API
%% @end
%%%-------------------------------------------------------------------

-module(ag_initiator_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ag_initiator_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
