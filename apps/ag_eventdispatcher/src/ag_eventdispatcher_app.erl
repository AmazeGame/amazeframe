%%%-------------------------------------------------------------------
%% @doc ag_eventdispatcher public API
%% @end
%%%-------------------------------------------------------------------

-module(ag_eventdispatcher_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ag_eventdispatcher_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
