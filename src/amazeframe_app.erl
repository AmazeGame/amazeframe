%%%-------------------------------------------------------------------
%% @doc amazeframe public API
%% @end
%%%-------------------------------------------------------------------

-module(amazeframe_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    amazeframe_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
