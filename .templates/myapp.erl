-module(myapp).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mysupervisor:start_link().

stop(_State) ->
    ok.
