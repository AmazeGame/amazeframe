-module(mysupervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-define(CHILD(I, Type),  #{id => I, start => {I, start_link, []},  restart => permanent,  shutdown => 5000, type => Type, modules => [I]}).
-define(CHILDM(I, Mod, Type),  #{id => I, start => {Mod, start_link, []},  restart => permanent,  shutdown => 5000, type => Type, modules => [Mod]}).

init([]) ->
    Child1 = ?CHILD(myserver,worker),
    Child2 = ?CHILD(myevent,worker),

    {ok, {#{strategy => one_for_one,
        intensity => 5,
        period => 30},
        [Child1,Child2]}
    }.
