%%%-------------------------------------------------------------------
%% @doc ag_metable top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ag_metable_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_SPEC(Id,Mod,Type,Args) , {Id, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    Child = ?CHILD(ag_metable_holder,worker),
    {ok, { {one_for_one, 5, 10}, [Child]} }.

%% internal functions
