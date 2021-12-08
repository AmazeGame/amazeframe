%%%-------------------------------------------------------------------
%% @doc ag_initiator top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ag_initiator_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(SERVER, ?MODULE).

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
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    %% codereloader
    ChildSpecs0 = case application:get_env(reloader_mode) of
            {ok, all} ->
                [?CHILD(agi_auto_codereloader, worker), ?CHILD(agi_codereloader, worker)];
            {ok, reloader} ->
                [?CHILD(agi_codereloader, worker)];
            {ok, autoreloader} ->
                [?CHILD(agi_auto_codereloader, worker)];
            undefined ->
                [?CHILD(agi_codereloader, worker)]
    end,

    %% add variable init
    ChildSpecs = [?CHILD(agi_variable_holder,worker)|ChildSpecs0],

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
