%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.11.02
%%%-------------------------------------------------------------------
%%%
-module(ag_debuglogger_sup).


-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([start_child/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILDA(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec init([]) ->
    {ok, {{one_for_one, Intensity :: non_neg_integer(), Period :: pos_integer()}, [supervisor:child_spec()]}}.
init([]) ->
    ChildSpecs = [
        ?CHILD(ag_debuglogger_manager, worker)
    ],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec start_child(ChildSpec :: supervisor:child_spec() | (List :: [term()])) ->
    supervisor:startchild_ret().
start_child(ChildSpec) ->
    supervisor:start_child(ag_debuglogger_sup, ChildSpec).