%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_cluster_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/1]).
%% Supervisor callbacks
-export([init/1]).

-include_lib("ag_base/include/agb_debuglogger.hrl").

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_child(Module :: atom()) ->
    supervisor:startchild_ret().
start_child(Module) ->
    Child = ?CHILD(Module, worker),
    ?LOG_DEBUG("ag_cluster_sup start_child ~p~n", [Child]),
    supervisor:start_child(ag_cluster_sup, Child).
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
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}
    | ignore).
init([]) ->
    Childs = [?CHILD(ag_cluster_manager, worker)],
    {ok, {{one_for_one, 5, 10}, Childs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
