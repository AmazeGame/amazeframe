%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
%%%
-module(ag_monitor_sup).

%% API
-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_child/2
]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILDA(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
    Child = [
        ?CHILD(ag_monitor_manager, worker)
    ],
    {ok, {{one_for_one, 5, 10}, Child}}.

-spec start_child(Module :: atom(), Args :: any()) -> supervisor:startchild_ret().
start_child(Module, Args) ->
    Child = ?CHILDA(Module, worker, [Args]),
    supervisor:start_child(?MODULE, Child).