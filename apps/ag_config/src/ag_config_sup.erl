%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.28
%%%-------------------------------------------------------------------
%%%
-module(ag_config_sup).

-include_lib("ag_base/include/agb_debuglogger.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([
    init/1,
    start_child/2
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILDA(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() ->
    supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init([]) ->
    {ok, {{one_for_one, Intensity :: non_neg_integer(), Period :: pos_integer()}, [supervisor:child_spec()]}}.
init([]) ->
    ?LOG_INFO("ag_config_sup init"),
    ChildSpecs = [
        ?CHILD(ag_config_manager, worker)
    ],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.

-spec start_child(Module :: atom(), Args :: any()) ->
    supervisor:startchild_ret().
start_child(Module, Args) ->
    ?LOG_INFO("ag_config_sup start_child Module:~p Args:~p", [Module, Args]),
    Child = ?CHILDA(Module, worker, [Args]),
    ?LOG_INFO("ag_config_sup start_child Child:~p ", [Child]),
    supervisor:start_child(ag_config_sup, Child).
