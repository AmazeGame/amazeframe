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

-module(ag_node_sup).


-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: term()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
-spec init([]) ->
    {ok, {{one_for_one, Intensity :: non_neg_integer(), Period :: pos_integer()}, [supervisor:child_spec()]}}.
init([]) ->
    Child = ?CHILD(ag_node_process, worker),
    {ok, {{one_for_one, 1, 1}, [Child]}}.

