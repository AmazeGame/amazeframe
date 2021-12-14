%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------
-module(agb_pool_supervisor).
-author("Adrianx Lau <adrianx.lau@gmail.com>").
-include("agb_debuglogger.hrl").

-behaviour(supervisor).
-callback init(Args :: term()) ->
    {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}.

%% Supervisor callbacks
-export([init/1]).
-export([start_link/4]).
-export([
    start_child/3,
    stop_child/2
]).
-export([start_pool_woker/4]).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_child(PoolName :: atom(), HashTerm :: any(), Args :: list()) ->
    Result :: {error, string()} | {ok, pid(), pid()}.
start_child(PoolName, HashTerm, Args) ->
    EtsName = make_ets_name(PoolName),
    case ets:info(EtsName) of
        undefined ->
            {error, "supervisor pool havn't been started"};
        _ ->
            case ets:lookup(EtsName, count) of
                [] ->
                    {error, "supervisor pool havn't no any workers"};
                [{_, Count}] ->
                    case ets:lookup(EtsName, erlang:phash(HashTerm, Count)) of
                        [] ->
                            {error, "start child faild"};
                        [{_, SupervisorPid}] ->
                            {ok, ChildPid} = supervisor:start_child(SupervisorPid, Args),
                            {ok, SupervisorPid, ChildPid}
                    end
            end
    end.

-spec stop_child(SupervisorPid, ChildPid) ->
    Result when
    SupervisorPid :: supervisor:sup_ref(),
    ChildPid :: pid() | term(),
    Result :: 'ok' | {'error', Error},
    Error :: 'not_found' | 'simple_one_for_one'.
stop_child(SupervisorPid, ChildPid) ->
    supervisor:terminate_child(SupervisorPid, ChildPid).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------

-spec start_link(SupName :: supervisor:sup_name(), SupervisorMod :: module(), Count :: pos_integer(),
    PoolInitArgs :: list()
) ->
    supervisor:startlink_ret().
start_link({_, PoolName} = SupName, SupervisorMod, Count, PoolInitArgs) ->
    supervisor:start_link(SupName, ?MODULE, [PoolName, SupervisorMod, Count, PoolInitArgs]).

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
    {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}).
init([PoolName, SupervisorMod, Count, PoolInitArgs]) ->
    EtsName = make_ets_name(PoolName),
    ets:new(EtsName, [set, public, {read_concurrency, true}, named_table]),
    ets:insert(EtsName, {count, Count}),
    PoolWorkers = lists:map(
        fun(N) ->
            Args = [PoolName, SupervisorMod, N, PoolInitArgs],
            {N, {?MODULE, start_pool_woker, Args}, permanent, 5000, supervisor, [?MODULE]}
        end,
        lists:seq(1, Count)
    ),
    {ok, {{one_for_one, 5, 20}, PoolWorkers}};
init([pool_worker, PoolName, SupervisorMod, Index, PoolInitArgs]) -> %% for pool_worker init
    ets:insert(make_ets_name(PoolName), {Index, self()}),
    {ok, {{simple_one_for_one, _MaxIntensity, _Period} = SupFlags, ChildSpec}} = SupervisorMod:init(PoolInitArgs),
    {ok, {SupFlags, ChildSpec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec start_pool_woker(PoolName :: atom(), SupervisorMod :: module(), Index :: integer(), PoolInitArgs :: list()) ->
    supervisor:startlink_ret().
start_pool_woker(PoolName, SupervisorMod, Index, PoolInitArgs) ->
    supervisor:start_link(?MODULE, [pool_worker, PoolName, SupervisorMod, Index, PoolInitArgs]).

make_ets_name(PoolName) when is_atom(PoolName) ->
    binary_to_atom(iolist_to_binary(["$ets_", atom_to_list(PoolName)]), utf8);
make_ets_name(PoolName) when is_list(PoolName) ->
    binary_to_atom(iolist_to_binary(["$ets_", PoolName]), utf8);
make_ets_name(PoolName) when is_binary(PoolName) ->
    binary_to_atom(iolist_to_binary(["$ets_", PoolName]), utf8).
