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
-module(ag_idcreator_sup).

-behaviour(supervisor).

-include_lib("ag_base/include/agb_debuglogger.hrl").
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILDA(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(SERVER, ?MODULE).

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
    {ok, {{one_for_one, Intensity, Period}, ChildSpecs}} when
    Intensity :: non_neg_integer(),
    Period :: pos_integer(),
    ChildSpecs :: [supervisor:child_spec()].
init([]) ->
    Childs =
        lists:flatten(
            [
                get_id_rendchar(),
                get_id_accumulator(),
                get_snowflake(),
                get_id_ets()
            ]),
    {ok, {{one_for_one, 5, 10}, Childs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_id_rendchar() ->
    case application:get_env(id_randchar) of
        undefined ->
            [?CHILDA(ag_idcreator_randchar, worker, [0, 1])];
        {ok, [{gameid, GameId}, {bucket, BucketId}]} ->
            [?CHILDA(ag_idcreator_randchar, worker, [GameId, BucketId])]
    end.

get_id_accumulator() ->
    case application:get_env(id_accumulator) of
        undefined ->
            [];
        {ok, Param} ->
            Driver = proplists:get_value(driver, Param),
            Pool = proplists:get_value(pools, Param),
            Opts = proplists:get_value(option, Param),
            agdb_manager:add_pool(Pool, Driver, Opts),
            Bizs = proplists:get_value(biz, Param),
            case proplists:get_value(default, Bizs) of
                undefined ->
                    [?CHILDA(ag_idcreator_accumulator, worker, [Pool, [{default, 0} | Bizs]])];
                _ ->
                    [?CHILDA(ag_idcreator_accumulator, worker, [Pool, Bizs])]
            end
    end.

get_snowflake() ->
    case application:get_env(id_snowflake) of
        undefined ->
            [];
        {ok, [{dc_id, random}, {worker_id, random}]} ->
            DcId = rand:uniform(31),
            WorkerId = rand:uniform(31),
            [?CHILDA(ag_idcreator_snowflake, worker, [DcId, WorkerId])];
        {ok, [{dc_id, DcId}, {worker_id, WorkerId}]} ->
            if
                DcId > 31; WorkerId > 31 ->
                    [];
                true ->
                    [?CHILDA(ag_idcreator_snowflake, worker, [DcId, WorkerId])]
            end
    end.

get_id_ets() ->
    case application:get_env(id_ets) of
        undefined ->
            [?CHILDA(ag_idcreator_id_ets, worker, [1, 10000000, 1])];
        {ok, [{incr, Incr}, {threshold, Threshold}, {setvalue, SetValue}]} ->
            [?CHILDA(ag_idcreator_id_ets, worker, [Incr, Threshold, SetValue])]
    end.
