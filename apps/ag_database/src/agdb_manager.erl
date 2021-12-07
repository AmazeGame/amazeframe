%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.29
%%%-------------------------------------------------------------------
%%%
-module(agdb_manager).
-behaviour(gen_server).
-include_lib("ag_base/include/agb_debuglogger.hrl").
%% API
-export([start_link/0]).
-export([
    add_pool/3,
    add_cached_pool/3
]).
-export_type([driver_args/0,driver/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-type driver() :: redis | redis_cluster | mysql | mongodb.
-type driver_args() :: {atom(), atom(), tuple()}.
-record(state, {}).


-define(SERVER, ?MODULE).
-define(MSG_INIT_POOLS, init_pools).
-define(MSG_ADD_POOL, add_pool).
-define(MSG_ADD_CACHEDPOOL, add_cachedpool).
%%%===================================================================
%%% API
%%%===================================================================

-spec add_pool(DriverPoolName :: atom(), Driver :: mysql|mongodb|redis, Opts :: tuple()) ->
    ok.
add_pool(DriverPoolName, Driver, Opts) ->
    gen_server:call(?MODULE, {?MSG_ADD_POOL, DriverPoolName, Driver, Opts}, infinity).

-spec add_cached_pool(
    DriverPoolName :: atom(),
    DBTuple :: {atom(), mysql|mongodb, tuple()},
    CacheTuple :: {atom(), redis, tuple()}
) ->
    ok.
add_cached_pool(DriverPoolName, DBTuple, CacheTuple) ->
    gen_server:call(?MODULE, {?MSG_ADD_CACHEDPOOL, DriverPoolName, DBTuple, CacheTuple}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    ?LOG_INFO("agdb_manager init"),
    self() ! ?MSG_INIT_POOLS,
    {ok, #state{}}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({?MSG_ADD_POOL, DriverPoolName, Driver, Opts}, _From, State) ->
    add_pool_info(DriverPoolName, Driver, Opts),
    {reply, ok, State};
handle_call({?MSG_ADD_CACHEDPOOL, DriverPoolName, DB, Cached}, _From, State) ->
    add_cached_pool_info(DriverPoolName, DB, Cached),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(?MSG_INIT_POOLS, State) ->
    do_init_pools(),
    ag_eventdispatcher_process:fire(ag_database, #{name=><<"initialized">>}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) ->
    term()).
terminate(Reason, _State) ->
    ?LOG_ERROR("ag_database agdb_manager terminate Reason:~p~n", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_init_pools() ->
    case application:get_env(driver_pools) of
        undefined ->
            ignore;
        {ok, PoolNames} ->
            lists:foreach(
                fun(PoolName) ->
                    case application:get_env(PoolName) of
                        undefined ->
                            ignore;
                        {ok, PoolNameInfo} ->
                            init_pool_info(PoolName, PoolNameInfo)
                    end
                end,
                PoolNames
            )
    end.

init_pool_info(PoolName, {Driver, OptName}) when Driver == mysql; Driver == mongodb ->
    {ok, Opts} = application:get_env(OptName),
    {ok, _} = agdb_database_adapter:init(Driver, PoolName, Opts);
init_pool_info(PoolName, {Driver, OptName}) when Driver == redis;Driver == redis_cluster ->
    {ok, Opts} = application:get_env(OptName),
    {ok, _} = agdb_cached_adapter:init(Driver, PoolName, Opts);
init_pool_info(PoolName, {DBPoolName, CachedPoolName}) ->
    {ok, {DBDrive, DBDriveOptName}} = application:get_env(DBPoolName),
    {ok, {CachedDrive, CachedDriveOptName}} = application:get_env(CachedPoolName),
    {ok, DBDriveOpts} = application:get_env(DBDriveOptName),
    {ok, CachedDriveOpts} = application:get_env(CachedDriveOptName),
    DBTuple = {DBPoolName, DBDrive, DBDriveOpts},
    CacheTuple = {CachedPoolName, CachedDrive, CachedDriveOpts},
    ok = agdb_database_cached_adapter:init(PoolName, DBTuple, CacheTuple).

add_pool_info(PoolName, Driver, Opts) when Driver == mysql; Driver == mongodb ->
    {ok, _} = agdb_database_adapter:init(Driver, PoolName, Opts);
add_pool_info(PoolName, Driver, Opts) when Driver == redis; Driver == redis_cluster ->
    {ok, _} = agdb_cached_adapter:init(Driver, PoolName, Opts).

add_cached_pool_info(PoolName, DBTuple, CacheTuple) ->
    ok = agdb_database_cached_adapter:init(PoolName, DBTuple, CacheTuple).
