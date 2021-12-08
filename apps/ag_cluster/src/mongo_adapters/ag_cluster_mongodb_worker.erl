%%%-------------------------------------------------------------------
%%% @author ayongbc <ayongbc@sina.com> 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_cluster_mongodb_worker).

-behaviour(gen_server).
-include("ag_cluster.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").
%% API
-export([start_link/0]).
-export([
    add_table/4,
    get_table/1,
    check_table/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-define(TABLE_ETS, 'CLUSTER_TABLE_ETS').

-record(state, {table_config = [] :: [tuple()]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec add_table(Table :: atom(), Attributes :: list(), Indices :: list(),
    TTL :: non_neg_integer()
) ->
    ok.
add_table(Table, Attributes, Indices, TTL) ->
    agb_ets:put(?TABLE_ETS, #table_config{table = Table, attribute = Attributes, index = Indices, ttl = TTL}),
    ok.

-spec get_table(Table :: atom()) ->
    tuple().
get_table(TableName) ->
    case agb_ets:lookup(?TABLE_ETS, TableName) of
        [] ->
            notfound;
        Object ->
            Object
    end.

-spec check_table(TableNames :: [atom()]) ->
    boolean().
check_table(TableNames) ->
    lists:all(
        fun(TableName) ->
            case agb_ets:lookup(?TABLE_ETS, TableName) of
                [] ->
                    false;
                _ ->
                    true
            end
        end,
        TableNames
    ).

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
    ?LOG_DEBUG("ag_cluster_redis_worker init"),
    agb_ets:init(?TABLE_ETS, [{keypos, #table_config.table}]),
    init_mongodb_pool(),
    {ok, #state{table_config = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}
) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
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
    State :: #state{}
) ->
    term()).
terminate(_Reason, _State) ->
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

init_mongodb_pool() ->
    case application:get_env(ag_cluster_mongodb_adapter) of
        undefined ->
            ?LOG_ERROR("ag_cluster_redis_worker init_redis_pool error notfound config"),
            throw({?MODULE, "notfound config"});
        {ok, Config} ->
            Pool = proplists:get_value(pools, Config),
            ag_cluster_variable:put(mongodb_pool, Pool),
            Opts = proplists:get_value(option, Config),
            agdb_manager:add_pool(Pool, mongodb, Opts)
    end.

