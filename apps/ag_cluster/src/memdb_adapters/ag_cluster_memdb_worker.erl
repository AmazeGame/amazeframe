%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_cluster_memdb_worker).


-behaviour(gen_server).
-include("ag_cluster.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").
%% API
-export([start_link/0]).
-export([add_table/3]).
-export([master/0]).

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
-define(MSG_CHECK, 'CHECK NOW').
-define(ADD_TABLE, 'ADD_TABLE').

-record(state, {
    workmod :: ag_cluster_memdb_master_check | ag_cluster_memdb_slave_check,
    table_config = [] :: [tuple()]
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec add_table(Table :: atom(), Attributes :: list(), Indices :: list()) ->
    ok.
add_table(Table, Attributes, Indices) ->
    ?LOG_INFO("ag_cluster_memdb_worker add table:~p~n", [Table]),
    gen_server:cast(?MODULE, {?ADD_TABLE, Table, Attributes, Indices}).


-spec(master() -> true| node()).
master() ->
    {ok, Master} = application:get_env(master),
    if
        Master =:= node() ->
            true;
        true ->
            Master
    end.
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
    WorkModule =
        case master() of
            true ->
                ag_cluster_memdb_master_check;
            _ ->
                ag_cluster_memdb_slave_check
        end,
    {ok, #state{workmod = WorkModule, table_config = []}}.

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
handle_cast({?ADD_TABLE, Table, Attributes, Indices}, #state{table_config = TabConfig} = State) ->
    NewTabConfig =
        case lists:keyfind(Table, #table_config.table, TabConfig) of
            false ->
                TabConfig ++ [#table_config{table = Table, attribute = Attributes, index = Indices}];
            _OldTabCfg ->
                NewItem = #table_config{table = Table, attribute = Attributes, index = Indices},
                ?LOG_WARNING("~p ADD_TABLE warning : ~p is duplicate~n", [?MODULE, Table]),
                lists:keyreplace(Table, #table_config.table, TabConfig, NewItem)
        end,
    self() ! ?MSG_CHECK,
    {noreply, State#state{table_config = NewTabConfig}};
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
handle_info(?MSG_CHECK, #state{workmod = Module, table_config = TableConfig} = State) ->
    ?LOG_ERROR("handle_info MSG_CHECK Module:~p~n", [State]),
    case Module:check(TableConfig) of
        [] ->
            {noreply, State#state{table_config = []}};
        NewConfig when TableConfig == NewConfig ->
            erlang:send_after(1000, self(), ?MSG_CHECK), %%有错，需要等待
            {noreply, State#state{table_config = TableConfig}};
        NewConfig ->
            {noreply, State#state{table_config = NewConfig}}
    end;
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

