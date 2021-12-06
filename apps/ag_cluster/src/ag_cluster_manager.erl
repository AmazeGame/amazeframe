%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_cluster_manager).

-include_lib("ag_base/include/agb_debuglogger.hrl").

%% API
-export([start_link/0]).
-export([check_table/1]).
-export([size/1]).
-export([add_table/4]).
-export([
    write/1,
    write_dirty/1,
    write_single/2
]).
-export([
    read/1, read/2,
    read_dirty/2,
    index_read/3
]).
-export([
    update/3,
    update_dirty/3,
    update_counter_dirty/3
]).
-export([
    delete/2,
    delete_dirty/2,
    delete_index/3,
    delete_object/1
]).

%%-export([update_expire/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(INIT, init).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec add_table(Table :: atom(), Attributes :: list(), Indices :: list(), TTL :: integer()) ->
    ok.
add_table(Table, Attributes, Indices, TTL) ->
    Adapter = adapter(),
    Adapter:add_table(Table, Attributes, Indices, TTL).

-spec(check_table(Table :: [atom()]) ->
    boolean()).
check_table(Table) ->
    Adapter = adapter(),
    Adapter:check_table(Table).

-spec size(atom()) ->
    non_neg_integer().
size(Table) ->
    Adapter = adapter(),
    Adapter:size(Table).

-spec write(Object :: tuple()) ->
    ok | {failed, Reason :: term()}.
write(Object) ->
    Adapter = adapter(),
    Adapter:write(Object).

%% 写入唯一的对象，同时写入时，先写入的有效
-spec write_single(Object :: tuple(), Condition :: atom() | tuple()) ->
    ok | {failed, Reason :: term()}.
write_single(Object, Condition) ->
    Adapter = adapter(),
    Adapter:write_single(Object, Condition).
%%-spec write_notrans(Object::tuple())-> ok .
%%write_notrans(Object) ->
%%    Adapter = adapter(),
%%    Adapter:write_notrans(Object).

-spec write_dirty(Object :: tuple()) ->
    ok.
write_dirty(Object) ->
    Adapter = adapter(),
    Adapter:write_dirty(Object).

-spec read(atom()) ->
    {ok, [tuple()]} | {failed, Reason :: term()}.
read(Table) ->
    Adapter = adapter(),
    Adapter:read(Table).

-spec read(atom(), Key :: term()) ->
    {ok, [tuple()]} | {failed, Reason :: term()}.
read(Table, Key) ->
    Adapter = adapter(),
    Adapter:read(Table, Key).

%%-spec read_notrans(atom(),Key::term())-> {ok,[]} | {ok,tuple()}.
%%read_notrans(Table, Key) ->
%%    Adapter = adapter(),
%%    Adapter:read_notrans(Table,Key).

-spec read_dirty(atom(), Key :: term()) ->
    {ok, [tuple()]}.
read_dirty(Table, Key) ->
    Adapter = adapter(),
    Adapter:read_dirty(Table, Key).

-spec index_read(atom(), SecondaryKey :: term(), Pos :: non_neg_integer()) ->
    {ok, []} | {ok, tuple()} | {ok, [tuple()]} | {failed, Reason :: term()}.
index_read(Table, SecondaryKey, Pos) ->
    Adapter = adapter(),
    Adapter:index_read(Table, SecondaryKey, Pos).

-spec update(Table :: atom(), TableKey :: term(), UpdateList :: [tuple()]) ->
    ok | {failed, Reason :: term()}.
update(Table, TableKey, UpdateList) ->
    Adapter = adapter(),
    Adapter:update(Table, TableKey, UpdateList).

-spec update_dirty(Table :: atom(), TableKey :: term(), UpdateList :: [tuple()]) ->
    ok | error.
update_dirty(Table, TableKey, UpdateList) ->
    Adapter = adapter(),
    Adapter:update_dirty(Table, TableKey, UpdateList).

-spec update_counter_dirty(Table :: atom(), Key :: term(), Incr :: integer()) ->
    NewVal :: integer().
update_counter_dirty(Table, Key, Incr) ->
    Adapter = adapter(),
    Adapter:update_counter_dirty(Table, Key, Incr).

-spec delete(Table :: atom(), Key :: term()) ->
    ok | {failed, Reason :: term()}.
delete(Table, Key) ->
    Adapter = adapter(),
    Adapter:delete(Table, Key).

-spec delete_dirty(Tab :: atom(), Key :: term()) ->
    'ok'.
delete_dirty(Table, Key) ->
    Adapter = adapter(),
    Adapter:delete_dirty(Table, Key).

-spec delete_object(Object :: tuple()) ->
    ok | {failed, Reason :: term()}.
delete_object(Object) ->
    Adapter = adapter(),
    Adapter:delete_object(Object).

-spec delete_index(atom(), SecondaryKey :: term(), Pos :: non_neg_integer()) ->
    ok | {failed, Reason :: term()}.
delete_index(Table, SecondaryKey, Pos) ->
    Adapter = adapter(),
    Adapter:delete_index(Table, SecondaryKey, Pos).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    ?LOG_DEBUG("ag_cluster_memdb_worker start_link"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    ag_cluster_config:init(),
    %self() ! ?INIT,
    {ok, #state{}}.

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
    Extra :: term()
) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

adapter() ->
    case ag_cluster_config:get(adapter) of
        undefined ->
            throw(bad_ag_cluster_config);
        {_, Adapter} ->
            Adapter
    end.

