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
-module(ag_idcreator_accumulator).

-behaviour(gen_server).

-include_lib("ag_base/include/agb_debuglogger.hrl").
%% API
-export([start_link/2]).

-export([
    gen_newid/1,
    gen_batchid/2
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
-define(CALL_TIMEOUT, 6000).

-record(state, {
    pool :: atom(),
    bizs :: list()
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(gen_newid(atom()) ->
    {ok, binary()}|term()).
gen_newid(Biz) ->
    gen_server:call(?MODULE, {gen_new_id, Biz}, ?CALL_TIMEOUT).

-spec(gen_batchid(atom(), integer()) ->
    list()).
gen_batchid(Biz, Count) ->
    gen_server:call(?MODULE, {gen_batch_id, Biz, Count}, ?CALL_TIMEOUT).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(atom(), list()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Pool, Bizs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Pool, Bizs], []).

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
init([Pool, Bizs]) ->
    init_bizs(Pool, Bizs),
    {ok, #state{pool = Pool, bizs = Bizs}}.

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
handle_call({gen_new_id, Biz}, _From, #state{pool = Pool, bizs = Bizs} = State) ->
    case proplists:get_value(Biz, Bizs) of
        undefined ->
            {reply, false, State};
        _ ->
            {_, Id} = agdb_cached_adapter:incr(Pool, build_key(Biz)),
            {reply, Id, State}
    end;
handle_call({gen_batch_id, Biz, Count}, _From, #state{pool = Pool, bizs = Bizs} = State) ->
    case proplists:get_value(Biz, Bizs) of
        undefined ->
            {reply, false, State};
        _ ->
            Command = [["INCR", build_key(Biz)] || _ <- lists:duplicate(Count, 1)],
            Result = agdb_cached_adapter:qp(Pool, Command),
            Ids = lists:map(fun({ok, Id}) ->
                agb_convertor:to_integer(Id) end, Result),
            {reply, Ids, State}
    end;
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
build_key(Key) ->
    <<(agb_convertor:to_binary(id_accumulator))/binary, $:, (agb_convertor:to_binary(Key))/binary>>.

init_bizs(Pool, Bizs) ->
    Fun =
        fun({Biz, InitValue}) ->
            KeyName = build_key(Biz),
            case agdb_cached_adapter:exists(Pool, KeyName) of
                {ok, not_exist} ->
                    agdb_cached_adapter:set(Pool, KeyName, InitValue);
                {error, Reason} ->
                    agb_error:error(Reason);
                _ ->
                    nothing
            end
        end,
    lists:foreach(Fun, Bizs).
