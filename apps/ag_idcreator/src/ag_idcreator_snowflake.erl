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
-module(ag_idcreator_snowflake).

-behaviour(gen_server).

-include_lib("ag_base/include/agb_debuglogger.hrl").
%% API
-export([
    start_link/2,
    gen_newid/0,
    gen_batchid/1
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
    last_time :: integer(),
    sequence :: integer(),
    dc_id :: integer(),
    worker_id :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(gen_newid() ->
    binary()|term()).
gen_newid() ->
    gen_server:call(?MODULE, gen_new_id, ?CALL_TIMEOUT).

-spec(gen_batchid(integer()) ->
    list()).
gen_batchid(Count) ->
    gen_server:call(?MODULE, {gen_batch_id, Count}, ?CALL_TIMEOUT).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(integer(), integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(DcId, WorkerId) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [DcId, WorkerId], []).

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
init([DcId, WorkerId]) ->
    {ok, #state{last_time = snowflake_now_ms(), sequence = 0, dc_id = DcId, worker_id = WorkerId}}.

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
handle_call(gen_new_id, _From, State) ->
    {[Id], NewState} = generateId(1, State),
    {reply, Id, NewState};
handle_call({gen_batch_id, Count}, _From, State) ->
    {Ids, NewState} = generateId(Count, State),
    {reply, Ids, NewState};
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
snowflake_now_ms() ->
    os:system_time(1000) - 1552348800000.

generateId(Count, #state{sequence = Sequence, last_time = LastTime, dc_id = DcId, worker_id = WorkerId} = State) ->
    NowMs = snowflake_now_ms(),
    Sequence1 =
        if
            NowMs < LastTime ->
                agb_error:error("Clock moved backwards.Refusing to generate id for ~p milliseconds",
                    [LastTime - NowMs]);
            NowMs == LastTime ->
                Sequence;
            true ->
                0
        end,
    if
        Sequence1 + Count > 4096 ->
            ?LOG_INFO("The number of ids created in the same millisecond is greater than the maximum"),
            timer:sleep(0),
            generateId(Count, State);
        true ->
            BatchIds =
                lists:map(
                    fun(Num) ->
                        Index = Sequence1 + Num,
                        Index bor (WorkerId bsl 12) bor (DcId bsl 17) bor (NowMs bsl 22)
                    end,
                    lists:seq(0, Count - 1)
                ),
            {BatchIds, State#state{sequence = Sequence1 + Count, last_time = NowMs}}
    end.
