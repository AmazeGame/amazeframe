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
-module(ag_idcreator_randchar).

-behaviour(gen_server).
-include_lib("ag_base/include/agb_debuglogger.hrl").

%% API
-export([start_link/2]).
-export([
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
-record(state, {index = 0 :: integer(), gameid = 0 :: integer(), bucket = 0 :: integer()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec(gen_newid() ->
    binary() | term()).
gen_newid() ->
    case whereis(?SERVER) of
        undefined ->
            error("application gen_idcreator hasn't be started");
        Pid ->
            gen_server:call(Pid, newid)
    end.

-spec(gen_batchid(Count :: integer()) ->
    list() | term()).
gen_batchid(Count) ->
    case whereis(?SERVER) of
        undefined ->
            error("application gen_idcreator hasn't be started");
        Pid ->
            gen_server:call(Pid, {batchid, Count})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(integer(), integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(GameId, Bucket) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [GameId, Bucket], []).

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
init([GameID, BucketIndex]) ->
    rand:seed(exs1024s, erlang:timestamp()),
    ?LOG_INFO("ag_idcreator_process init gameid:~p,bucket:~p", [GameID, BucketIndex]),
    {ok, #state{index = 0, gameid = GameID, bucket = BucketIndex}}.

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
handle_call({batchid, Count}, _From, #state{index = Index, gameid = GameId, bucket = Bucket} = State) ->
    Reply = lists:map(fun(I) ->
        gen_newid(Index + I, GameId, Bucket) end, lists:seq(0, Count - 1)),
    {reply, Reply, State#state{index = Index + Count}};
handle_call(newid, _From, #state{index = Index, gameid = GameId, bucket = Bucket} = State) ->
    Reply = gen_newid(Index, GameId, Bucket),
    {reply, Reply, State#state{index = Index + 1}};
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
gen_newid(Index, GameId, Bucket) ->
    {{Y, M, D}, {H, Min, S}} = erlang:universaltime(),
    Y_M_D = Y * 372 + M * 31 + D,
    OutIoList =
        [
            rand_char(), rand_char(),
            get_chars(S, 1),
            rand_char(), rand_char(),
            get_chars(GameId, 4),
            rand_char(), rand_char(),
            get_chars(Min, 1),
            rand_char(), rand_char(),
            get_chars(Bucket, 4),
            rand_char(), rand_char(),
            get_chars(Index, 4),
            rand_char(), rand_char(),
            get_chars(H, 1),
            rand_char(), rand_char(),
            get_chars(Y_M_D, 6),
            rand_char(), rand_char()
        ],
    iolist_to_binary(OutIoList).

get_char(N) when (N >= 1) and (N =< 26) ->
    N - 1 + $A;
get_char(N) when (N >= 27) and (N =< 52) ->
    N - 27 + $a;
get_char(N) when (N >= 53) and (N =< 62) ->
    N - 53 + $0;
get_char(N) when N == 63 ->
    $$;
get_char(N) when N == 64 ->
    $#;
get_char(N) when N == 0 ->
    $=.

rand_char() ->
    get_char(rand:uniform(64)).

get_chars(N, L) ->
    Chars = get_pure_chars(N),
    NewChars = lists:reverse([get_char(C) || C <- Chars]),
    Len = length(NewChars),
    if
        Len >= L ->
            NewChars;
        true ->
            Sub = L - Len,
            [[rand_char() || _X <- lists:seq(1, Sub)], NewChars]
    end.

get_pure_chars(N) when N >= 65 ->
    [N rem 64 | get_pure_chars(N div 64)];
get_pure_chars(N) when (N >= 0) and (N =< 64) ->
    [N].
