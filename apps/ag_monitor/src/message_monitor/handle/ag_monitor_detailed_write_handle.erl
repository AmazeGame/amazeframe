%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
%%%
-module(ag_monitor_detailed_write_handle).

-include("../../../include/ag_monitor_msg_tracking_struct.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").

-behaviour(gen_server).
-behaviour(ag_monitor_message_api).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    observe/1,
    start/1,
    stop/0
]).

-define(SERVER, ?MODULE).
-define(WRITE_LOG, writelog).

-record(state, {
    biz_name :: atom()
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start(term()) -> ok.
start(Args) ->
    ag_monitor_sup:start_child(?MODULE, Args),
    ok.

-spec stop() -> ok.
stop() ->
    supervisor:terminate_child(ag_monitor_sup, ?MODULE),
    supervisor:delete_child(ag_monitor_sup, ?MODULE),
    ok.

-spec observe(map()) -> ok.
observe(
    #{
        ?TRACKING_MSG_TYPE := Type,
        ?TRACKING_REQUEST_NAME := InName
    } = Metrics) when Type == async_request;Type == sync_request ->
%%    ?LOG_DEBUG("ag_monitor_detailed_write_handle observe ~p~n",Metrics),
    IsRecord =
        case ag_monitor_config:get({detailed_message_write, unmatchlist}) of
            undefined ->
                true;
            {_, UnMatchList} ->
                case unmatch_msg_name(InName, UnMatchList) of
                    false ->
                        true;
                    _ ->
                        false
                end
        end,
    if
        IsRecord ->
            Metrics0 = dispatch_metrics(Metrics),
            Metrics1 = normalization_message(Metrics0),
            gen_server:cast(?MODULE, {?WRITE_LOG, agb_json:encode(Metrics1)});
        true ->
            ok
    end;
observe(
    #{
        ?TRACKING_MSG_TYPE := push
    } = Metrics) ->
%%    ?LOG_DEBUG("ag_monitor_detailed_write_handle observe push ~p~n",Metrics),
    Metrics0 = dispatch_metrics(Metrics),
    Metrics1 = normalization_message(Metrics0),
    gen_server:cast(?MODULE, {?WRITE_LOG, agb_json:encode(Metrics1)}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).

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
init([#{bizlogger_bizname := BizName, unmatchlist := UnMatchList, binarymaxbytesize := MaxSize}]) ->
    ag_monitor_config:put({detailed_message_write, unmatchlist}, UnMatchList),
    ag_monitor_config:put({detailed_message_write, binarymaxbytesize}, MaxSize),
    {ok, #state{biz_name = BizName}};
init([#{bizlogger_bizname := _, unmatchlist := _} = Param]) ->
    init([Param#{binarymaxbytesize => 2048}]);
init([#{bizlogger_bizname := _, binarymaxbytesize := _} = Param]) ->
    init([Param#{unmatchlist => []}]);
init([#{bizlogger_bizname := _} = Param]) ->
    init([Param#{unmatchlist => [], binarymaxbytesize => 2048}]).


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
handle_cast({?WRITE_LOG, Metrics}, #state{biz_name = BizName} = State) ->
    ag_bizlogger:write_log(BizName, Metrics),
    {noreply, State};
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
    State :: #state{}) -> term()).
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
unmatch_msg_name(Name, UnMatchList) ->
    lists:member(Name, UnMatchList).

dispatch_metrics(#{?TRACKING_MSG_TYPE := sync_request, ?TRACKING_STATUS := fin} = Metrics) ->
    #{?TRACKING_REQ_START := ReqStart, ?TRACKING_REQ_END := ReqEnd} = Metrics,
    Metrics0 = remove_invalid_data(Metrics),
    Metrics0#{
        ?TRACKING_REQ_TIME => get_tracking_req_or_exec_time(ReqEnd, ReqStart),
        ?TRACKING_EXEC_TIME => get_tracking_req_or_exec_time(ReqEnd, ReqStart)
    };
dispatch_metrics(#{?TRACKING_MSG_TYPE := sync_request} = Metrics) ->
    remove_invalid_data(Metrics);
dispatch_metrics(#{?TRACKING_MSG_TYPE := async_request, ?TRACKING_STATUS := fin} = Metrics) ->
    #{
        ?TRACKING_REQ_START := ReqStart,
        ?TRACKING_REQ_END := ReqEnd,
        ?TRACKING_EXEC_START := ExecStart,
        ?TRACKING_EXEC_END := ExecEnd
    } = Metrics,
    Metrics0 = remove_invalid_data(Metrics),
    Metrics0#{
        ?TRACKING_REQ_TIME => get_tracking_req_or_exec_time(ReqEnd, ReqStart),
        ?TRACKING_EXEC_TIME => get_tracking_req_or_exec_time(ExecEnd, ExecStart)
    };
dispatch_metrics(#{?TRACKING_MSG_TYPE := async_request} = Metrics) ->
    remove_invalid_data(Metrics);
dispatch_metrics(#{?TRACKING_MSG_TYPE := push} = Metrics) ->
    Metrics.

get_tracking_req_or_exec_time(EndTime, StartTime) ->
    EndTimeUnit = erlang:convert_time_unit(EndTime, native, millisecond),
    StartTimeUnit = erlang:convert_time_unit(StartTime, native, millisecond),
    EndTimeUnit - StartTimeUnit.

normalization_message(Metrics) ->
    Metrics0 = remove_invalid_data(Metrics),
    {_, BinaryMaxSize} = ag_monitor_config:get({detailed_message_write, binarymaxbytesize}),
    maps:map(
        fun
            (_, V) when is_map(V) ->
                normalization_message(V);
            (_, V) when is_binary(V) ->
                BinarySize = byte_size(V),
                if
                    BinarySize > BinaryMaxSize -> <<"More than max size">>;
                    true -> V
                end;
            (_, V) ->
                V
        end,
        Metrics0
    ).

remove_invalid_data(Metrics) ->
    InvalidKeys = [?TRACKING_REQ_START, ?TRACKING_REQ_END, ?TRACKING_EXEC_START, ?TRACKING_EXEC_END],
    lists:foldl(
        fun(Key, Map) ->
            maps:remove(Key, Map)
        end,
        Metrics,
        InvalidKeys
    ).