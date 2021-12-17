%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%% 对单个ip的单个消息的发送接收计数器，在单位之间内统计次数，发现次数超过阀值，打印警告信息
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
%%%
-module(ag_monitor_warning_write_handle).
-include("../../../include/ag_monitor_msg_tracking_struct.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").

-behaviour(gen_server).
-behaviour(ag_monitor_message_api).

-ifdef(TEST_CT).
-compile(export_all).
-endif.

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
-define(DO_ANALYZE, do_analyze).

-record(state, {
    biz_name :: atom(),                     %% bizlogger的bizname名称
    analyze_frequency :: integer(),         %% 分析记录信息的频率
    threshold :: integer(),                 %% 打印警告log的阀值
    record_message_map = #{} :: map()            %% 记录的消息信息
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec observe(map()) -> ok.
observe(Message) ->
    gen_server:cast(?MODULE, {record_message, Message}),
    ok.

-spec start(term()) -> ok.
start(Args) ->
    ag_monitor_sup:start_child(?MODULE, Args),
    ok.

-spec stop() -> ok.
stop() ->
    supervisor:terminate_child(ag_monitor_sup, ?MODULE),
    supervisor:delete_child(ag_monitor_sup, ?MODULE),
    ok.

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
init([#{bizlogger_bizname := BizName, frequency := Frequency, threshold := ThresholdValue}]) ->
    erlang:start_timer(Frequency * 1000, self(), ?DO_ANALYZE),
    {ok, #state{biz_name = BizName, analyze_frequency = Frequency * 1000, threshold = ThresholdValue}};
init([#{bizlogger_bizname := _, frequency := _} = Param]) ->
    init([Param#{threshold => 0}]);
init([#{bizlogger_bizname := _, threshold := _} = Param]) ->
    init([Param#{frequency => 60}]);
init([#{bizlogger_bizname := _} = Param]) ->
    init([Param#{frequency => 60, threshold => 0}]).

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
handle_cast({record_message, Message}, State) ->
    {noreply, on_record_message(Message, State)};
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
handle_info({timeout, _, ?DO_ANALYZE}, State) ->
    #state{
        analyze_frequency = Frequency,
        biz_name = BizName,
        record_message_map = Map,
        threshold = ThresholdValue
    } = State,
    erlang:start_timer(Frequency, self(), ?DO_ANALYZE),
    spawn(fun() -> do_analyze(Map, BizName, ThresholdValue) end),
    {noreply, State#state{record_message_map = #{}}};
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
on_record_message(
    #{
        ?TRACKING_MSG_TYPE := Type,
        ?TRACKING_REQUEST_NAME := InMsgName,
        ?TRACKING_RESPONSE_NAME := OutMsgName,
        ?TRACKING_USER_ID := UserId
    }, #state{record_message_map = RecordMessageMap} = State) when Type == async_request;Type == sync_request ->
    ClientMessageMap = maps:get(UserId, RecordMessageMap, #{}),
    ClientMessageMap1 = add_message_count(InMsgName, ClientMessageMap),
    ClientMessageMap2 = add_message_count(OutMsgName, ClientMessageMap1),
    State#state{record_message_map = RecordMessageMap#{UserId => ClientMessageMap2}};
on_record_message(
    #{
        ?TRACKING_MSG_TYPE := Type,
        ?TRACKING_REQUEST_NAME := InMsgName,
        ?TRACKING_USER_ID := UserId
    }, #state{record_message_map = RecordMessageMap} = State) when Type == async_request;Type == sync_request ->
    ClientMessageMap = maps:get(UserId, RecordMessageMap, #{}),
    ClientMessageMap1 = add_message_count(InMsgName, ClientMessageMap),
    State#state{record_message_map = RecordMessageMap#{UserId => ClientMessageMap1}};
on_record_message(
    #{
        ?TRACKING_MSG_TYPE := push,
        ?TRACKING_USER_ID := UserId,
        ?TRACKING_RESPONSE_NAME := OutMsgName
    },
    #state{record_message_map = RecordMessageMap} = State) ->
    ClientMessageMap = maps:get(UserId, RecordMessageMap, #{}),
    ClientMessageMap1 = add_message_count(OutMsgName, ClientMessageMap),
    State#state{record_message_map = RecordMessageMap#{UserId => ClientMessageMap1}}.

add_message_count(<<>>, MessageMap) ->
    MessageMap;
add_message_count(Name, MessageMap) ->
    NameBin = agb_convertor:to_binary(Name),
    case maps:get(NameBin, MessageMap, undefined) of
        undefined ->
            MessageMap#{NameBin => 1};
        Count ->
            MessageMap#{NameBin => Count + 1}
    end.

do_analyze(RecordMap, BizName, ThresholdValue) ->
    RecordMapList = maps:to_list(RecordMap),
    [do_analyze_client_recordmap(ClientMap, BizName, ThresholdValue) || ClientMap <- RecordMapList].

do_analyze_client_recordmap({UserId, ClientRecordMap}, BizName, ThresholdValue) ->
    ClientMapList = maps:to_list(ClientRecordMap),
    lists:foreach(
        fun({Name, Count}) ->
            if
                Count > ThresholdValue ->
                    WriteInfo =
                        #{
                            <<"userid">> => UserId,
                            <<"node">> => node(),
                            <<"time">> => get_format_date_time_bin(),
                            <<"messagename">> => Name,
                            <<"disposecount">> => Count
                        },
                    ag_bizlogger:write_log(BizName, WriteInfo);
                true ->
                    ignore
            end
        end,
        ClientMapList
    ).

get_format_date_time_bin() ->
    Format = agb_time:format_datetime("DD/Mon/YYYY:hh:mm:ss +0000", calendar:universal_time()),
    agb_convertor:to_binary(Format).