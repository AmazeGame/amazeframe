%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.28
%%%-------------------------------------------------------------------
%%%

-module(ag_logger_manager).

-include("ag_logger.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([write_log/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).



-define(SERVER, ?MODULE).
-define(INIT_LOGGER,<<"initlogger">>).

-record(state, {
    loggerState = #{} :: map()
}).

%%%===================================================================
%%% API
%%%===================================================================
%% 写入bilog和oplog,log内容为map，map中的字符串要为binary
-spec(write_log(LogType :: (?LOG_TYPE_OP | ?LOG_TYPE_BI ),Log :: map()) -> ok).
write_log(LogType,Log)  when LogType == ?LOG_TYPE_OP ->
    logger:info(Log,#{logtype=>LogType});
write_log(LogType,Log)  when LogType == ?LOG_TYPE_BI ->
    NewLog = maps:merge( Log,#{
                                appid => ag_logger_variable:get(appid),
                                hostname => ag_logger_variable:get(hostname),
                                date => agb_convertor:to_binary( agb_time:format_datetime( ag_logger_variable:get(dateformat) , calendar:universal_time()))} ),
    logger:info(NewLog,#{logtype=>LogType}).

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
    ag_logger_variable:init(),
    logger:set_primary_config(level,all),
    case init_config() of
        ok ->
            self() ! ?INIT_LOGGER,
            {ok,#state{}};
        _->
            ignore
    end.

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

handle_info(?INIT_LOGGER,State) ->
    case init_logger(State) of
        {ok,NewState} ->
            {noreply,NewState};
        {error,Reason}->
            {stop,Reason}
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

init_config()->
    case  application:get_env(param) of
        undefined ->
            ?LOG_INFO("init_logger gen_env config undefined"),
            error;
        {ok,Config}->
            lists:foreach(
                fun({K,V}) ->
                    ag_logger_variable:put(K,V)
                end,maps:to_list(Config)),
            ok
    end.

init_logger(State)->
    case application:get_env(logger) of
        undefined ->
            ?LOG_INFO("init_logger gen_env logger undefined"),
            {error, "loggerconfigerror"};
        {'ok', ConfigList} ->
            Fun =
                fun({HandlerId,Module,Config},Map)->
                    case
                        logger:add_handler(HandlerId,Module,Config) of
                        ok->
                            Map#{HandlerId=>{Module,Config}};
                        {error,Error} ->
                            ?LOG_INFO("logger:add_handler error ~p~n",[{HandlerId,Module,Error}]),
                            Map
                    end
                end,
            LoggerState = lists:foldl( Fun,#{},ConfigList ),
            logger:remove_handler(default),
            {ok,State#state{loggerState = LoggerState}}
    end.

