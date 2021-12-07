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
-module(ag_debuglogger_manager).

-include_lib("ag_base/include/agb_debuglogger.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

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


-record(state, {
    loggerState = #{} :: map()
}).

%%%===================================================================
%%% API
%%%===================================================================


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
    {ok, LoggerState} = init_logger(),
    {ok, #state{loggerState = LoggerState}}.

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
init_logger() ->
    LoggerState =
        case init_handler_config() of
            [] ->
                #{};
            Loggers ->
                logger:set_primary_config(level, all),
                logger:remove_handler(default),
                lists:foldl(
                    fun({HandlerId, Module, Config}, Map) ->
                        ok = logger:add_handler(HandlerId, Module, Config),
                        Map#{HandlerId => {Module, Config}}
                    end,
                    #{},
                    Loggers
                )
        end,
    {ok, LoggerState}.


init_handler_config() ->
    {ok, ConsoleConfig} = application:get_env(ag_debuglogger, console),
    {ok, FileConfig} = application:get_env(ag_debuglogger, file),
    {ok, LoggerConfig} = application:get_env(ag_debuglogger, logger_config),
    Filters = application:get_env(ag_debuglogger, filters, []),
    Formatter =
        application:get_env(ag_debuglogger, formatter,
            {logger_formatter, #{legacy_header => true, single_line => false}}),
    init_handler_config(console, ConsoleConfig, LoggerConfig, Filters, Formatter) ++
    init_handler_config(file, FileConfig, LoggerConfig, Filters, Formatter).

init_handler_config(console, Config, LoggerConfig, Filters, Formatter) ->
    Open = maps:get(open, Config, true),
    Level = maps:get(level, Config, debug),
    UseFormatter =
        case maps:get(formatter, Config, undefined) of
            undefined ->
                Formatter;
            SF ->
                SF
        end,
    case Open of
        true ->
            [
                {console, logger_std_h, #{
                    config => LoggerConfig,
                    level => Level,
                    filters => Filters,
                    formatter => UseFormatter}
                }
            ];
        false ->
            []
    end;
init_handler_config(file, Config, LoggerConfig, Filters, Formatter) ->
    Open = maps:get(open, Config, true),
    Level = maps:get(level, Config, debug),
    LogFolder = maps:get(log_folder, Config, "logs/"),
    MaxNoFile = maps:get(max_no_files, Config, 10),
    MaxNoBytes = maps:get(max_no_bytes, Config, 20485760),
    LevelList = get_log_level_list(Level),
    LoggerConfig1 = maps:merge(
        LoggerConfig,
        #{
            type => wrap,
            max_no_files => MaxNoFile,
            max_no_bytes => MaxNoBytes
        }
    ),
    UseFormatter =
        case maps:get(formatter, Config, undefined) of
            undefined ->
                Formatter;
            SF ->
                SF
        end,
    case Open of
        true ->
            lists:map(
                fun(L) ->
                    LB = atom_to_binary(L, utf8),
                    LogName = binary_to_list(<<LB/binary, ".log">>),
                    {L, logger_disk_log_h, #{
                        config => maps:merge(LoggerConfig1, #{file => filename:join(LogFolder, LogName)}),
                        level => L,
                        filters => Filters,
                        formatter => UseFormatter}
                    }
                end, LevelList
            );
        false ->
            []
    end.

get_log_level_list(debug) ->
    [error, warning, info, debug];
get_log_level_list(info) ->
    [error, warning, info];
get_log_level_list(warning) ->
    [error, warning];
get_log_level_list(error) ->
    [error];
get_log_level_list(_) ->
    [debug].