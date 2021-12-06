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
-module(ag_bizlogger_app).
-behaviour(application).


%% Application callbacks
-export([
    start/0, start/2,
    stop/1
]).

-export([write_log/2]).
%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    case ag_bizlogger_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

-spec start() -> 'ok' | {'error', Reason} when
    Reason :: term().
start() ->
    application:start(ag_bizlogger).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_header_info(Biz) ->
    {ok, BizList} = application:get_env(ag_bizlogger, biz_list),
    case agb_maplist:keyfind(Biz, biz, BizList) of
        #{header := {Module, Args}} ->
            {Module, Args};
        _ ->
            null
    end.

get_logger_handle(Biz) ->
    {ok, BizList} = application:get_env(ag_bizlogger, biz_list),
    case agb_maplist:keyfind(Biz, biz, BizList) of
        #{handle := LoggerHandle} ->
            LoggerHandle;
        _ ->
            agb_error:error("bizlogger biz:[~p] not loggerhandle", [Biz])
    end.

write(Biz, Pid, Log) when is_map(Log) ->
    LoggerHandle = get_logger_handle(Biz),
    ProcessSize = get_write_process_size(Biz),
    WriteIndex = erlang:phash(Pid, ProcessSize),
    InfoBin = agb_json:encode(Log),
    ProcessName = apply(LoggerHandle,make_process_name,[Biz,WriteIndex]),
    case is_list(InfoBin) of
        true ->
            LoggerHandle:write(ProcessName, [InfoBin, "\n"]);
        false ->
            LineFeed = <<"\n">>,
            LoggerHandle:write(ProcessName, <<InfoBin/binary, LineFeed/binary>>)
    end;
write(Biz, Pid, InfoBin) when is_binary(InfoBin) ->
    LoggerHandle = get_logger_handle(Biz),
    ProcessSize = get_write_process_size(Biz),
    WriteIndex = erlang:phash(Pid, ProcessSize),
    ProcessName = apply(LoggerHandle,make_process_name,[Biz,WriteIndex]),
    LineFeed = <<"\n">>,
    LoggerHandle:write(ProcessName, <<InfoBin/binary, LineFeed/binary>>);
write(Biz, Pid, InfoBin) when is_list(InfoBin) ->
    LoggerHandle = get_logger_handle(Biz),
    ProcessSize = get_write_process_size(Biz),
    WriteIndex = erlang:phash(Pid, ProcessSize),
    ProcessName = apply(LoggerHandle,make_process_name,[Biz,WriteIndex]),
    LoggerHandle:write(ProcessName, [InfoBin, "\n"]).

get_write_process_size(BizName) ->
    {ok, BizList} = application:get_env(ag_bizlogger, biz_list),
    case agb_maplist:keyfind(BizName, biz, BizList) of
        false ->
            agb_error:error("not found logwriter ~s~n", [BizName]);
        #{process_num := Num} ->
            Num
    end.
%%%===================================================================
%%% API
%%%===================================================================
-spec write_log(Biz :: atom(), Log :: map()|string()|binary()) ->
    ok.
write_log(Biz, Log) ->
    case get_header_info(Biz) of
        null ->
            write(Biz, self(), Log);
        {Module, Args} ->
            Header = apply(Module, header, Args),
            NewLog = apply(Module, do_combine_header_opt, [Header, Log]),
            write(Biz, self(), NewLog)
    end.


