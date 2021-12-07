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
-module(ag_logger_disk_log_h).

-include_lib("kernel/include/logger.hrl").



%%% API
-export([filesync/1]).

%% logger_h_common callbacks
-export([init/2, check_config/4, reset_state/2,
    filesync/3, write/4, handle_info/3, terminate/3]).

%% logger callbacks
-export([log/2, adding_handler/1, removing_handler/1, changing_config/3,
    filter_config/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%%-----------------------------------------------------------------
%%%
-spec filesync(Name) -> ok | {error,Reason} when
    Name :: atom(),
    Reason :: handler_busy | {badarg,term()}.

filesync(Name) ->
    logger_h_common:filesync(?MODULE,Name).

%%%===================================================================
%%% logger callbacks
%%%===================================================================

%%%-----------------------------------------------------------------
%%% Handler being added
adding_handler(Config) ->
    %io:format("adding_handler ~p~n",[Config]),
    logger_h_common:adding_handler(Config).

%%%-----------------------------------------------------------------
%%% Updating handler config
changing_config(SetOrUpdate, OldConfig, NewConfig) ->
    %io:format("changing_config ~p~n",[{OldConfig, NewConfig}]),
    logger_h_common:changing_config(SetOrUpdate, OldConfig, NewConfig).

%%%-----------------------------------------------------------------
%%% Handler being removed
removing_handler(Config) ->
    logger_h_common:removing_handler(Config).

%%%-----------------------------------------------------------------
%%% Log a string or report
-spec log(LogEvent, Config) -> ok when
    LogEvent :: logger:log_event(),
    Config :: logger:handler_config().

log(LogEvent, Config) ->
    logger_h_common:log(LogEvent, Config).

%%%-----------------------------------------------------------------
%%% Remove internal fields from configuration
filter_config(Config) ->
    logger_h_common:filter_config(Config).

%%%===================================================================
%%% logger_h_common callbacks
%%%===================================================================
init(Name, Config = #{filenameformat:=FileNameFormat,split_file_rule:=SplitFileRule,outdir:=OutDir}) ->
    open_ag_logger_file_process(Name,Config),
    {ok,#{log_opts => #{
        filenameformat => FileNameFormat,
        split_file_rule => SplitFileRule,
        outdir => OutDir},
        prev_log_result => ok,
        prev_sync_result => ok,
        prev_disk_log_info => undefined}}.


check_config(Name,set,undefined,HConfig0) ->
%    io:format("check_config 001 ~p~n",[Name]),
    HConfig=merge_default_logopts(Name,maps:merge(get_default_config(),HConfig0)),
    check_config(HConfig);
check_config(_Name,SetOrUpdate,OldHConfig,NewHConfig0) ->
%    io:format("check_config 002 ~p~n",[OldHConfig]),
    WriteOnce = maps:with([],OldHConfig),
    Default =
        case SetOrUpdate of
            set ->
                %% Do not reset write-once fields to defaults
                maps:merge(get_default_config(),WriteOnce);
            update ->
                OldHConfig
        end,

    NewHConfig = maps:merge(Default,NewHConfig0),

    %% Fail if write-once fields are changed
    case maps:with([],NewHConfig) of
        WriteOnce ->
            check_config(NewHConfig);
        Other ->
            {Old,New} = logger_server:diff_maps(WriteOnce,Other),
            {error,{illegal_config_change,?MODULE,Old,New}}
    end.

check_config(HConfig) ->
    case check_h_config(maps:to_list(HConfig)) of
        ok ->
            {ok,HConfig};
        {error,{Key,Value}} ->
            {error,{invalid_config,?MODULE,#{Key=>Value}}}
    end.

check_h_config([{filenameformat,FileNameFormat}|Config]) when is_list(FileNameFormat) ->
    check_h_config(Config);
check_h_config([{split_file_rule,SFR}|Config]) when is_atom(SFR) ->
    check_h_config(Config);
check_h_config([{outdir,OutDir}|Config]) when is_list(OutDir) ->
    check_h_config(Config);
check_h_config([Other | _]) ->
    {error,Other};
check_h_config([]) ->
    ok.

get_default_config() ->
    #{}.

merge_default_logopts(_Name, HConfig) ->
    Defaults = #{filenameformat => "out/YYYY-MM-DD/YYY-DD-MM hh-mm-ss.txt",
                    split_file_rule => every_5min,
                    outdir => "./"},
    maps:merge(Defaults, HConfig).

filesync(Name,_Mode,State) ->
    maybe_notify_error(Name, filesync, ok, prev_sync_result, State).


write(Name, Mode, Bin, State) ->
    %io:format("--------write---------~p~n",[{Name, Mode, Bin}]),
    Result = disk_log_write(Name, Mode, Bin),
    maybe_notify_error(Name, log, Result, prev_log_result, State).


reset_state(_Name, State) ->
    State#{prev_log_result => ok,
        prev_sync_result => ok,
        prev_disk_log_info => undefined}.

%% The disk log owner must handle status messages from disk_log.
%%handle_info(Name, {disk_log, _Node, Log, Info={truncated,_NoLostItems}}, State) ->
%%    maybe_notify_status(Name, Log, Info, prev_disk_log_info, State);
%%handle_info(Name, {disk_log, _Node, Log, Info = {blocked_log,_Items}}, State) ->
%%    maybe_notify_status(Name, Log, Info, prev_disk_log_info, State);
%%handle_info(Name, {disk_log, _Node, Log, Info = full}, State) ->
%%    maybe_notify_status(Name, Log, Info, prev_disk_log_info, State);
%%handle_info(Name, {disk_log, _Node, Log, Info = {error_status,_Status}}, State) ->
%%    maybe_notify_status(Name, Log, Info, prev_disk_log_info, State);
handle_info(_, _, State) ->
    State.

terminate(Name, _Reason, _State) ->
    _ = close_disk_log(Name, normal),
    ok.

%%%-----------------------------------------------------------------
%%% Internal functions
close_disk_log(Name, _) ->
%%    _ = ?disk_log_sync(Name),
    disk_log_close(Name),
    ok.

disk_log_write(Name, Mode, Bin) ->
    ag_logger_file_process:write(Name,Mode,Bin).

disk_log_close(Name) ->
    ag_logger_file_process:close(Name).

%%%-----------------------------------------------------------------
%%% Print error messages, but don't repeat the same message
maybe_notify_error(Name, Op, Result, Key, #{log_opts:=LogOpts}=State) ->
    {Result,error_notify_new({Name, Op, LogOpts, Result}, Result, Key, State)}.

%%maybe_notify_status(Name, Log, Info, Key, State) ->
%%    error_notify_new({disk_log, Name, Log, Info}, Info, Key, State).

error_notify_new(Term, What, Key, State) ->
    error_notify_new(What, maps:get(Key,State), Term),
    State#{Key => What}.

error_notify_new(ok,_Prev,_Term) ->
    ok;
error_notify_new(Same,Same,_Term) ->
    ok;
error_notify_new(_New,_Prev,Term) ->
    logger_h_common:error_notify(Term).

open_ag_logger_file_process(Name,Config)->
    #{split_file_rule:=SplitFileRule, filenameformat:=FileNameFormat,outdir:=OutDir} = Config,
    %io:format("tsl_file_writer start ~p~n",[{SplitFileRule,FileNameFormat,OutDir,Name}]),
    OutDir1 = filename:absname_join(OutDir, atom_to_list(Name)),
    agb_file:ensure_dir(OutDir1),
    ChildSpec =
        {
            {ag_logger_file_process,Name},
            {ag_logger_file_process, start_link, [Name,FileNameFormat, SplitFileRule, OutDir1]},
            permanent,
            5000,
            worker,
            [ag_logger_file_process]
        },
    ag_logger_sup:start_child(ChildSpec),
    ok.