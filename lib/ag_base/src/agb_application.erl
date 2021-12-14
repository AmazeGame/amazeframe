%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------
-module(agb_application).
-include("agb_debuglogger.hrl").

%% API
-export([
    check_started/1,
    config_file/1,
    apply_application_env/2,
    init_app_config/1
]).

%% @doc check the application is started
-spec check_started(Application :: atom()) ->
    ok | {error, Reason :: term()}.
check_started(undefined) ->
    ok;
check_started(Application) ->
    case application:ensure_all_started(Application) of
        {ok, _} ->
            ok;
        E ->
            {error,E}
    end.

-spec config_file(File :: file:name_all()) ->
    ok | {error, term()}.
config_file(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            init_app_config(Terms);
        {error, Reason} ->
            ?LOG_ERROR("agb_application load config file:~p error:~p", [File, Reason]),
            {error, Reason}
    end.

-spec init_app_config(Config :: list() | tuple()) ->
    ok.
init_app_config(ConfigList) when is_list(ConfigList) ->
    lists:foreach(
        fun({AppName, Params}) ->
            apply_application_env(AppName, Params)
        end,
        ConfigList
    );
init_app_config(Config) when is_tuple(Config) ->
    {AppName, Params} = Config,
    apply_application_env(AppName, Params).

-spec apply_application_env(AppName :: atom(), Params :: list()) ->
    ok.
apply_application_env(_, [{config, SubConfig}]) ->
    config_file(filename:absname(SubConfig));
apply_application_env(AppName, Params) ->
    lists:foreach(
        fun({K, V}) ->
            application:set_env([{AppName, [{K, V}]}])
        end,
        Params
    ).
