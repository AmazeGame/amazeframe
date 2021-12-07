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
-module(ag_config_app).

-behaviour(application).

%% Application callbacks
-export([
    start/0, start/2,
    stop/1
]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(StartType :: application:start_type(), StartArgs :: term()) ->
    {ok, pid()}.
start(_StartType, _StartArgs) ->
    ag_config_sup:start_link().

-spec stop(State :: term()) ->
    ok.
stop(_State) ->
    ok.

-spec start() ->
    'ok' | {'error', Reason}
    when Reason :: term().
start() ->
    application:start(ag_config).