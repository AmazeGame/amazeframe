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
-module(ag_debuglogger_app).

-behaviour(application).
-include_lib("ag_base/include/agb_debuglogger.hrl").
%% Application callbacks
-export([
    start/0, start/2,
    stop/1
]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(StartType :: application:start_type(), StartArgs :: term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    ag_debuglogger_sup:start_link().

-spec(start() ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start() ->
    application:start(ag_debuglogger).

-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.


