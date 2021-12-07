%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.29
%%%-------------------------------------------------------------------
%%%
-module(ag_database_app).

-behaviour(application).

-export([start/2, start/0]).
-export([stop/0, stop/1]).
start(_StartType, _StartArgs) ->
    ag_database_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
-spec start() ->
    ok.
start() ->
    application:start(ag_database).

-spec stop() ->
    ok.
stop() ->
    application:stop(ag_database).