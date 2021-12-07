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
-module(ag_debuglogger_manager_SUITE).

-compile(export_all).
-include_lib("ag_base/include/agb_debuglogger.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [start_manager].


start_manager(_) ->
    agb_application:check_started(ag_debuglogger),
    timer:sleep(5000),
    ?LOG_INFO("test info"),
    ?LOG_ERROR("test error"),
    ?LOG_DEBUG("test debug").