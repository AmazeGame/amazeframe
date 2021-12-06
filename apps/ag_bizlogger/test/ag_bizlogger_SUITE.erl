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
-module(ag_bizlogger_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [write].


init_per_suite(Config) ->
    agb_application:check_started(ag_bizlogger),
    Config.

end_per_suite(_Config) ->

    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

write(_) ->
    ag_bizlogger:write_log(bi, #{<<"key">> => test}),
    ag_bizlogger:write_log(bi, #{<<"key">> => test, <<"app_id">> => <<"1">>, <<"host_name">> => <<"2">>, <<"date">> => <<"DD/Mon/YYYY:hh:mm:ss +0000">>}),

    ag_bizlogger:write_log(op, #{<<"key">> => test}),
    ag_bizlogger:write_log(op, "log is string"),
    ag_bizlogger:write_log(op, <<"log is binary">>),
    ?assertException(error, _, ag_bizlogger:write_log(error, #{<<"key">> => test})).

