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
-module(ag_debuglogger_filter_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [filter_test].

filter_test(_) ->
    ?assertEqual(#{level=> error}, ag_debuglogger_filter:dev_log(#{level=> error}, [])),
    ?assertEqual(ignore, ag_debuglogger_filter:dev_log(#{level=> debug}, [])),
    ?assertEqual(
        #{level=> debug, meta=>#{logtype=>logtype_develop}},
        ag_debuglogger_filter:dev_log(#{level=> debug, meta=>#{logtype=>logtype_develop}}, [])),
    ?assertEqual(
        stop,
        ag_debuglogger_filter:dev_log(#{level=> debug, meta=>#{logtype=>log}}, [])),
    ?assertEqual(
        stop,
        ag_debuglogger_filter:dev_log(#{meta=>#{}}, [])),
    ?assertEqual(ignore, ag_debuglogger_filter:dev_log(#{}, [])),
    ok.