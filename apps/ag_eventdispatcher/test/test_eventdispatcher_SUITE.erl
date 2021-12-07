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
-module(test_eventdispatcher_SUITE).

%% API
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [test_eventdispatcher].

init_per_suite(_Config) ->
    logger:set_primary_config(level, all),
    application:start(ag_eventdispatcher),
    _Config.

end_per_suite(_Config) ->
    ok.

test_eventdispatcher(_Config) ->
    Modules = agb_behaviour:get_behaviour_modules(ag_eventdispatcher_acceptor),
    ct:log("test_eventdispatcher Modules:~p", [Modules]),
    ag_eventdispatcher_process:fire(ct, {text0, self()}),
    ag_eventdispatcher_process:fire(ct, {text1, self()}),
    ag_eventdispatcher_process:fire(ct, {text2, self()}),
    receive_msg().

receive_msg() ->
    receive
        {E, M} ->
            ct:log("~p recv ~p success ", [M, E]),
            receive_msg()
    after 5000 ->
        ok
    end.