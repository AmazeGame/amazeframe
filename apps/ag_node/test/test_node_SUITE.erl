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
-module(test_node_SUITE).


%% API
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
%%        {group, master},
%%        {group, slave}
    ].

groups() ->
    [
        {slave, [], [test_slave]},
        {master, [], [test_master]}
    ].

init_per_suite(_Config) ->
    _Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, _Config) ->
    _Config.
end_per_testcase(_, _Config) ->
    ok.

init_per_group(slave, _Config) ->
    %% 启动master节点 5秒后自动退出
    agb_os:cmd_async("erl -name gamemaster@127.0.0.1 -setcookie ag_node_cookie -eval \"timer:sleep(5000)\" -s init stop"),
    net_kernel:start(['ag_node@127.0.0.1', longnames]),
    application:ensure_all_started(ag_node);
init_per_group(master, _Config) ->
    %% 启动master节点 5秒后自动退出
    net_kernel:start(['gamemaster@127.0.0.1', longnames]),
    application:ensure_all_started(ag_node).
end_per_group(_, _Config) ->
    net_kernel:stop(),
    application:stop(ag_node),
    ok.