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
-module(ag_node_app).


-behaviour(application).

-include("ag_node.hrl").
%% Application callbacks
-export([
    start/0, start/2,
    stop/1,
    master/0,
    register_node_state_change_event/1,
    unregister_node_state_change_event/1
]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec(start(StartType :: term(), StartArgs :: term()) ->
    {ok, Pid :: pid()} | {error, Error :: term()}).
start(_StartType, _StartArgs) ->
    {ok,DataDir} = application:get_env(ag_node,'ra_data_dir'),  %%获取设置ra数据参数
    ra:start([{data_dir,DataDir}]),                             %%ra的启动必须使用这种方式
    ag_node_cluster:init(),
    case ag_node_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

-spec(stop(_State :: term()) -> ok).
stop(_State) ->
    ok.

-spec(start() -> 'ok' | {'error', term()}).
start() ->
    application:start(ag_node).

-spec(master() -> true| node()).
master() ->
    ag_node_ra:master().

-spec register_node_state_change_event(pid()) -> ok.
register_node_state_change_event(AcceptPid) ->
    ag_node_process:register_state_change_event(AcceptPid).

-spec unregister_node_state_change_event(pid()) -> ok.
unregister_node_state_change_event(AcceptPid) ->
    ag_node_process:unregister_state_change_event(AcceptPid).
%%%===================================================================
%%% Internal functions
%%%===================================================================

