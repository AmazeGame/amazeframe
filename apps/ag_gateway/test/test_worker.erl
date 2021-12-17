%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(test_worker).

-behaviour(ag_engine_worker_module).

-include_lib("ag_engine/include/ag_engine_core_defines.hrl").
%% API
-compile(export_all).


%%-------------------------------------ag_engine_worker_module------------------------------------------------------
create_start_worker(RWContext, GatePid, GateNode) ->
    #{?INTERNAL_DEFINE_ID:=Id, ?INTERNAL_DEFINE_ID_TYPE:=IdType} = RWContext,
    Pid = spawn(fun start_loop/0),
    Security = "12345678",
    Archive = "22345678",
    Session = <<"12345678">>,
    ok = ag_engine_cluster:register_player(Id, IdType, Pid, node(), GatePid, GateNode, Session, Security, Archive),
    {Pid, Session, Security, Archive}.

exist_start_worker(RWContext, GatePid, GateNode) ->
    #{?INTERNAL_DEFINE_ID:=Id, ?INTERNAL_DEFINE_ID_TYPE:=IdType} = RWContext,
    Pid = spawn(fun start_loop/0),
    Security = "12345678",
    Archive = "22345678",
    Session = <<"12345678">>,
    ok = ag_engine_cluster:register_player(Id, IdType, Pid, node(), GatePid, GateNode, Session, Security, Archive),
    {Pid, Session, Security, Archive}.

rpc_touch_me(_) ->
    ignore.

rpc_update_session_info(_, _) ->
    ignore.

rpc_update_gate_info(_) ->
    ignore.

rpc_message_handle(_, _, _, _) ->
    ignore.

pop_all_msg(_) ->
    {ok,[]}.

pop_msg() ->
    {ok,[]}.

start_loop() ->
    receive
    after 5000 ->
        ok
    end.