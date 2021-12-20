%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(test_meck_worker).

-behaviour(ag_engine_worker_module).

-include("ag_engine_core_defines.hrl").
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

quick_start_work(GatePid) ->
    Pid = spawn(fun start_loop/0),
    Pid ! {setGatePid, GatePid},
    receive
        ok ->
            ok
    end,
    Pid.

rpc_touch_me(_) ->
    ignore.

rpc_update_session_info(_, _) ->
    ignore.

rpc_update_gate_info(_) ->
    ignore.

rpc_message_handle(RoleWorkerPid, Module, Message) ->
    RoleWorkerPid ! {'MSG_HANDLE_REMOTE_MESSAGE', Module, Message}.

pop_all_msg(_) ->
    ct:pal("-------pop_all_msg---------"),
    MsgList =
    [
        #{?MESSAGE_NAME_KEY=>1},
        #{?MESSAGE_NAME_KEY=>2},
        #{?MESSAGE_NAME_KEY=>3},
        #{?MESSAGE_NAME_KEY=>4}
    ],
    {ok, MsgList}.

pop_msg() ->
    {error, undefined}.

start_loop() ->
    receive
        {setGatePid, GatePid} ->
            ag_engine_storage:set_gatepid(GatePid),
            GatePid ! ok,
            start_loop();
        {'MSG_HANDLE_REMOTE_MESSAGE', Module, Message} ->
            ct:pal("MSG_HANDLE_REMOTE_MESSAGE ~p~n", [{Module, Message}]),
            GPid = ag_engine_storage:get_gatepid(),
            ag_engine_msg_executer:async_apply(GPid, Module, Message),
            start_loop()
    after 5000 ->
        ok
    end.