%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_msg_push_executer).

%% API
-export([
    pop_all_msg/2,
    pop_msg/2
]).
-export([
    message_to_client/3,
    text_message_to_client/3
]).

-spec pop_all_msg(list(), Pid :: pid()) ->
    [MsgObject :: map()].
pop_all_msg(_, RoleWorker) ->
    case catch ag_engine_worker_module:worker_pop_all_msg(RoleWorker) of
        {ok, Messages} ->
            Messages;
        ignore ->
            [];
        {'EXIT', _} ->
            []
    end.

-spec pop_msg(list(), Pid :: pid()) ->
    {error, undefined} | map() | [].
pop_msg(_, RoleWorker) ->
    case catch ag_engine_worker_module:worker_pop_msg(RoleWorker) of
        {ok, MsgObject} ->
            MsgObject;
        ignore ->
            [];
        {error, undefined} ->
            {error, undefined};
        {'EXIT', _} ->
            []
    end.

-spec message_to_client(list(), GatePid :: pid(), ReplyObject :: term()) ->
    ok.
message_to_client(_, GatePid, ReplyObject) ->
    ag_engine_gateway_module:message_to_gate_client(GatePid, ReplyObject),
    ok.

-spec text_message_to_client(list(), GatePid :: pid(), ReplyObject :: term()) ->
    ok.
text_message_to_client(_, GatePid, ReplyObject) ->
    ag_engine_gateway_module:text_message_to_gate_client(GatePid, ReplyObject),
    ok.