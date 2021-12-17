%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_gateway_module).

-include_lib("ag_base/include/agb_debuglogger.hrl").

-callback message_to_client(GatePid :: pid(), MsgObject :: term()) ->
    ok | {message_to_client, term()}.
-callback text_message_to_client(GatePid :: pid(), MsgObject :: term()) ->
    ok | {text_message_to_client, term()}.
-callback on_persist_loaded(GatePid :: pid()) ->
    ok | {on_persist_loaded, pid()}.
-callback send_async_handle_result(GatePid :: pid(), ReplyObject :: term(), Echo :: term()) ->
    ok.
-callback rpc_get_core_value(GatePid :: pid(), [ag_engine_storage:core_key()]) ->
    list().
%% API
-export([init/0]).
-export([
    message_to_gate_client/2,
    text_message_to_gate_client/2,
    on_persist_loaded_to_gate/1,
    send_async_handle_result/3
]).
-export([rpc_get_core_value/2]).

-define(KEY_GATEWAY_MODULE, gateway_module).

init() ->
    agb_ets:init(table()),
    scan_behaviour().

table() ->
    'ts_game_gateway_module'.

scan_behaviour() ->
    case agb_behaviour:get_behaviour_modules(?MODULE) of
        [] ->
            agb_ets:put(table(), {?KEY_GATEWAY_MODULE, undefined});
        [Mod | _] ->
            ?LOG_DEBUG("ag_engine_worker_module scan_behaviour:~p~n", [Mod]),
            agb_ets:put(table(), {?KEY_GATEWAY_MODULE, Mod})
    end.

worker_module() ->
    agb_ets:get(table(), ?KEY_GATEWAY_MODULE).

-spec message_to_gate_client(GatePid :: pid(), MsgObject :: term()) ->
    ok| {message_to_client, term()}.
message_to_gate_client(GatePid, MsgObject) ->
    Mod = worker_module(),
    Mod:message_to_client(GatePid, MsgObject).

-spec text_message_to_gate_client(GatePid :: pid(), MsgObject :: term()) ->
    ok| {message_to_client, term()}.
text_message_to_gate_client(GatePid, MsgObject) ->
    Mod = worker_module(),
    Mod:text_message_to_client(GatePid, MsgObject).

send_async_handle_result(GatePid, Echo, Result) ->
    Mod = worker_module(),
    Mod:send_async_handle_result(GatePid, Echo, Result).

on_persist_loaded_to_gate(GatePid) ->
    Mod = worker_module(),
    Mod:on_persist_loaded(GatePid).

rpc_get_core_value(GatePid, CoreKeyList) ->
    Mod = worker_module(),
    Mod:rpc_get_core_value(GatePid, CoreKeyList).