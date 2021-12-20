%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(test_gateway).


-behaviour(ag_engine_gateway_module).
%% API
-compile(export_all).


message_to_client(GatePid, Msg) ->
    GatePid ! {message_to_client, Msg}.

text_message_to_client(GatePid, Msg) ->
    GatePid ! {text_message_to_client, Msg}.

on_persist_loaded(_) ->
    ok.

send_async_handle_result(undefined, _, _) ->
    ok;
send_async_handle_result(GatePid, Result, Echo) ->
    ct:pal("send_async_handle_result GatePid:[~p] Result:~p Echo:~p~n", [GatePid, Result, Echo]),
    GatePid ! {async_handle_result, Result, Echo}.

rpc_get_core_value(_GatePid,_) ->
    [{client_ip,<<"127.0.0.1">>},{id,<<"adbcadr">>}].