%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_gateway_app).

-behaviour(application).
-behaviour(ag_engine_gateway_module).

-include_lib("ag_base/include/agb_debuglogger.hrl").
%% Application callbacks
-export([
    start/2,
    stop/1
]).

%% API exports
-export([on_persist_loaded/1]).
-export([on_bad_request/1]).
-export([text_message_to_client/2]).
-export([message_to_client/2]).
-export([send_async_handle_result/3]).
-export([rpc_get_core_value/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(StartType :: application:start_type(), StartArgs :: term()) ->
    {ok, pid()}.
start(_StartType, _StartArgs) ->
    ok = agb_application:check_started(crypto),
    ok = agb_application:check_started(asn1),
    ok = agb_application:check_started(public_key),
    ok = agb_application:check_started(ssl),
    ok = agb_application:check_started(inets),
    ok = agb_application:check_started(ranch),
    ok = agb_application:check_started(cowlib),
    ok = agb_application:check_started(cowboy),
    ag_gateway_sup:start_link().

-spec(stop(_State :: term()) ->
    ok).
stop(_State) ->
    ok.

-spec on_persist_loaded(GatePid :: pid()) ->
    ok| {on_persist_loaded, pid()}.
on_persist_loaded(undefined) ->
    ok;
on_persist_loaded(GatePid) ->
    GatePid ! {on_persist_loaded, self()}.

-spec on_bad_request(GatePid :: pid()) ->
    ok| {stop, bad_request}.
on_bad_request(GatePid) ->
    GatePid ! {stop, bad_request}.

-spec message_to_client(GatePid :: pid(), ReplyObject :: term()) ->
    ok| {message_to_client, term()}.
message_to_client(undefined, _ReplyObject) ->
    ok;
message_to_client(GatePid, ReplyObject) ->
    GatePid ! {message_to_client, ReplyObject}.

-spec text_message_to_client(GatePid :: pid(), ReplyObject :: term()) ->
    ok | {text_message_to_client, term()}.
text_message_to_client(undefined, _ReplyObject) ->
    ok;
text_message_to_client(GatePid, ReplyObject) ->
    GatePid ! {text_message_to_client, ReplyObject}.

-spec send_async_handle_result(pid(), map(), map()) ->
    ok.
send_async_handle_result(undefined, _, _) ->
    ok;
send_async_handle_result(GatePid, Result, Echo) ->
    GatePid ! {async_handle_result, Result, Echo},
    ok.

-spec rpc_get_core_value(pid(), [ag_engine_storage:core_key()]) ->
    list().
rpc_get_core_value(GatePid, StorageKeyList) when GatePid == self() ->
    ag_engine_storage:get_core_info_by_keys(StorageKeyList);
rpc_get_core_value(GatePid, StorageKeyList) ->
    GatePid ! {get_core_value, StorageKeyList, self()},
    receive
        {core_value, Value} ->
            Value
    after 2000 ->
        ?LOG_ERROR("rpc_get_core_value timeout"),
        []
    end.