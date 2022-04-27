%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_worker_module).


-include("ag_engine_core_defines.hrl").
-include("ag_engine_code_defines.hrl").
-include("ag_engine.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").
%% API
-export([init/0]).
-export_type([
    user_info/0,
    create_work_info/0
]).
-export([
    worker_touch_me/1,
    worker_update_session_info/2,
    worker_update_gate_info/1,
    worker_message_handle/3,
    worker_pop_msg/1,
    worker_pop_all_msg/1,
    worker_create_start/3,
    worker_exist_start/3
]).

-type user_info() :: map().
-type create_work_info() :: {pid(), binary(), binary() | string() | integer(), binary() | string() | integer()}.

-callback create_start_worker(user_info(), pid(), node()) ->
    false | tuple() | ignore.
-callback exist_start_worker(user_info(), pid(), node()) ->
    false | tuple() | ignore.
-callback rpc_touch_me(undefined | pid()) ->
    ignore | ok.
-callback rpc_update_session_info(undefined | pid(), string()) ->
    ignore | ok.
-callback rpc_update_gate_info(undefined | pid()) ->
    ignore | ok.
-callback rpc_message_handle(undefined | pid(), module(), map()) ->
    ok | term() | ignore.
-callback pop_all_msg(Pid :: pid()) ->
    {ok, [MsgObject :: map()]} | ignore.
-callback pop_msg(Pid :: pid()) ->
    {error, undefined} | {ok, MsgObject :: map()} | ignore.

-define(KEY_WORKER_MODULE, worker_module).

init() ->
    scan_behaviour().

scan_behaviour() ->
    case application:get_env(ag_engine, is_use_roleworker) of
        {ok,true} ->
            case agb_behaviour:get_behaviour_modules(?MODULE) of
                [] ->
                    agb_error:error("unfound roleworker module");
                [Mod | _] ->
                    ?LOG_DEBUG("ag_engine_worker_module scan_behaviour:~p~n", [Mod]),
                    ag_engine_variable:put(?KEY_WORKER_MODULE, Mod)
            end;
        _ ->
            ag_engine_variable:put(?KEY_WORKER_MODULE, undefined)
    end.

worker_module() ->
    ag_engine_variable:getv(?KEY_WORKER_MODULE).

-spec worker_create_start(user_info(), pid(), node()) ->
    false | tuple() | ignore.
worker_create_start(Message, GatePid, GateNode) ->
    execute_worker_call(worker_module(), create_start_worker, [Message, GatePid, GateNode]).

-spec worker_exist_start(user_info(), pid(), node()) ->
    false | tuple() | ignore.
worker_exist_start(Message, GatePid, GateNode) ->
    execute_worker_call(worker_module(), exist_start_worker, [Message, GatePid, GateNode]).

-spec worker_touch_me(undefined | pid()) ->
    ignore | ok.
worker_touch_me(Pid) ->
    execute_worker_call(worker_module(), rpc_touch_me, [Pid]).

-spec worker_update_session_info(undefined | pid(), string()) ->
    ignore | ok.
worker_update_session_info(Pid, String) ->
    execute_worker_call(worker_module(), rpc_update_session_info, [Pid, String]).

-spec worker_update_gate_info(undefined | pid()) ->
    ignore | ok.
worker_update_gate_info(Pid) ->
    execute_worker_call(worker_module(), rpc_update_gate_info, [Pid]).

-spec worker_message_handle(undefined | pid(), module(), map()) ->
    ok | term() | ignore.
worker_message_handle(Pid, Mod, Message) ->
    execute_worker_call(worker_module(), rpc_message_handle, [Pid, Mod, Message]).

-spec worker_pop_msg(Pid :: pid()) ->
    {error, undefined} | {ok, MsgObject :: map()} | ignore.
worker_pop_msg(Pid) ->
    execute_worker_call(worker_module(), pop_msg, [Pid]).

-spec worker_pop_all_msg(Pid :: pid()) ->
    {ok, [MsgObject :: map()]} | ignore.
worker_pop_all_msg(Pid) ->
    execute_worker_call(worker_module(), pop_all_msg, [Pid]).

execute_worker_call(undefined, _, _) ->
    ignore;
execute_worker_call(Mod, Fun, Args) ->
    apply(Mod, Fun, Args).