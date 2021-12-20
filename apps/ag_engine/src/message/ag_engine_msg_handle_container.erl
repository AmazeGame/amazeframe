%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_msg_handle_container).

-include("ag_engine_code_defines.hrl").
-include("ag_engine_core_defines.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").
-record(msg_handler_table, {msg_name :: binary(), msg_handler :: atom(), roleworker_handle :: boolean()}).
%% API
-export([init/0]).
-export([add_handler/2]).
-export([remove_handler/1]).
-export([lookup_handler/1]).

-spec init() ->
    ok.
init() ->
    ?LOG_DEBUG("ag_engine_msg_handle_container init"),
    scan_behaviour().

scan_behaviour() ->
    Mods = agb_behaviour:get_behaviour_modules(ag_engine_message_handler),
    lists:foreach(
        fun(Mod) ->
            case erlang:function_exported(Mod, get_messages, 0) of
                false ->
                    ignore;
                true ->
                    case Mod:get_messages() of
                        {GateMsgNames, RoleWorkerMsgNames} ->
                            lists:foreach(
                                fun(MsgName) ->
                                    add_handler(MsgName, Mod)
                                end,
                                GateMsgNames
                            ),
                            lists:foreach(
                                fun(MsgName) ->
                                    add_handler(MsgName, Mod, true)
                                end,
                                RoleWorkerMsgNames
                            );
                        GateMsgNames ->
                            lists:foreach(
                                fun(MsgName) ->
                                    add_handler(MsgName, Mod)
                                end,
                                GateMsgNames
                            )
                    end
            end
        end,
        Mods
    ).

lookup_handler(MsgName) when is_list(MsgName) ->
    lookup_handler(list_to_binary(MsgName));
lookup_handler(MsgName) ->
    case ag_engine_msg_variable:geto(MsgName) of
        [] ->
            undefined;
        [#msg_handler_table{msg_handler = Handler, roleworker_handle = false}] ->
            Handler;
        [#msg_handler_table{msg_handler = Handler, roleworker_handle = true}] ->
            {Handler, true}
    end.

-spec add_handler(MsgName :: string() | binary(), Handler :: module()) ->
    ok.
add_handler(MsgName, Handler) when is_list(MsgName) ->
    add_handler(list_to_binary(MsgName), Handler);
add_handler(MsgName, Handler) ->
    Object = #msg_handler_table{msg_name = MsgName, msg_handler = Handler, roleworker_handle = false},
    ag_engine_msg_variable:put(Object),
    ok.

-spec add_handler(MsgName :: string() | binary(), Handler :: module(), ShadowHandle :: boolean()) ->
    ok.
add_handler(MsgName, Handler, ShadowHandle) when is_list(MsgName) ->
    add_handler(list_to_binary(MsgName), Handler, ShadowHandle);
add_handler(MsgName, Handler, ShadowHandle) ->
    Object = #msg_handler_table{msg_name = MsgName, msg_handler = Handler, roleworker_handle = ShadowHandle},
    ag_engine_msg_variable:put(Object),
    ok.

-spec remove_handler(MsgName :: string() | binary()) ->
    ok.
remove_handler(MsgName) when is_list(MsgName) ->
    remove_handler(list_to_binary(MsgName));
remove_handler(MsgName) ->
    ag_engine_msg_variable:delete(MsgName).