-module(ag_engine_msg_variable).
-behaviour(agb_variable).
-export([table/0]).
-include_lib("ag_base/include/agb_variable.hrl").
-record(msg_handler_table, {msg_name :: binary(), msg_handler :: atom(), roleworker_handle :: boolean()}).
table() ->
    {'AMAZEGAME_ENGINE_MESSAGE_VARIABLE',[{keypos, #msg_handler_table.msg_name}]}.
