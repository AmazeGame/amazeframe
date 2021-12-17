%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_message_handler).

-type reply() :: []|<<>>|map().

-callback get_messages() ->
    [binary() | integer()] | {[binary() | integer()], [binary() | integer()]}.
-callback handle(Message :: map()) ->
    {true, Reply :: reply()} | {false, Reason :: map()}.

-include("ag_engine_code_defines.hrl").
-include("ag_engine_core_defines.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").
-export([init/0]).

init() ->
    ag_engine_msg_handle_container:init().


