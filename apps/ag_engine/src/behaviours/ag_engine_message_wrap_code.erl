%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_message_wrap_code).

-include("ag_engine_core_defines.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").

%% API
-callback get_infos() -> [{integer(), binary()}].

-export([
    init/0,
    pack_info/1
]).

-define(ETS_CODENAME, '$$ets_code_name').

-spec init() ->
    ok.
init() ->
    agb_ets:init(table()),
    Modules = agb_behaviour:get_behaviour_modules(?MODULE),
    lists:foreach(
        fun(Module) ->
            Objects = Module:get_infos(),
            check(Objects),
            agb_ets:put(table(), Objects)
        end,
        Modules
    ).

table() ->
    ?ETS_CODENAME.

-spec pack_info(Message :: map()) ->
    map().
pack_info(#{?MESSAGE_ERROR_CODE_KEY:=Code} = Message) ->
    case agb_ets:lookup(table(), Code) of
        [] ->
            Message;
        {_, Info} ->
            Message#{?MESSAGE_ERROR_INFO_KEY=>Info}
    end;
pack_info(Message) ->
    Message.

check(Objects) ->
    lists:foreach(
        fun({Key, Value}) ->
            case agb_ets:lookup(table(), Key) of
                [] ->
                    ok;
                Object ->
                    ?LOG_WARNING("Code:~p has been input by ~p~n", [{Key, Value}, Object])
            end
        end,
        Objects
    ).
