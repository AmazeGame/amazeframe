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

-spec init() ->
    ok.
init() ->
    Modules = agb_behaviour:get_behaviour_modules(?MODULE),
    lists:foreach(
        fun(Module) ->
            Objects = Module:get_infos(),
            check(Objects),
            ag_engine_code_variable:puto(Objects)
        end,
        Modules
    ).


-spec pack_info(Message :: map()) ->
    map().
pack_info(#{?MESSAGE_ERROR_CODE_KEY:=Code} = Message) ->
    case ag_engine_code_variable:getv(Code) of
        undefined ->
            Message;
        Info ->
            Message#{?MESSAGE_ERROR_INFO_KEY=>Info}
    end;
pack_info(Message) ->
    Message.

check(Objects) ->
    lists:foreach(
        fun({Key, Value}) ->
            case ag_engine_code_variable:geto(Key) of
                undefined ->
                    ok;
                Object ->
                    ?LOG_WARNING("Code:~p has been input by ~p~n", [{Key, Value}, Object])
            end
        end,
        Objects
    ).
