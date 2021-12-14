%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------
-module(agb_argument).
-export([get/1, get/0]).
-spec get() ->
    [{Flag :: atom(), Values :: [string()]}].
get() ->
    init:get_arguments().

-spec get(Flag :: atom()) ->
    error | {ok, [Values :: [string()]]}.
get(Flag) when is_atom(Flag) ->
    init:get_argument(Flag);
get(Flag) when is_list(Flag) ->
    AFlag = list_to_atom(Flag),
    ?MODULE:get(AFlag);
get(Flag) when is_binary(Flag) ->
    AFlag = binary_to_atom(Flag, latin1),
    ?MODULE:get(AFlag).
