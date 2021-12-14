%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------
-module(agb_error).


-export([error/1, error/2]).

-spec error(Reason :: term()) ->
    no_return().
error(String) ->
    erlang:error(String).

-spec error(Format, Args) ->
    no_return() when
    Format :: term(),
    Args :: [term()].
error(Format, Args) ->
    String = lists:flatten(io_lib:format(Format, Args)),
    erlang:error(String).
