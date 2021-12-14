%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------


-module(agb_hex).
-export([
    bin_to_hexstr/1,
    hexstr_to_bin/1,
    bin_to_hexlstr/1,
    bin_to_hexustr/1,
    bin_to_hexlbin/1,
    bin_to_hexubin/1
]).

-spec bin_to_hexstr(Bin :: binary()) ->
    list().
bin_to_hexstr(Bin) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) ||
        X <- binary_to_list(Bin)]).

-spec hexstr_to_bin(S :: string()) ->
    binary().
hexstr_to_bin(S) ->
    hexstr_to_bin(S, []).

hexstr_to_bin([], Acc) ->
    list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X, Y | T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [X, Y]),
    hexstr_to_bin(T, [V | Acc]);
hexstr_to_bin([X | T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", lists:flatten([X, "0"])),
    hexstr_to_bin(T, [V | Acc]).

-spec bin_to_hexustr(Bin :: binary()) ->
    list().
bin_to_hexustr(Bin) ->
    bin_to_hexstr(Bin).

-spec bin_to_hexlstr(Bin :: binary()) ->
    list().
bin_to_hexlstr(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).

-spec bin_to_hexlbin(Bin :: binary()) ->
    binary().
bin_to_hexlbin(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).

-spec bin_to_hexubin(Bin :: binary()) ->
    binary().
bin_to_hexubin(Bin) ->
    iolist_to_binary([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).
