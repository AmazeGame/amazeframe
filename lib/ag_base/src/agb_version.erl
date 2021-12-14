%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.13
%%%-------------------------------------------------------------------
-module(agb_version).

%% API
-export([compare_version/2]).

-spec compare_version(V1 :: string() | binary(), V2 :: string() | binary()) ->
    0 | 1 | -1.
compare_version(V1, V2) when is_list(V1) ->
    compare_version(list_to_binary(V1), V2);
compare_version(V1, V2) when is_list(V2) ->
    compare_version(V1, list_to_binary(V2));
compare_version(V1, V2) when is_binary(V1) andalso is_binary(V2) ->
    V1Numbers = lists:map(fun(S) -> binary_to_integer(S) end, binary:split(V1, <<".">>, [global])),
    V2Numbers = lists:map(fun(S) -> binary_to_integer(S) end, binary:split(V2, <<".">>, [global])),
    if
        V1Numbers =:= V2Numbers ->
            0;
        V1Numbers > V2Numbers ->
            1;
        true ->
            -1
    end.
