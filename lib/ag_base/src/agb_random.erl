%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------
-module(agb_random).

%% API
-export([rand_by_weight/1]).

-spec rand_by_weight(List :: list()) ->
    tuple() | undefined.
rand_by_weight([]) ->
    undefined;
rand_by_weight(ValueWeights) ->
    TotalWeight = total_weight(ValueWeights),
    RandValue = rand:uniform(TotalWeight),
    {K, _} = rand_by_weight(RandValue, ValueWeights, 0),
    K.

total_weight(List) ->
    lists:foldl(
        fun({_Key, Value}, Acc) ->
            Acc + Value
        end,
        0,
        List
    ).

rand_by_weight(_RandValue, [], _CompareValue) ->
    undefined;
rand_by_weight(RandValue, [{Value, Weight} | T], CompareValue) ->
    NewCompareValue = Weight + CompareValue,
    case RandValue =< NewCompareValue of
        true ->
            {Value, Weight};
        false ->
            rand_by_weight(RandValue, T, NewCompareValue)
    end.
