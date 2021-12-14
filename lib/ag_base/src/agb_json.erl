%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------

-module(agb_json).
-export([
    encode/1,
    decode2map/1,
    decode2list/1
]).

-define(JSON_CODEC,jsx).


-spec encode(Data::term()) -> binary().
encode(Data) ->
    case ?JSON_CODEC:encode(Data) of
        [_ | _] = BinList ->
            binary:list_to_bin(BinList);
        Bin ->
            Bin
    end.

-spec decode2map(Data::binary()) -> map().
decode2map(Data) ->
    ?JSON_CODEC:decode(Data, [return_maps]).

-spec decode2list(Data::binary()) -> list().
decode2list(Data) ->
    ?JSON_CODEC:decode(Data).
