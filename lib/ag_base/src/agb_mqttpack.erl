%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.13
%%%-------------------------------------------------------------------
-module(agb_mqttpack).


%% API
-export([
    pack/1,
    unpack/1
]).

-spec pack(LenOrBinary :: non_neg_integer() | binary()) ->
    binary().
pack(Len) when is_integer(Len) ->
    do_pack(Len);
pack(Binary) when is_binary(Binary) ->
    Len = erlang:size(Binary),
    LenBinary = do_pack(Len),
    <<LenBinary/binary, Binary/binary>>.

do_pack(Len) when Len >= 0 andalso Len =< 16#7F ->
    <<Len>>;
do_pack(Len) when Len >= 16#80 andalso Len =< 16#3FFF ->
    C0 = Len rem 16#80,
    C1 = Len div 16#80,
    <<1:1, C0:7, C1>>;
do_pack(Len) when Len >= 16#4000 andalso Len =< 16#1FFFFF ->
    C0 = Len rem 16#80,
    C1 = (Len band 16#3FFF) bsr 7,
    C2 = Len div 16#4000,
    <<1:1, C0:7, 1:1, C1:7, C2>>;
do_pack(Len) when Len >= 16#200000 andalso Len =< 16#FFFFFFF ->
    C0 = Len rem 16#80,
    C1 = (Len band 16#3FFF) bsr 7,
    C2 = (Len band 16#1FFFFF) bsr 14,
    C3 = Len div 16#200000,
    <<1:1, C0:7, 1:1, C1:7, 1:1, C2:7, C3>>;
do_pack(_) ->
    error(cannot_support).

-spec unpack(Binary :: binary()) ->
    {ok, integer(), Payload :: binary()} |
    {ok, integer(), Payload :: binary(), LeftBinary :: binary()} |
    {partial, integer(), Payload :: binary()}.
unpack(<<0:1, C:7, Payload/binary>>) ->
    UPLen = C,
    do_unpack(UPLen, Payload);
unpack(<<1:1, C0:7, 0:1, C1:7, Payload/binary>>) ->
    UPLen = C0 + C1 * 128,
    do_unpack(UPLen, Payload);
unpack(<<1:1, C0:7, 1:1, C1:7, 0:1, C2:7, Payload/binary>>) ->
    UPLen = C0 + C1 * 128 + C2 * 128 * 128,
    do_unpack(UPLen, Payload);
unpack(<<1:1, C0:7, 1:1, C1:7, 1:1, C2:7, 0:1, C3:7, Payload/binary>>) ->
    UPLen = C0 + C1 * 128 + C2 * 128 * 128 + C3 * 128 * 128 * 128,
    do_unpack(UPLen, Payload);
unpack(_) ->
    error(cannot_support).

do_unpack(UPLen, Payload) ->
    PayLen = erlang:size(Payload),
    if
        PayLen == UPLen ->
            {ok, UPLen, Payload};
        PayLen > UPLen ->
            {ok, UPLen, binary:part(Payload, 0, UPLen), binary:part(Payload, UPLen, PayLen - UPLen)};
        true ->
            {partial, UPLen, Payload}
    end.
