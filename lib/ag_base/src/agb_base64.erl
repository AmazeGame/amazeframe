%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------
-module(agb_base64).

%% API
-export([
    decode/1,
    encode/1,
    encode_mime/1
]).

-spec encode(
    binary() | iolist()
) ->
    binary().

encode(Bin) when is_binary(Bin) ->
    <<<<(url_encode_digit(D))>> || <<D>> <= base64:encode(Bin), D =/= $= >>;
encode(L) when is_list(L) ->
    encode(iolist_to_binary(L)).

-spec encode_mime(
    binary() | iolist()
) ->
    binary().
encode_mime(Bin) when is_binary(Bin) ->
    <<<<(url_encode_digit(D))>> || <<D>> <= base64:encode(Bin)>>;
encode_mime(L) when is_list(L) ->
    encode_mime(iolist_to_binary(L)).

-spec decode(
    binary() | iolist()
) ->
    binary().
decode(Bin) when is_binary(Bin) ->
    Bin2 =
        case byte_size(Bin) rem 4 of
            % 1 -> << Bin/binary, "===" >>;
            2 ->
                <<Bin/binary, "==">>;
            3 ->
                <<Bin/binary, "=">>;
            _ ->
                Bin
        end,
    base64:decode(<<<<(url_decode_digit(D))>> || <<D>> <= Bin2 >>);
decode(L) when is_list(L) ->
    decode(iolist_to_binary(L)).

url_encode_digit($/) ->
    $_;
url_encode_digit($+) ->
    $-;
url_encode_digit(D) ->
    D.

url_decode_digit($_) ->
    $/;
url_decode_digit($-) ->
    $+;
url_decode_digit(D) ->
    D.
