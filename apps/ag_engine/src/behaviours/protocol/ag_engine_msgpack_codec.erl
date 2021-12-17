%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_msgpack_codec).


-behaviour(ag_engine_protocol_codec).
%% API
-export([
    decode/1, decode/2,
    encode/1, encode/2,
    protocol_name/0,
    init/0
]).

-spec decode(Data :: binary()) ->
    map().
decode(Data) ->
    {ok, Result} = msgpack:unpack(Data, [{unpack_str, as_binary}]),
    Result.

-spec decode(Data :: binary(), _ :: any()) ->
    map().
decode(Data, _) ->
    {ok, Result} = msgpack:unpack(Data, [{unpack_str, as_binary}]),
    Result.

-spec encode(msgpack:object()) ->
    binary().
encode(Data) ->
    msgpack:pack(Data, [{pack_str, from_binary}]).

-spec encode(msgpack:object(), _ :: any()) ->
    binary().
encode(Data, _) ->
    msgpack:pack(Data, [{pack_str, from_binary}]).

-spec protocol_name() ->
    binary().
protocol_name() ->
    <<"msgpack">>.

-spec init() ->
    ok.
init() ->
    ok.