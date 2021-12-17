%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_json_codec).

-behaviour(ag_engine_protocol_codec).
%% API
-export([
    decode/1, decode/2,
    encode/1, encode/2,
    protocol_name/0,
    init/0
]).

-spec decode(Data) ->
    Result when
    Data :: iolist() | binary(),
    Result :: maps:map().
decode(Data) ->
    agb_json:decode2map(Data).

-spec decode(Data, Opts) -> Result when
    Data :: iolist() | binary(),
    Opts :: list(),
    Result :: maps:map().
decode(Data, _) ->
    ?MODULE:decode(Data).

-spec encode(Msg) -> Result when
    Msg :: jiffy:json_value(),
    Result :: iodata().
encode(Msg) ->
    agb_json:encode(Msg).

-spec encode(Msg, Opts) -> Result when
    Msg :: jiffy:json_value(),
    Opts :: list(),
    Result :: iodata().
encode(Msg, _Opts) ->
    ?MODULE:encode(Msg).

-spec protocol_name() ->
    binary().
protocol_name() ->
    <<"jsx_json">>.

-spec init() ->
    ok.
init() ->
    ok.
