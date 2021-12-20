%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_protocol_codec).


%% API
-callback protocol_name() ->
    binary().
-callback encode(Message :: maps:map()) ->
    iodata().
-callback decode(Binary :: binary() | iolist()) ->
    maps:map().
-callback init() ->
    ok.

-export([
    init/0,
    encode/1, encode/3,
    decode/1, decode/4
]).

-spec init() ->
    ok.
init() ->
    scan_behaviours().

scan_behaviours() ->
    Modules = agb_behaviour:get_behaviour_modules(?MODULE),
    lists:foreach(
        fun(Module) ->
            Protocol = Module:protocol_name(),
            Module:init(),
            ag_engine_proto_codec_variable:put(Protocol, Module)
        end,
        Modules
    ).

-spec encode(Message :: map()) -> iodata().
encode(Message) ->
    agb_json:encode(Message).

-spec encode(
    Protocol :: binary()|string(),
    Message :: map()|[map()],
    Filters :: []|[{module(), function()}]) -> iodata().
encode(Protocol, Messages, Filters) when is_list(Messages) ->
    [agb_variablepack:pack(do_encode(Protocol, Message, Filters)) || Message <- Messages];
encode(Protocol, Message, Filters) ->
    do_encode(Protocol, Message, Filters).

do_encode(Protocol, Message, Filters) when is_list(Protocol) ->
    do_encode(agb_convertor:to_binary(Protocol), Message, Filters);
do_encode(<<"json">>, Message, Filters) ->
    after_codec_message(encode(Message), Filters);
do_encode(Protocol, Message, Filters) ->
    case codec_from_protocol(Protocol) of
        undefined ->
            after_codec_message(agb_json:encode(Message), Filters);
        Driver ->
            after_codec_message(Driver:encode(Message), Filters)
    end.

-spec decode(Binary :: binary()) -> map().
decode(Binary) ->
    agb_json:decode2map(Binary).

-spec decode(Protocol :: binary()|string(), MsgCarrier :: boolean(), Binary :: binary(),
    Filters :: [{module(), atom()}|function()]) -> [map()]|map().
decode(Protocol, true, Binary, Filters) ->
    [do_decode(Protocol, B, Filters) || B <- unpack_and_decode(Binary)];
decode(Protocol, _MsgCarrier, Binary, Filters) ->
    do_decode(Protocol, Binary, Filters).

do_decode(Protocol, Binary, Filters) when is_list(Protocol) ->
    do_decode(agb_convertor:to_binary(Protocol), Binary, Filters);
do_decode(<<"json">>, Binary, Filters) ->
    after_codec_message(decode(Binary), Filters);
do_decode(Protocol, Binary, Filters) ->
    case codec_from_protocol(Protocol) of
        undefined ->
            after_codec_message(agb_json:decode2map(Binary), Filters);
        Driver ->
            after_codec_message(Driver:decode(Binary), Filters)
    end.

unpack_and_decode(Binary) ->
    unpack_and_decode(Binary, []).

unpack_and_decode(<<>>, Binaries) ->
    lists:reverse(Binaries);
unpack_and_decode(Binary, Binaries) ->
    case agb_variablepack:unpack(Binary) of
        {ok, _Len, Payload} ->
            unpack_and_decode(<<>>, [Payload | Binaries]);
        {ok, _Len, Payload, LeftBinary} ->
            unpack_and_decode(LeftBinary, [Payload | Binaries]);
        {partial, Len, Payload} ->
            Err = agb_string:sprintf("can not support partial pack ~p ~p", [Len, Payload]),
            error(Err)
    end.

codec_from_protocol(<<"json">>) ->
    ?MODULE;
codec_from_protocol(Protocol) ->
    case ag_engine_proto_codec_variable:getv(Protocol) of
        undefined ->
            ?MODULE;
        Module ->
            Module
    end.

after_codec_message(Message, Filters) ->
    lists:foldl(
        fun
            ({Module, Function}, Msg0) ->
                Module:Function(Msg0);
            (Function, Msg0) ->
                Function(Msg0)
        end,
        Message,
        Filters
    ).
