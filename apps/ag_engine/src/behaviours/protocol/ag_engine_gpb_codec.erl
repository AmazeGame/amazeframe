%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_gpb_codec).

-behaviour(ag_engine_protocol_codec).
-include("../../../include/ag_engine_core_defines.hrl").
-include("../../../include/ag_engine_code_defines.hrl").
-include_lib("kernel/include/file.hrl").
%% API
-export([
    init/0,
    decode/1, decode/2,
    encode/1, encode/2,
    protocol_name/0,
    encode_object/1,
    decode_object/1
]).
-define(BASE_MESSAGE_PB_MOD, base_message_pb).

-spec init() ->
    ok.
init() ->
    {ok, Type} = application:get_env(ag_engine, gpb_msg_name_type),
    String = base_message(Type),
    {ok, BaseMessagePb, CodeBinary} = gpb_compile:string(?BASE_MESSAGE_PB_MOD, String, [binary]),
    load_code(BaseMessagePb, CodeBinary),
    agb_ets:init(table()),
    agb_ets:put(table(), {gpb_msg_name_type, Type}),
    scan_pb_module().

table() ->
    '##$?ets_base_message_codec'.

-spec protocol_name() ->
    binary().
protocol_name() ->
    <<"gpb">>.

base_message(string) ->
    agb_string:sprintf("message base_message {\n\trequired string ~s = 1;\n}\n", [?MESSAGE_NAME_KEY]);
base_message(int32) ->
    agb_string:sprintf("message base_message {\n\trequired int32 ~s = 1;\n}\n", [?MESSAGE_NAME_KEY]);
base_message(uint32) ->
    agb_string:sprintf("message base_message {\n\trequired uint32 ~s = 1;\n}\n", [?MESSAGE_NAME_KEY]).

scan_pb_module() ->
    Functions = [{encode_msg, 2}, {decode_msg, 2}, {get_msg_names, 0}, {get_msg_defs, 0}],
    Modules = agb_behaviour:get_function_modules("*_pb.beam", Functions),
    lists:foreach(
        fun(Module) ->
            Names = Module:get_msg_defs(),
            lists:foreach(
                fun
                    ({{msg, Name}, List}) ->
                        %%agb_ets:put(table(), Name, {Name, Module});
                        filter_name_and_put_ets(Module, Name, List);
                    ({{enum, _}, _}) ->
                        ignore
                end, Names)
        end,
        Modules
    ).

filter_name_and_put_ets(Module, Name, List) ->
    case catch agb_maplist:keyfind(name, name, List) of
        {'EXIT', _} ->
            ignore;
        false ->
            ignore;
        TMap ->
            put_ets(Module, Name, TMap)
    end.

put_ets(Module, Name, TMap) ->
    Opts = maps:get(opts, TMap),
    case catch lists:keyfind(default, 1, Opts) of
        {'EXIT', _} ->
            ignore;
        false ->
            ignore;
        {_, Default} when is_list(Default) ->
            agb_ets:put(table(), list_to_atom(Default), {Name, Module});
        {_, Default} when is_integer(Default) ->
            agb_ets:put(table(), Default, {Name, Module})
    end.

lookup_module(Name) when is_binary(Name) ->
    lookup_module(binary_to_atom(Name, utf8));
lookup_module(Name) when is_list(Name) ->
    lookup_module(list_to_atom(Name));
lookup_module(Name) when is_atom(Name) orelse is_integer(Name) ->
    case agb_ets:lookup(table(), Name) of
        [] ->
            undefined;
        {_, {NameAtom, Module}} ->
            {NameAtom, Module}
    end.

-spec encode(Message :: map()) ->
    binary().
encode(#{?MESSAGE_NAME_KEY_ATOM := Name} = Message) ->
    EncodeObject = encode_object(Message),
    {NameAtom, Module} = lookup_module(Name),
    Module:encode_msg(EncodeObject, NameAtom);
encode(#{?MESSAGE_NAME_KEY := Name} = Message) ->
    EncodeObject = encode_object(Message),
    {NameAtom, Module} = lookup_module(Name),
    Module:encode_msg(EncodeObject, NameAtom).

-spec encode(Message :: map(), {MidModule :: module(), Function :: atom()}) ->
    binary().
encode(#{?MESSAGE_NAME_KEY := Name} = Message, {MidModule, Function}) ->
    MidModule:Function(Message),
    EncodeObject = encode_object(Message),
    {NameAtom, Module} = lookup_module(Name),
    Module:encode_msg(EncodeObject, NameAtom).

-spec decode(Binary :: binary()) ->
    map().
decode(Binary) ->
    {_, Name} = ?BASE_MESSAGE_PB_MOD:decode_msg(Binary, base_message),
    {NameAtom, Module} = lookup_module(Name),
    decode_object(Module:decode_msg(Binary, NameAtom)).

-spec decode(Binary :: binary(), {MidModule :: module(), Function :: atom()}) ->
    map().
decode(Binary, {MidModule, Function}) ->
    {_, Name} = ?BASE_MESSAGE_PB_MOD:decode_msg(Binary, base_message),
    {NameAtom, Module} = lookup_module(Name),
    MsgObj = Module:decode_msg(Binary, NameAtom),
    decode_object(MidModule:Function(MsgObj)).

encode_object(Object) ->
    maps:from_list(encode_k_v_list(maps:to_list(Object))).

encode_k_v_list(KVTupleList) ->
    lists:map(
        fun
            ({K, V}) ->
                encode_k_v(K, V);
            (V) ->
                encode_v(V)
        end,
        KVTupleList
    ).

encode_k_v(K, V) ->
    {encode_k(K), encode_v(V)}.

encode_k(K) ->
    agb_convertor:to_atom(K).

encode_v(V) when is_map(V) ->
    encode_object(V);
encode_v(List) when is_list(List) ->
    encode_k_v_list(List);
encode_v(V) ->
    V.

decode_object(Object) when is_list(Object) ->
    lists:map(
        fun(V) ->
            decode_object(V)
        end,
        Object
    );
decode_object(Object) when is_map(Object) ->
    maps:from_list(decode_k_v_list(maps:to_list(Object)));
decode_object(Object) ->
    Object.

decode_k_v_list(KVList) ->
    lists:map(
        fun
            ({K, V}) ->
                decode_k_v(K, V);
            (V) ->
                decode_object(V)
        end,
        KVList
    ).

decode_k_v(K, V) ->
    {decode_k(K), decode_v(V)}.

decode_k(K) ->
    agb_convertor:to_binary(K).

decode_v(V) when is_map(V) ->
    decode_object(V);
decode_v(List) when is_list(List) ->
    decode_k_v_list(List);
decode_v(V) ->
    V.

unload_code(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod),
    code:delete(Mod),
    ok.

load_code(Mod, Code) ->
    unload_code(Mod),
    {module, Mod} = code:load_binary(Mod, "<nofile>", Code),
    ok.
