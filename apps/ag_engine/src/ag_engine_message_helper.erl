%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_message_helper).
-include("ag_engine_core_defines.hrl").

-export([
    pack_name/2,
    pack_code/2, pack_code/3,
    get_name/1,
    get_echo/1,
    pack_echo/2
]).
-export([
    get_value/2,
    get_session/1
]).

-spec pack_code(Name :: binary() | string(), Code :: integer()) ->
    map().
pack_code(Name, Code) ->
    ErrMsg = #{?MESSAGE_ERROR_CODE_KEY=> Code},
    pack_name(Name, ag_engine_message_wrap_code:pack_info(ErrMsg)).

-spec pack_code(Name :: binary() | string(), Code :: integer(), Message :: map()) ->
    map().
pack_code(Name, Code, Message) ->
    pack_name(Name, ag_engine_message_wrap_code:pack_info(Message#{?MESSAGE_ERROR_CODE_KEY=> Code})).

-spec pack_name(Name :: binary() | atom() | string(), MessageBody :: map() | [tuple()]) ->
    map().
pack_name(Name, MessageBody) when is_atom(Name) ->
    pack_name(atom_to_binary(Name, latin1), MessageBody);
pack_name(Name, MessageBody) when is_list(Name) ->
    pack_name(list_to_binary(Name), MessageBody);
pack_name(Name, MessageBody) when is_list(MessageBody) ->
    pack_name(Name, maps:from_list(MessageBody));
pack_name(Name, MessageBody) ->
    MessageBody#{?MESSAGE_NAME_KEY=>Name}.

-spec get_name(Message :: map()) ->
    binary().
get_name(#{?MESSAGE_NAME_KEY:=Name} = Message) when is_map(Message) ->
    Name;
get_name(_Message) ->
    error(noname_message).

-spec get_value(Key :: atom() | binary() | string(), Message :: map() | [tuple()]) ->
    term().
get_value(Key, Message) when is_atom(Key) ->
    get_value(atom_to_binary(Key, latin1), Message);
get_value(Key, Message) when is_list(Key) ->
    get_value(list_to_binary(Key), Message);
get_value(Key, Message) when is_list(Message) ->
    NoCaseKey = string:lowercase(Key),
    NewMessage = [{iolist_to_binary(string:lowercase(K)), V} || {K, V} <- Message],
    case lists:keyfind(NoCaseKey, 1, NewMessage) of
        false ->
            undefined;
        {NoCaseKey, Value} ->
            Value
    end;
get_value(Key, Message) when is_map(Message) ->
    NoCaseKey = string:lowercase(Key),
    NewMessage = maps:from_list([{iolist_to_binary(string:lowercase(K)), V} || {K, V} <- maps:to_list(Message)]),
    case maps:is_key(NoCaseKey, NewMessage) of
        false ->
            undefined;
        true ->
            maps:get(NoCaseKey, NewMessage)
    end.

-spec get_echo(map()) ->
    any().
get_echo(#{?INTERNAL_DEFINE_ECHO:=Echo} = Message) when is_map(Message) ->
    Echo;
get_echo(_Message) ->
    <<>>.

pack_echo(<<>>, Message) ->
    Message;
pack_echo(Echo, Message) ->
    Message#{?INTERNAL_DEFINE_ECHO=> Echo}.

-spec get_session(Message :: map() | [tuple()]) ->
    undefined | binary().
get_session(Message) ->
    get_value(?MESSAGE_SESSION_KEY, Message).