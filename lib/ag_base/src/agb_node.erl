%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------
-module(agb_node).

%% API
-export([
    split/1,
    get_node_sname/1,
    get_node_sname_str/1,
    names/0,
    str_names/0
]).

-spec split(NodeName :: atom()) ->
    {atom(), string()}.
split(NodeName) ->
    NameStr = atom_to_list(NodeName),
    I = string:str(NameStr, "@"),
    if
        I =:= 0 ->
            {NodeName, ""};
        true ->
            Name = string:substr(NameStr, 1, I - 1),
            Host = string:substr(NameStr, I + 1),
            {list_to_atom(Name), Host}
    end.

-spec get_node_sname(NodeName :: atom()) ->
    atom().
get_node_sname(Node) ->
    StrNode = atom_to_list(Node),
    case string:tokens(StrNode, "@") of
        [NodeName, _Host] ->
            list_to_atom(NodeName);
        _ ->
            undefined
    end.

-spec get_node_sname_str(NodeName :: atom()) ->
    string().
get_node_sname_str(Node) ->
    StrNode = atom_to_list(Node),
    case string:tokens(StrNode, "@") of
        [NodeName, _Host] ->
            NodeName;
        _ ->
            []
    end.

-spec str_names() ->
    list().
str_names() ->
    lists:map(
        fun(N) ->
        binary_to_list(N) end,
        names()
    ).

-spec names() ->
    list().
names() ->
    {ok, NamePorts} = erl_epmd:names(),
    lists:filter(
        fun
            (<<>>) -> false;
            (_Name) -> true
        end,
        [name(Port) || {_, Port} <- NamePorts]
    ).

name(Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {packet, 2}, {active, false}]) of
        {error, _Reason} ->
            <<>>;
        {ok, P} ->
            OutName =
                try
                    ok = gen_tcp:send(P, <<$n, 5:16, 16#ffffffff:32, 1:32, "erl@localhost">>),
                    {ok, <<"sok">>} = gen_tcp:recv(P, 0),
                    {ok, <<$n, _V1, _V2, _Flags:32, _Challenge:32, Name/binary>>} = gen_tcp:recv(P, 0),
                    Name
                catch
                    _E:_R:_S ->
                        <<>>
                end,
            gen_tcp:close(P),
            OutName
    end.
