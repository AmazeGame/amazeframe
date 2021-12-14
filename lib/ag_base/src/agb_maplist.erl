%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------

-module(agb_maplist).

%% API
-export([
    keydelete/3,
    keyfind/3,
    keymap/3,
    keymember/3,
    keymerge/3,
    keyreplace/4,
    keystore/4,
    keysearch/3,
    keysort/2,
    keytake/3
]).

-spec(keydelete(Key :: term(), Field :: term(), MapList :: [map()]) ->
    [map()]).
keydelete(Key, Field, MapList) ->
    lists:filter(
        fun(Map) ->
            case maps:is_key(Field, Map) of
                true ->
                    maps:get(Field, Map) =/= Key; %%找到删除
                false ->
                    true                        %%找不到保留
            end
        end,
        MapList
    ).

-spec(keyfind(Key :: term(), Field :: term(), MapList :: [map()]) ->
    map()|false).
keyfind(Key, Field, MapList) ->
    lists:foldl(
        fun
            (_Map, LastMap) when is_map(LastMap) ->
                LastMap;
            (Map, false) ->
                case maps:is_key(Field, Map) of
                    true ->
                        case maps:get(Field, Map) == Key of
                            true ->
                                Map;
                            false ->
                                false
                        end;
                    false ->
                        false
                end
        end,
        false,
        MapList
    ).

-spec(keymap(Fun :: function(), Field :: term(), MapList :: [map()]) ->
    []).
keymap(Fun, Field, MapList) ->
    lists:map(
        fun(Map) ->
            Fun(maps:get(Field, Map))
        end,
        MapList
    ).

-spec(keymember(Key :: term(), Field :: term(), MapList :: [map()]) ->
    boolean()).
keymember(Key, Field, MapList) ->
    lists:foldl(
        fun
            (_Map, true) ->
                true;
            (Map, false) ->
                case maps:is_key(Field, Map) of
                    true ->
                        maps:get(Field, Map) == Key;
                    false ->
                        false
                end
        end,
        false,
        MapList
    ).

-spec(keymerge(Field :: term(), MapList1 :: [map()], MapList2 :: [map()]) ->
    [map()]).
keymerge(Field, MapList1, MapList2) ->
    case MapList2 of
        [] ->
            MapList1;
        [H2 | T2] ->
            E2 = maps:get(Field, H2),
            M = keymerge2_1(Field, MapList1, E2, H2, T2, []),
            lists:reverse(M, [])
    end.

-spec(keyreplace(Key :: term(), Field :: term(), MapList :: [map()], NewMap :: map()) ->
    [map()]).
keyreplace(Key, Field, [Map | Tail], NewMap) ->
    case maps:is_key(Field, Map) of
        true ->
            case maps:get(Field, Map) == Key of
                true ->
                    [NewMap | Tail];
                false ->
                    [Map | keyreplace(Key, Field, Tail, NewMap)]
            end;
        false ->
            [Map | keyreplace(Key, Field, Tail, NewMap)]
    end;
keyreplace(_, _, [], _) ->
    [].


-spec(keystore(Key :: term(), Field :: term(), MapList :: [map()], NewMap :: map()) ->
    [map()]).
keystore(Key, Field, [Map | Tail], NewMap) ->
    case maps:is_key(Field, Map) of
        true ->
            case maps:get(Field, Map) == Key of
                true ->
                    [NewMap | Tail];
                false ->
                    [Map | keystore(Key, Field, Tail, NewMap)]
            end;
        false ->
            [Map | keystore(Key, Field, Tail, NewMap)]
    end;
keystore(_, _, [], NewMap) ->
    [NewMap].


-spec(keysearch(Key :: term(), Field :: term(), MapList :: [map()]) ->
    {value, map()} | false).
keysearch(Key, Field, [Map | Tail]) ->
    case maps:is_key(Field, Map) of
        true ->
            case maps:get(Field, Map) == Key of
                true ->
                    {value, Map};
                false ->
                    keysearch(Key, Field, Tail)
            end;
        false ->
            keysearch(Key, Field, Tail)
    end;
keysearch(_, _N, []) ->
    false.

-spec(keysort(Field :: term(), MapList :: [map()]) ->
    [map()]).
keysort(Field, MapList) ->
    Fun =
        fun(A, B) ->
            KeyA = maps:get(Field, A),
            KeyB = maps:get(Field, B),
            KeyA < KeyB
        end,
    lists:sort(Fun, MapList).

-spec(keytake(Key :: term(), Field :: term(), MapList :: [map()]) ->
    {value, map(), [map()]} | false).
keytake(Key, Field, MapList) ->
    keytake(Key, Field, MapList, []).

keytake(Key, Field, [Map | Tail], MapList) ->
    case maps:is_key(Field, Map) of
        true ->
            case maps:get(Field, Map) == Key of
                true ->
                    {value, Map, lists:reverse(MapList, Tail)};
                false ->
                    keytake(Key, Field, Tail, [Map | MapList])
            end;
        false ->
            keytake(Key, Field, Tail, [Map | MapList])
    end;
keytake(_K, _N, [], _L) ->
    false.

%% Elements from the first list are prioritized.
keymerge2_1(Field, [H1 | T1], E2, H2, T2, M) ->
    case maps:get(Field, H1) of
        E1 when E1 =< E2 ->
            keymerge2_1(Field, T1, E2, H2, T2, [H1 | M]);
        E1 ->
            keymerge2_2(Field, T1, E1, H2, T2, M, H1)
    end;
keymerge2_1(_Field, [], _E2, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

keymerge2_2(Field, T1, E1, HdM, [H2 | T2], M, H1) ->
    case maps:get(Field, H2) of
        E2 when E1 =< E2 ->
            keymerge2_1(Field, T1, E2, H2, T2, [H1, HdM | M]);
        _E2 ->
            keymerge2_2(Field, T1, E1, H2, T2, [HdM | M], H1)
    end;
keymerge2_2(_Field, T1, _E1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

