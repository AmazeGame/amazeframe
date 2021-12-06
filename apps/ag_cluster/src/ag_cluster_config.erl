%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_cluster_config).


%% API
-export([
    init/0,
    put/2,
    get/1,
    getv/1
]).

-spec init() ->
    atom().
init() ->
    agb_ets:init(table()).

-spec table() ->
    atom().
table() ->
    'ag_cluster_config'.

-spec put(Key :: any(), Value :: term()) ->
    boolean().
put(Key, Value) ->
    agb_ets:put(table(), Key, Value).

-spec get(Key :: any()) ->
    term().
get(Key) ->
    case agb_ets:lookup(table(), Key) of
        [] ->
            undefined;
        Obj ->
            Obj
    end.

-spec getv(Key :: any()) ->
    atom().
getv(Key) ->
    case agb_ets:lookup(table(), Key) of
        [] ->
            undefined;
        {_, ObjV} ->
            ObjV
    end.
