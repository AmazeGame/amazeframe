%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_platform).

-include_lib("ag_base/include/agb_debuglogger.hrl").

%% API
-export([
    init/0,
    validate/2,
    transform_id/2,
    restore_id/2
]).

-callback auth_types() ->
    [term()].
-callback validate(Type :: term(), SecurityInfo :: term()) ->
    {ok, Context :: term()}|{error, Reason :: term()}.
-callback transform_id(Type :: term(), Id :: binary()) ->
    binary().
-callback restore_id(Type :: term(), Id :: binary()) ->
    binary().

-record(auth_type, {type :: term(), module :: module()}).

init() ->
    case ets:info(table()) of
        undefined ->
            ets:new(table(), [public, set, named_table, {keypos, #auth_type.type}]);
        _ ->
            ignore
    end,
    scan_behaviour().

table() ->
    'ts_game_thirdplatform'.

scan_behaviour() ->
    Mods = agb_behaviour:get_behaviour_modules(?MODULE),
    lists:foreach(
        fun(Mod) ->
            Types = Mod:auth_types(),
            lists:foreach(
                fun(Type) ->
                    ets:insert(table(), #auth_type{type = Type, module = Mod})
                end,
                Types
            )
        end,
        Mods
    ).

lookup_module(Type) when is_list(Type) ->
    lookup_module(list_to_binary(Type));
lookup_module(Type) when is_binary(Type) ->
    case ets:lookup(table(), Type) of
        [] ->
            undefined;
        [#auth_type{module = Handler} | _] ->
            Handler
    end.

validate(Type, SecurityInfo) ->
    case lookup_module(Type) of
        undefined ->
            {error, {Type, no_auth_type}};
        Module ->
            Module:validate(Type, SecurityInfo)
    end.

transform_id(Type, Id) ->
    case lookup_module(Type) of
        undefined ->
            Id;
        Module ->
            Module:transform_id(Type, Id)
    end.

restore_id(Type, Id) ->
    case lookup_module(Type) of
        undefined ->
            Id;
        Module ->
            Module:restore_id(Type, Id)
    end.
