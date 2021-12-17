%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_login_event).

-callback on_login_online_player(Context :: map()) ->
    {ag_engine_core:operation_type(), map()}.

%% API
-export([
    init/0,
    on_login_online_player/1
]).

table_name() ->
    'login_event_module_ets'.

-spec init() ->
    ok.
init() ->
    Table = agb_ets:init(table_name()),
    scan_behaviour(Table).

scan_behaviour(Table) ->
    case agb_behaviour:get_behaviour_modules(?MODULE) of
        [] ->
            ok;
        [Module | _] ->
            agb_ets:put(Table, {driver, Module}), ok
    end.

driver() ->
    case ets:lookup(table_name(), driver) of
        [] ->
            undefined;
        [{driver, Module}] ->
            Module
    end.

-spec on_login_online_player(Content :: map()) ->
    {ag_engine_core:operation_type(), map()}.
on_login_online_player(Content) ->
    case driver() of
        undefined ->
            {kick_online, undefined};
        Module ->
            Module:on_login_online_player(Content)
    end.
