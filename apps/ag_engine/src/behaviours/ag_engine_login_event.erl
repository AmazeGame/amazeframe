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

-spec init() ->
    ok.
init() ->
    scan_behaviour().

scan_behaviour() ->
    case agb_behaviour:get_behaviour_modules(?MODULE) of
        [] ->
            ok;
        [Module | _] ->
            ag_engine_variable:put(login_driver, Module)
    end.

driver() ->
    ag_engine_variable:getv(login_driver).

-spec on_login_online_player(Content :: map()) ->
    {ag_engine_core:operation_type(), map()}.
on_login_online_player(Content) ->
    case driver() of
        undefined ->
            {kick_online, undefined};
        Module ->
            Module:on_login_online_player(Content)
    end.
