%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_archive).


-type id_type() :: integer() | string() | binary().

-callback create_user(Context :: map()) ->
    {id_type(),id_type()}.
-callback load_user(Context :: map()) ->
    {id_type(),id_type()}.

%% API
-export([
    init/0,
    create_user/1,
    load_user/1
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
            ag_engine_variable:put(archive_driver,Module) 
    end.

driver() ->
    ag_engine_variable:getv(archive_driver).

-spec create_user(Context :: term()) ->
    term().
create_user(Content) ->
    Driver = driver(),
    Driver:create_user(Content).

-spec load_user(Context :: term()) ->
    term() | false.
load_user(Content) ->
    Driver = driver(),
    Driver:load_user(Content).


