%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.29
%%%-------------------------------------------------------------------
%%%

-module(agdb_database_adapter).

-callback connect(atom(), tuple()) ->
    ok | {error, Reason :: term()}.

-callback find_one(term(), binary(), map() | [tuple()]) ->
    undefined | map() | {error, Reason :: term()}.
-callback find_one(term(), binary(), map() | [tuple()], list()) ->
    undefined | map() | {error, Reason :: term()}.
-callback find(term(), binary(), map() | [tuple()]) ->
    undefined | map() | list() | {error, Reason :: term()}.
-callback find(term(), binary(), map() | [tuple()], list()) ->
    undefined | map() | list() |{error, Reason :: term()}.
-callback count(term(), binary(), map() | [tuple()]) ->
    integer() | {error, Reason :: term()}.
-callback insert(term(), binary(), map() | [map()]) ->
    ok | {error, Reason :: term()}.
-callback replace(term(), binary(), map() | [map()]) ->
    ok | {error, Reason :: term()}.
-callback update(term(), binary(), map() | [tuple()], map() | [tuple()]) ->
    ok | {error, Reason :: term()}.
-callback delete(term(), binary(), map() | [tuple()]) ->
    ok | {error, Reason :: term()}.

-export([init/3]).
-export([adapter/1]).
-export([find_one/3, find_one/4]).
-export([find/3, find/4]).
-export([count/3]).
-export([insert/3]).
-export([replace/3]).
-export([update/4]).
-export([delete/3]).

-spec init(atom(), atom(), tuple()) ->
    {ok, module()} | {error, term()}.
init(Driver, PoolName, Opts) ->
    case adapter(PoolName) of
        undefined ->
            Adapter = list_to_atom(lists:concat(["agdb_database_adapter_", Driver])),
            case lists:member(Adapter, agb_behaviour:get_behaviour_modules(?MODULE)) of
                true ->
                    ok = Adapter:connect(PoolName, Opts),
                    agdb_config:put({?MODULE, PoolName}, {Adapter, PoolName}),
                    {ok, Adapter};
                false ->
                    {error, agb_string:sprintf("the db driver:~p is error!~n", [Driver])}
            end;
        {Adapter, _PoolName} ->
            {ok, Adapter}
    end.

-spec adapter(atom()) ->
    undefined | {module(), atom()}.
adapter(PoolName) ->
    case agdb_config:geto({?MODULE, PoolName}) of
        undefined ->
            undefined;
        {_, AdapterInfo} ->
            AdapterInfo
    end.

-spec find_one(DriverPoolName :: atom(), Table :: binary(), Where :: map() | [tuple()]) ->
    undefined | map() | {error, Reason :: term()}.
find_one(DriverPoolName, Table, Where) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:find_one(PoolName, Table, Where).

-spec find_one(DriverPoolName :: atom(), Table :: binary(), Where :: map() | [tuple()], QueryFields :: list()) ->
    undefined | map() | {error, Reason :: term()}.
find_one(DriverPoolName, Table, Where, QueryFields) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:find_one(PoolName, Table, Where, QueryFields).

-spec find(DriverPoolName :: atom(), Table :: binary(), Where :: map() | [tuple()]) ->
    list() | {error, Reason :: term()}.
find(DriverPoolName, Table, Where) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:find(PoolName, Table, Where).

-spec find(DriverPoolName :: atom(), Table :: binary(), Where :: map() | [tuple()], QueryFields :: list()) ->
    list() | {error, Reason :: term()}.
find(DriverPoolName, Table, Where, QueryFields) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:find(PoolName, Table, Where, QueryFields).

-spec count(DriverPoolName :: atom(), Table :: binary(), Where :: map() | [tuple()]) ->
    integer() | {error, Reason :: term()}.
count(DriverPoolName, Table, Where) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:count(PoolName, Table, Where).

-spec insert(DriverPoolName :: atom(), Table :: binary(), InsertObjects :: map() | [map()]) ->
    ok | {error, Reason :: term()}.
insert(DriverPoolName, Table, InsertObjects) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:insert(PoolName, Table, InsertObjects).

-spec replace(DriverPoolName :: atom(), Table :: binary(), InsertOrUpdateObjects :: map() | [map()]) ->
    ok | {error, Reason :: term()}.
replace(DriverPoolName, Table, InsertOrUpdateObjects) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:replace(PoolName, Table, InsertOrUpdateObjects).

-spec update(DriverPoolName :: atom(), Table :: binary(), Where :: map() | [tuple()], Update :: map()) ->
    ok | {error, Reason :: term()}.
update(DriverPoolName, Table, Where, Update) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:update(PoolName, Table, Where, Update).

-spec delete(DriverPoolName :: atom(), Table :: binary(), Where :: map() | [tuple()]) ->
    ok | {error, Reason :: term()}.
delete(DriverPoolName, Table, Where) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:delete(PoolName, Table, Where).
