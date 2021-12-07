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

-module(agdb_database_adapter_mongodb).

-behaviour(agdb_database_adapter).
-include_lib("ag_base/include/agb_debuglogger.hrl").

-export([connect/2]).
-export([make_selector_string/1]).
-export([find_one/3, find_one/4]).
-export([find/3, find/4, find/6]).
-export([count/3, count/4]).
-export([insert/3]).
-export([replace/3]).
-export([update/4, update/6]).
-export([inc/4]).
-export([delete/3]).
-export([ensure_index/3]).
-export([
    add_one_bagobj/5,
    update_one_bagobj/6,
    remove_bagobj/5,
    delete_bagobj/4,
    find_bag/4
]).

-define(MONGODB_SUP, mc_super_sup).

-type mongo_result() :: [bson:document() | maps:map()].
-type collection() :: binary() | atom(). % without db prefix

-spec connect(atom(), tuple()) ->
    ok | {error, Reason :: term()}.
connect(PoolName, {Indexes, {Type, Hosts}, PoolArgs, WorkerArgs}) ->
    {ok,Pid} = mongoc:connect({Type, Hosts}, [{register, PoolName} ,{name, PoolName} | PoolArgs], WorkerArgs),
    create_indexes(Pid, Indexes),
    ok;
connect(PoolName, {Indexes, {Type, ReplicaName, Hosts}, PoolArgs, WorkerArgs}) ->
    {ok,Pid}  = mongoc:connect({Type, ReplicaName, Hosts}, [{register, PoolName} | PoolArgs], WorkerArgs),
    create_indexes(Pid, Indexes),
    ok.
create_indexes(_PoolName, []) ->
    ok;
create_indexes(PoolName, [{Coll, IndexSpecs} | T]) ->
    ensure_index(PoolName, Coll, IndexSpecs),
    create_indexes(PoolName, T).

%% return {{boolean(), map()}, bson:document()};
-spec insert(term(), collection(), map() | [map()]) ->
    ok | {error, Reason :: term()}.
insert(PoolName, Coll, Doc) ->
    Result = mongo_api:insert(PoolName, agb_convertor:to_binary(Coll), Doc),
    case Result of
        {{true, #{<<"n">> := Count, <<"writeErrors">> := [#{<<"errmsg">> := Reason}]}}, _} ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>agdb_database_adapter_mongodb insert PoolName:[~p],Coll:[~p],writeErrors:[~p]",
                [PoolName, Coll, Reason]),
            {error, {Count, Reason}};
        {{true, #{<<"n">> := Count}}, _} when Count > 0 ->
            ok;
        {{false, #{<<"errmsg">> := Reason}}, _} ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>agdb_database_adapter_mongodb insert PoolName:[~p],Coll:[~p],Errors:[~p]",
                [PoolName, Coll, Reason]),
            {error, Reason}
    end.

%% return {{boolean(), map()}, bson:document()};
-spec replace(term(), collection(), map() | [map()]) ->
    ok | {error, Reason :: term()}.
replace(PoolName, Coll, Doc) ->
    insert(PoolName, Coll, Doc).

%% return {boolean(), map()}
-spec update(term(), collection(), map()|[tuple()], map()|[tuple()]) ->
    ok | {error, Reason :: term()}.
update(PoolName, Coll, Selector1, Doc) ->
    Selector = make_selector_string(Selector1),
    Result = mongo_api:update(PoolName,
        agb_convertor:to_binary(Coll), Selector, #{<<"$set">> => bson:flatten_map(Doc)}, #{}),
    case Result of
        {true, _} ->
            ok;
        {false, Reason} ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>~p update error:[~p],PoolName:[~p],Coll:[~p],Selector:[~p],update:[~p]",
                [?MODULE, Reason, PoolName, Coll, Selector, Doc]),
            {error, Reason}
    end.

%% return {boolean(), map()}
-spec update(term(), collection(), map() | [tuple()], map(), boolean(), boolean()) ->
    ok | {error, Reason :: term()}.
update(PoolName, Coll, Selector, Doc, Upsert, MultiUpdate) ->
    Selector1 = make_selector_string(Selector),
    Result = mongo_api:update(PoolName, agb_convertor:to_binary(Coll), Selector1,
        #{<<"$set">> => bson:flatten_map(Doc)}, #{upsert => Upsert, multi => MultiUpdate}),
    case Result of
        {true, _} ->
            ok;
        {false, Reason} ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>~p update error:[~p],PoolName:[~p],Coll:[~p],Selector:[~p],update:[~p]",
                [?MODULE, Reason, PoolName, Coll, Selector1, Doc]),
            {error, Reason}
    end.
%% return {boolean(), map()}

-spec inc(term(), collection(), map() | [tuple()], map()) ->
    ok | {error, Reason :: term()}.
inc(PoolName, Coll, Selector, Doc) ->
    Selector1 = make_selector_string(Selector),
    Result = mongo_api:update(PoolName,
        agb_convertor:to_binary(Coll), Selector1, #{<<"$inc">> => bson:flatten_map(Doc)}, #{}),
    case Result of
        {true, _} ->
            ok;
        {false, Reason} ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>~p inc error:[~p],PoolName:[~p],Coll:[~p],Selector:[~p],Doc:[~p]",
                [?MODULE, Reason, PoolName, Coll, Selector1, Doc]),
            {error, Reason}
    end.

%% return {boolean(), map()}.
-spec delete(term(), collection(), map() | [tuple()]) ->
    ok | {error, Reason :: term()}.
delete(PoolName, Coll, Selector1) ->
    Selector = make_selector_string(Selector1),
    Result = mongo_api:delete(PoolName, agb_convertor:to_binary(Coll), Selector),
    case Result of
        {true, _} ->
            ok;
        {false, #{<<"errmsg">> := Reason}} ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>~p delete error:[~p],PoolName:[~p],Coll:[~p],Selector:[~p]",
                [?MODULE, Reason, PoolName, Coll, Selector]),
            {error, Reason}
    end.

%% @doc returns first selected document, if any
-spec find_one(term(), collection(), map()|[tuple()]) ->
    map() | undefined.
find_one(PoolName, Coll, Selector1) ->
    Selector = make_selector_string(Selector1),
    mongo_api:find_one(PoolName, agb_convertor:to_binary(Coll), Selector, #{<<"_id">> => false}).

-spec find_one(term(), collection(), map()|[tuple()], list()) ->
    map() | undefined.
find_one(PoolName, Coll, Selector1, Fields) ->
    Selector = make_selector_string(Selector1),
    Args = lists:foldl(fun(Field, Map) ->
        maps:put(Field, true, Map) end, #{}, Fields),
    mongo_api:find_one(PoolName, agb_convertor:to_binary(Coll), Selector, Args#{<<"_id">> => false}).

-spec find(atom(), collection(), map()|[tuple()]) ->
    mongo_result().
find(PoolName, Coll, Selector1) ->
    Selector = make_selector_string(Selector1),
    Result = mongo_api:find(PoolName, agb_convertor:to_binary(Coll), Selector, #{<<"_id">> => false}),
    case Result of
        [] ->
            [];
        {ok, PID} ->
            get_all(PID)
    end.

-spec find(atom(), collection(), map()|[tuple()], list()) ->
    mongo_result().
find(PoolName, Coll, Selector1, Fields) ->
    Selector = make_selector_string(Selector1),
    Args = lists:foldl(fun(Field, Map) ->
        maps:put(Field, true, Map) end, #{}, Fields),
    Result = mongo_api:find(PoolName, agb_convertor:to_binary(Coll), Selector, Args#{<<"_id">> => false}),
    case Result of
        [] ->
            [];
        {ok, PID} ->
            get_all(PID)
    end.

-spec find(atom(), collection(), map()|[tuple()], list(), integer(), integer()) ->
    mongo_result().
find(PoolName, Coll, Selector1, Fields, Skip, BatchSize) ->
    Selector = make_selector_string(Selector1),
    Args = lists:foldl(fun(Field, Map) ->
        maps:put(Field, true, Map) end, #{}, Fields),
    Args1 = Args#{<<"_id">> => false},
    Result = mongo_api:find(PoolName, agb_convertor:to_binary(Coll), Selector, Args1, Skip, BatchSize),
    case Result of
        [] ->
            [];
        {ok, PID} ->
            get_all(PID)
    end.

-spec get_all(Cursor) ->
    Result when
    Cursor :: pid(),
    Result :: mongo_result().
get_all(Cursor) ->
    case mc_cursor:rest(Cursor) of
        error ->
            [];
        L ->
            L
    end.

-spec count(term(), collection(), map()|[tuple()]) ->
    integer().
count(PoolName, Coll, Selector1) ->
    Selector = make_selector_string(Selector1),
    mongo_api:count(PoolName, agb_convertor:to_binary(Coll), Selector, 0).

-spec count(term(), collection(), map()|[tuple()], map()) ->
    integer().
count(PoolName, Coll, Selector1, #{limit := Limit}) ->
    Selector = make_selector_string(Selector1),
    mongo_api:count(PoolName, agb_convertor:to_binary(Coll), Selector, Limit).

%%-spec command(term(), mc_worker_api:selector()) -> {boolean(), map()}. % Action
%%command(PoolName, Command) ->
%%    poolboy:transaction(PoolName,
%%        fun(Worker) ->
%%            mc_worker_api:command(Worker, Command)
%%        end).

-spec ensure_index(term(), collection(), [bson:document()]) ->
    ok | {error, any()}.
ensure_index(PoolName, Coll, IndexSpec) ->
    ?LOG_INFO("ensure_index(~p ~p ~p)~n", [PoolName, Coll,IndexSpec]),
    Result = mongo_api:ensure_index(PoolName, Coll, IndexSpec),
    ?LOG_INFO("ensure_index IndexSpec:~p result:~p~n", [IndexSpec, Result]).

-spec update_one_bagobj(term(), collection(), binary(), map()|[tuple()], map()|[tuple()], map()|[tuple()]) ->
    ok | {error, Reason :: term()}.
update_one_bagobj(PoolName, Coll, BagName, SelectorObj, SelectorOwner, Doc) ->
    BagNameB = agb_convertor:to_binary(BagName),
    Selector1 = maps:merge(bson:flatten_map(#{BagNameB => SelectorObj}), SelectorOwner),
    Selector = make_selector_string(Selector1),
    %?LOG_DEBUG("mongodb delete_bagobj Selector:[~p],BagName:[~p],doc:[~p] ~n",[Selector,BagName,Doc]),
    Result = mongo_api:update(PoolName, agb_convertor:to_binary(Coll), Selector,
        #{<<"$set">> => #{<<BagNameB/binary, $., $$>> => bson:flatten_map(Doc)}}, #{}),
    case Result of
        {true, _} ->
            ok;
        {false, Reason} ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>~p update_one_bagobj error:[~p],PoolName:[~p],Coll:[~p],Selector:[~p],Doc:[~p]",
                [?MODULE, Reason, PoolName, Coll, Selector, Doc]),
            {error, Reason}
    end.

-spec add_one_bagobj(term(), collection(), binary(), map()|[tuple()], map()|[tuple()]) ->
    ok | {error, Reason :: term()}.
add_one_bagobj(PoolName, Coll, BagName, SelectorOwner, Doc) ->
    Selector = make_selector_string(SelectorOwner),
    %?LOG_DEBUG("mongodb add_one_bagobj Selector:[~p],BagName:[~p]~n",[Selector,BagName]),
    Result = mongo_api:update(PoolName, agb_convertor:to_binary(Coll), Selector,
        #{<<"$addToSet">> => #{agb_convertor:to_binary(BagName) => bson:flatten_map(Doc)}}, #{}),
    case Result of
        {true, _} ->
            ok;
        {false, Reason} ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>~p add_one_bagobj error:[~p],PoolName:[~p],Coll:[~p],Selector:[~p],Doc:[~p]",
                [?MODULE, Reason, PoolName, Coll, Selector, Doc]),
            {error, Reason}
    end.

-spec remove_bagobj(term(), collection(), binary(), map()|[tuple()], map()|[tuple()]) ->
    ok | {error, Reason :: term()}.
remove_bagobj(PoolName, Coll, BagName, SelectorObj, SelectorOwner) ->
    SelectorOwner1 = make_selector_string(SelectorOwner),
    SelectorObj1 = make_selector_string(SelectorObj),
    %?LOG_DEBUG("mongodb remove_one_bagobj Selector:[~p],BagName:[~p]~n",[SelectorOwner1,BagName]),
    Result = mongo_api:update(PoolName, agb_convertor:to_binary(Coll), SelectorOwner1,
        #{<<"$pull">> => #{agb_convertor:to_binary(BagName) => SelectorObj1}}, #{}),
    case Result of
        {true, _} ->
            ok;
        {false, #{<<"errmsg">> := Reason}} ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>~p remove_bagobj error:[~p],PoolName:[~p],Coll:[~p],Selector:[~p]",
                [Reason, PoolName, Coll, SelectorObj1]),
            {error, Reason}
    end.

-spec delete_bagobj(term(), collection(), binary(), map()|[tuple()]) ->
    ok | {error, Reason :: term()}.
delete_bagobj(PoolName, Coll, BagName, SelectorOwner) ->
    Selector = make_selector_string(SelectorOwner),
    %?LOG_DEBUG("mongodb delete_bagobj Selector:[~p],BagName:[~p]~n",[Selector,BagName]),
    Result = mongo_api:update(PoolName, agb_convertor:to_binary(Coll), Selector,
        #{<<"$unset">> => #{agb_convertor:to_binary(BagName) => <<"">>}}, #{}),
    case Result of
        {true, _} ->
            ok;
        {false, #{<<"errmsg">> := Reason}} ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>~p delete_bagobj error:[~p],PoolName:[~p],Coll:[~p],Selector:[~p]",
                [Reason, PoolName, Coll, Selector]),
            {error, Reason}
    end.

-spec find_bag(term(), collection(), binary(), map()|[tuple()]) ->
    list() | {error, Reason :: term()}.
find_bag(PoolName, Coll, BagName, SelectorOwner) ->
    case find(PoolName, Coll, SelectorOwner, [BagName]) of
        [] ->
            [];
        [BagObjMap] ->
            maps:get(BagName, BagObjMap, [])
    end.

-spec make_selector_string(map() | [any()]) ->
    tuple().
make_selector_string(Selector) when is_map(Selector) ->
    make_selector_string(maps:to_list(Selector));
make_selector_string([]) ->
    {};
make_selector_string(Selector) ->
    erlang:list_to_tuple(lists:reverse(lists:foldl(
        fun
            ({K, V}, Sum) ->
                [pack_value(V), agb_convertor:to_binary(K) | Sum];
            ({K, 'equals', V}, Sum) ->
                [pack_value(V), agb_convertor:to_binary(K) | Sum];
            ({K, '=', V}, Sum) ->
                [pack_value(V), agb_convertor:to_binary(K) | Sum];
            ({K, 'in', {Min, Max}}, Sum) ->
                [{'$gte', Min}, agb_convertor:to_binary(K), {'$lte', Max}, agb_convertor:to_binary(K) | Sum];
            ({K, 'in', Vs}, Sum) when is_list(Vs) ->
                [{'$in', lists:map(fun(V) ->
                    pack_value(V) end, Vs)}, agb_convertor:to_binary(K) | Sum];
            ({K, 'not_in', Vs}, Sum) when is_list(Vs) ->
                [{'$nin', lists:map(fun(V) ->
                    pack_value(V) end, Vs)}, agb_convertor:to_binary(K) | Sum];
            ({K, 'not_in', {Min, Max}}, Sum) ->
                [[{agb_convertor:to_binary(K), {'$lt', Min}}, {agb_convertor:to_binary(K), {'$gt', Max}}], '$or' | Sum];
            ({K, Operator, V}, Sum) ->
                [{operator_to_mongo_op(Operator), pack_value(V)}, agb_convertor:to_binary(K) | Sum]
        end, [], Selector))).

operator_to_mongo_op('not_equals') ->
    '$ne';
operator_to_mongo_op('!=') ->
    '$ne';
operator_to_mongo_op('gt') ->
    '$gt';
operator_to_mongo_op('>') ->
    '$gt';
operator_to_mongo_op('ge') ->
    '$gte';
operator_to_mongo_op('>=') ->
    '$gte';
operator_to_mongo_op('lt') ->
    '$lt';
operator_to_mongo_op('<') ->
    '$lt';
operator_to_mongo_op('le') ->
    '$lte';
operator_to_mongo_op('<=') ->
    '$lte'.

% Value conversions
-define(GREGORIAN_SECONDS_1970, 62167219200).
datetime_to_now(DateTime) ->
    GSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    ESeconds = GSeconds - ?GREGORIAN_SECONDS_1970,
    {ESeconds div 1000000, ESeconds rem 1000000, 0}.

pack_value(undefined) ->
    <<"">>;
pack_value(null) ->
    <<"">>;
pack_value([]) ->
    <<"">>;
pack_value(Val) when is_integer(Val) ->
    Val;
pack_value(Val) when is_boolean(Val) ->
    Val;
pack_value(Val) when is_binary(Val) ->
    Val;
pack_value(Val) when is_atom(Val) ->
    agb_convertor:to_binary(Val);
pack_value(Val) when is_list(Val) ->
    agb_convertor:to_binary(Val);
pack_value({{Y, M, D}, {_, _, _}} = Val) when is_integer(Y) and is_integer(M) and is_integer(D) ->
    datetime_to_now(Val);
pack_value({Y, M, D} = Val) when is_integer(Y) and is_integer(M) and is_integer(D) ->
    datetime_to_now({Val, {0, 0, 0}});
pack_value(Val) ->
    term_to_binary(Val).