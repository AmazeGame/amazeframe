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

-module(agdb_database_adapter_mysql).

-behaviour(agdb_database_adapter).

-include_lib("ag_base/include/agb_debuglogger.hrl").

-export([connect/2]).
-export([find_one/3, find_one/4]).
-export([find/3, find/4]).
-export([find_bag/4]).
-export([count/3]).
-export([insert/3]).
-export([replace/3]).
-export([update/4]).
-export([update_one_bagobj/6]).
-export([add_one_bagobj/5]).
-export([remove_bagobj/5]).
-export([delete_bagobj/4]).
-export([delete/3]).
-export([insert_id/3]).
-export([query/3]).
-export([flush_db/1]).
-export([
    make_insert_sql/2,
    make_replace_sql/2,
    make_update_sql/3,
    make_delete_sql/2,
    make_count_sql/2,
    make_query_one_sql/3,
    make_query_many_sql/3,
    make_where_string/1,
    make_update_set_string/1
]).

-spec connect(atom(), tuple()) ->
    ok | {error, Reason :: term()}.
connect(PoolName, {PoolArgs, WorkerArgs}) ->
    ?LOG_DEBUG("gen_db_adapter_mysql connect ~p~n", [{PoolArgs, WorkerArgs}]),
    PoolArgs1 = [{strategy, fifo}, {name, {local, PoolName}}, {worker_module, mysql} | PoolArgs],
    case ag_database_sup:start_child(poolboy:child_spec(PoolName, PoolArgs1, WorkerArgs)) of
        {ok, _PID} ->
            ok;
        {error, {already_started, _PID}} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec insert(atom(), binary(), map() | [map()]) ->
    ok | {error, Reason :: term()}.
insert(PoolName, Table, InsertObject) when is_map(InsertObject) ->
    insert(PoolName, Table, [InsertObject]);
insert(PoolName, Table, InsertObjects) when is_list(InsertObjects) ->
    {Sql, Params} = make_insert_sql(Table, InsertObjects),
    query(PoolName, Sql, Params).

-spec replace(atom(), binary(), map() | [map()]) ->
    ok | {error, Reason :: term()}.
replace(PoolName, Table, InsertObject) when is_map(InsertObject) ->
    replace(PoolName, Table, [InsertObject]);
replace(PoolName, Table, InsertObjects) when is_list(InsertObjects) ->
    {Sql, Params} = make_replace_sql(Table, InsertObjects),
    query(PoolName, Sql, Params).

-spec update(atom(), binary(), map() | [tuple()], map() | [tuple()]) ->
    ok | {error, Reason :: term()}.
update(PoolName, Table, Where, Update) ->
    {Sql, Params} = make_update_sql(Table, Where, Update),
    query(PoolName, Sql, Params).

-spec update_one_bagobj(atom(), binary(), binary(), map() | [tuple()], map() | [tuple()], map() | [tuple()]) ->
    ok | {error, Reason :: term()}.
update_one_bagobj(PoolName, Table, BagName, _, Owner, Update) ->
    NewUpdate = maps:merge(Owner, Update),
    NewTable = make_bagtable(Table, BagName),
    replace(PoolName, NewTable, NewUpdate).

-spec add_one_bagobj(atom(), binary(), binary(), map() | [tuple()], map() | [tuple()]) ->
    ok | {error, Reason :: term()}.
add_one_bagobj(PoolName, Table, BagName, Owner, Update) ->
    NewUpdate = maps:merge(Owner, Update),
    NewTable = make_bagtable(Table, BagName),
    replace(PoolName, NewTable, NewUpdate).

-spec delete(atom(), binary(), map() | [tuple()]) ->
    ok | {error, Reason :: term()}.
delete(PoolName, Table, Where) ->
    {Sql, Params} = make_delete_sql(Table, Where),
    query(PoolName, Sql, Params).

-spec remove_bagobj(atom(), binary(), binary(), map() | [tuple()], map()) ->
    ok | {error, Reason :: term()}.
remove_bagobj(PoolName, Table, BagName, Where, _) ->
    NewTable = make_bagtable(Table, BagName),
    {Sql, Params} = make_delete_sql(NewTable, Where),
    query(PoolName, Sql, Params).

-spec delete_bagobj(atom(), binary(), binary(), map() | [tuple()]) ->
    ok | {error, Reason :: term()}.
delete_bagobj(PoolName, Table, BagName, Where) ->
    NewTable = make_bagtable(Table, BagName),
    {Sql, Params} = make_delete_sql(NewTable, Where),
    query(PoolName, Sql, Params).

-spec count(atom(), binary(), map() | [tuple()]) ->
    Count :: integer() | {error, Reason :: term()}.
count(PoolName, Table, Where) ->
    {Sql, Params} = make_count_sql(Table, Where),
    case query(PoolName, Sql, Params) of
        {ok, _, [[Count]]} ->
            Count;
        {error, Reason} ->
            {error, Reason}
    end.

-spec find_one(atom(), binary(), map()|[tuple()]) ->
    undefined | map() | {error, Reason :: term()}.
find_one(PoolName, Table, Where) ->
    find_one(PoolName, Table, Where, []).

-spec find_one(atom(), binary(), map() | [tuple()], list()) ->
    undefined | map() | {error, Reason :: term()}.
find_one(PoolName, Table, Where, Fields) ->
    {Sql, Params} = make_query_one_sql(Table, Where, Fields),
    case query(PoolName, Sql, Params) of
        {ok, RFields, RValues} ->
            case make_results(RFields, RValues) of
                [] ->
                    undefined;
                [Obj] ->
                    Obj
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec find(atom(), binary(), map() | [tuple()]) ->
    list() | {error, Reason :: term()}.
find(PoolName, Table, Where) ->
    find(PoolName, Table, Where, []).

-spec find(atom(), binary(), map()|[tuple()], list()) ->
    list() | {error, Reason :: term()}.
find(PoolName, Table, Where, Fields) ->
    {Sql, Params} = make_query_many_sql(Table, Where, Fields),
    case query(PoolName, Sql, Params) of
        {ok, RFields, RValues} ->
            make_results(RFields, RValues);
        {error, Reason} ->
            {error, Reason}
    end.

-spec find_bag(atom(), binary(), binary(), map()|[tuple()]) ->
    list() | {error, Reason :: term()}.
find_bag(PoolName, Table, BagName, Where) ->
    NewTable = make_bagtable(Table, BagName),
    {Sql, Params} = make_query_many_sql(NewTable, Where, []),
    case query(PoolName, Sql, Params) of
        {ok, RFields, RValues} ->
            make_results(RFields, RValues);
        {error, Reason} ->
            {error, Reason}
    end.

-spec insert_id(atom(), binary(), map()) ->
    {ok, integer()} | {error, Reason :: term(), integer()}.
insert_id(PoolName, Table, Object) when is_map(Object) ->
    {Sql, Params} = make_insert_sql(Table, [Object]),
    Fun =
        fun(MysqlCon, Args) ->
            {mysql:query(MysqlCon, Sql, Args), mysql:insert_id(MysqlCon)}
        end,
    LogFun =
        fun(R) ->
            ?LOG_ERROR(">>>>>>>>>>>>>>agdb_database_adapter_mysql insert_id Sql:[~p],Params:[~p] error:[~p]~n",
                [Sql, Params, R])
        end,
    case mysql_poolboy:transaction(PoolName, Fun, [Params]) of
        {atomic, {ok, ID}} ->
            {ok, ID};
        {atomic, {error, Reason}} ->
            LogFun(Reason),
            {error, Reason};
        {aborted, Reason} ->
            LogFun(Reason),
            {error, Reason};
        Reason ->
            LogFun(Reason),
            {error, Reason}
    end.

-spec flush_db(atom()) ->
    list().
flush_db(PoolName) ->
    {ok, _, TableNames} = query(PoolName, <<"show tables;">>, []),
    lists:foldl(
        fun([TableName], Acc) ->
            Result = query(PoolName, agb_string:sprintf("truncate table ~s;", [TableName]), []),
            [{TableName, Result} | Acc]
        end,
        [],
        TableNames
    ).

-spec make_insert_sql(binary(), [maps:map()]) ->
    {string(), [term()]}.
make_insert_sql(Table, [O | _] = InsertObjects) ->
    KeyBins = binary:list_to_bin(lists:join(<<", ">>, lists:map(
        fun(K1) ->
            K = agb_convertor:to_binary(K1),
            <<$`, K/binary, $`>>
        end,
        maps:keys(O))
    )),
    FiledStrs = <<$(, KeyBins/binary, $)>>,
    {ValueBins, ValueLists} =
        lists:foldl(
            fun
                (InsertObject, {ObjValueBins, ObjValueSum}) ->
                    {ObjValueBinList, ObjPacketValues} =
                        lists:foldl(
                            fun(V, {ValueBinSum, PackValueSum}) ->
                                {[<<"?">> | ValueBinSum], [pack_value(V) | PackValueSum]}
                            end,
                            {[], []},
                            maps:values(InsertObject)
                        ),
                    ObjValueStrs = binary:list_to_bin(lists:join(<<", ">>, ObjValueBinList)),
                    ObjValueBin = <<$(, ObjValueStrs/binary, $)>>,
                    {[ObjValueBin | ObjValueBins], [lists:reverse(ObjPacketValues) | ObjValueSum]}
            end,
            {[], []},
            InsertObjects
        ),
    ValueStrs = binary:list_to_bin(lists:join(<<", ">>, ValueBins)),
    Sql = agb_string:sprintf("insert into ~s ~s values ~s", [agb_convertor:to_binary(Table), FiledStrs, ValueStrs]),
    Params = lists:concat(lists:reverse(ValueLists)),
    ?LOG_DEBUG("============>insert sql:~p params:~p~n", [Sql, Params]),
    {Sql, Params}.

-spec make_replace_sql(binary(), [maps:map()]) ->
    {string(), [term()]}.
make_replace_sql(Table, [O | _] = InsertObjects) ->
    KeyBins = binary:list_to_bin(lists:join(<<", ">>, lists:map(
        fun(K1) ->
            K = agb_convertor:to_binary(K1),
            <<$`, K/binary, $`>>
        end,
        maps:keys(O))
    )),
    FiledStrs = <<$(, KeyBins/binary, $)>>,
    {ValueBins, ValueLists} =
        lists:foldl(
            fun(InsertObject, {ObjValueBins, ObjValueSum}) ->
                ObjValues = maps:values(InsertObject),
                {ObjValueBinList, ObjPacketValues} =
                    lists:foldl(
                        fun(V, {ValueBinSum, PackValueSum}) ->
                            {[<<"?">> | ValueBinSum], [pack_value(V) | PackValueSum]}
                        end,
                        {[], []},
                        ObjValues
                    ),
                ObjValueStrs = binary:list_to_bin(lists:join(<<", ">>, ObjValueBinList)),
                ObjValueBin = <<$(, ObjValueStrs/binary, $)>>,
                {[ObjValueBin | ObjValueBins], [lists:reverse(ObjPacketValues) | ObjValueSum]}
            end,
            {[], []},
            InsertObjects
        ),
    ValueStrs = binary:list_to_bin(lists:join(<<", ">>, ValueBins)),
    Sql = agb_string:sprintf("replace into ~s ~s values ~s", [agb_convertor:to_binary(Table), FiledStrs, ValueStrs]),
    Params = lists:concat(lists:reverse(ValueLists)),
    ?LOG_DEBUG("============>replace sql:~p params:~p~n", [Sql, Params]),
    {Sql, Params}.

-spec make_update_sql(binary(), map() | [tuple()], map() | [tuple()]) ->
    {string(), [term()]}.
make_update_sql(Table, Where, Update) ->
    {WhereString, WhereValues} = make_where_string(Where),
    {SetString, SetValues} = make_update_set_string(Update),
    Sql = agb_string:sprintf("update ~s set ~s ~s", [agb_convertor:to_binary(Table), SetString, WhereString]),
    Params = lists:concat([SetValues, WhereValues]),
    ?LOG_DEBUG("============>update sql:~p params:~p~n", [Sql, Params]),
    {Sql, Params}.

-spec make_delete_sql(binary(), map() | [tuple()]) ->
    {string(), [term()]}.
make_delete_sql(Table, Where) ->
    {WhereString, WhereValues} = make_where_string(Where),
    Sql = agb_string:sprintf("delete from ~s ~s", [agb_convertor:to_binary(Table), WhereString]),
    ?LOG_DEBUG("============>update sql:~p params:~p~n", [Sql, WhereValues]),
    {Sql, WhereValues}.

-spec make_count_sql(binary(), map() | [tuple()]) ->
    {string(), [term()]}.
make_count_sql(Table, Where) ->
    {WhereString, WhereValues} = make_where_string(Where),
    Sql = agb_string:sprintf("select count(*) from ~s ~s", [agb_convertor:to_binary(Table), WhereString]),
    ?LOG_DEBUG("============>query count sql:~p params:~p~n", [Sql, WhereValues]),
    {Sql, WhereValues}.

-spec make_query_one_sql(binary(), map() | [tuple()], list()) ->
    {string(), [term()]}.
make_query_one_sql(Table, Where, Fields) ->
    {WhereString, WhereValues} = make_where_string(Where),
    FieldString = pack_filed(Fields),
    TableBin = agb_convertor:to_binary(Table),
    Sql = agb_string:sprintf("select ~s from ~s ~s limit 1", [FieldString, TableBin, WhereString]),
    ?LOG_DEBUG("============>query one sql:~p params:~p~n", [Sql, WhereValues]),
    {Sql, WhereValues}.

pack_filed([]) ->
    <<"*">>;
pack_filed(Fields) ->
    binary:list_to_bin(lists:join(<<",">>, lists:map(
        fun(K) ->
            <<$`, (agb_convertor:to_binary(K))/binary, $`>>
        end, Fields
    ))).

-spec make_query_many_sql(binary(), map() | [tuple()], list()) ->
    {string(), [term()]}.
make_query_many_sql(Table, Where, Fields) ->
    {WhereString, WhereValues} = make_where_string(Where),
    FieldString = pack_filed(Fields),
    Sql = agb_string:sprintf("select ~s from ~s ~s", [FieldString, agb_convertor:to_binary(Table), WhereString]),
    ?LOG_DEBUG("============>query many sql:~p params:~p~n", [Sql, WhereValues]),
    {Sql, WhereValues}.

-spec make_where_string(map() | [any()]) ->
    {binary(), [any()]}.
make_where_string(Where) when is_map(Where) ->
    make_where_string(maps:to_list(Where));
make_where_string([]) ->
    {<<" ">>, []};
make_where_string(Where) ->
    {WhereBinList, ValueList} = lists:foldl(
        fun
            ({K, V}, {WhereSum, ValueSum}) when V == undefined; V == null ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " is ?">> | WhereSum], [<<"null">> | ValueSum]};
            ({K, V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " = ?">> | WhereSum], [pack_value(V) | ValueSum]};
            ({K, 'equals', V}, {WhereSum, ValueSum}) when V == undefined; V == null ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " is ?">> | WhereSum], [<<"null">> | ValueSum]};
            ({K, 'equals', V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " = ?">> | WhereSum], [pack_value(V) | ValueSum]};
            ({K, '=', V}, {WhereSum, ValueSum}) when V == undefined; V == null ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " is ?">> | WhereSum], [<<"null">> | ValueSum]};
            ({K, '=', V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " = ?">> | WhereSum], [pack_value(V) | ValueSum]};
            ({K, 'not_equals', V}, {WhereSum, ValueSum}) when V == undefined; V == null ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " is not ?">> | WhereSum], [<<"null">> | ValueSum]};
            ({K, 'not_equals', V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " != ?">> | WhereSum], [pack_value(V) | ValueSum]};
            ({K, '!=', V}, {WhereSum, ValueSum}) when V == undefined; V == null ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " is not ?">> | WhereSum], [<<"null">> | ValueSum]};
            ({K, '!=', V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " != ?">> | WhereSum], [pack_value(V) | ValueSum]};
            ({K, 'gt', V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " > ?">> | WhereSum], [pack_value(V) | ValueSum]};
            ({K, '>', V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " > ?">> | WhereSum], [pack_value(V) | ValueSum]};
            ({K, 'lt', V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " < ?">> | WhereSum], [pack_value(V) | ValueSum]};
            ({K, '<', V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " < ?">> | WhereSum], [pack_value(V) | ValueSum]};
            ({K, 'ge', V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " >= ?">> | WhereSum], [pack_value(V) | ValueSum]};
            ({K, '>=', V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " >= ?">> | WhereSum], [pack_value(V) | ValueSum]};
            ({K, 'le', V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " <= ?">> | WhereSum], [pack_value(V) | ValueSum]};
            ({K, '<=', V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " <= ?">> | WhereSum], [pack_value(V) | ValueSum]};
            ({K, 'like', V}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " like ?">> | WhereSum],
                    [<<"%", (pack_value(V))/binary, "%">> | ValueSum]};
            ({K, 'in', Vs}, {WhereSum, ValueSum}) when is_list(Vs) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " in ", (pack_in(Vs))/binary>> | WhereSum],
                        lists:reverse(Vs) ++ ValueSum};
            ({K, 'in', {Min, Max}}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " <= ?">>,
                    <<$`, (agb_convertor:to_binary(K))/binary, $`, " >= ?">> | WhereSum],
                    [pack_value(Max), pack_value(Min) | ValueSum]};
            ({K, 'not_in', Vs}, {WhereSum, ValueSum}) when is_list(Vs) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " not in ", (pack_in(Vs))/binary>> | WhereSum],
                        lists:reverse(Vs) ++ ValueSum};
            ({K, 'not_in', {Min, Max}}, {WhereSum, ValueSum}) ->
                {[<<$`, (agb_convertor:to_binary(K))/binary, $`, " > ?">>,
                    <<$`, (agb_convertor:to_binary(K))/binary, $`, " < ?">> | WhereSum],
                    [pack_value(Max), pack_value(Min) | ValueSum]}
        end, {[], []}, Where),
    WhereAnd = binary:list_to_bin(lists:join(<<" and ">>, lists:reverse(WhereBinList))),
    {<<"where ", WhereAnd/binary>>, lists:reverse(ValueList)}.

pack_in(Vs) ->
    Rs = binary:list_to_bin(lists:join(<<",">>, lists:map(fun(_) ->
        <<"?">> end, Vs))),
    <<$(, Rs/binary, $)>>.

-spec make_update_set_string(map() | [any()]) ->
    {binary(), [any()]}.
make_update_set_string(Updates) when is_map(Updates) ->
    make_update_set_string(maps:to_list(Updates));
make_update_set_string(Updates) when is_list(Updates) ->
    {KeyBinList, ValueList} =
        lists:foldl(
            fun({Key, Val}, {KeySum, ValSum}) ->
                {[<<$`, (agb_convertor:to_binary(Key))/binary, $`, " = ?">> | KeySum], [pack_value(Val) | ValSum]}
            end,
            {[], []},
            Updates
        ),
    {binary:list_to_bin(lists:join(<<" , ">>, lists:reverse(KeyBinList))), lists:reverse(ValueList)}.

pack_value(undefined) ->
    <<"">>;
pack_value(null) ->
    <<"">>;
pack_value("undefined") ->
    <<"">>;
pack_value("null") ->
    <<"">>;
pack_value([]) ->
    <<"">>;
pack_value(Val) when is_integer(Val) ->
    Val;
pack_value(Val) when is_binary(Val) ->
    Val;
pack_value(Val) when is_atom(Val) ->
    agb_convertor:to_binary(Val);
pack_value(Val) when is_list(Val) ->
    agb_convertor:to_binary(Val);
pack_value({_, _, _} = Val) ->
    agb_convertor:to_binary(pack_date(Val));
pack_value({{_, _, _}, {_, _, _}} = Val) ->
    agb_convertor:to_binary(pack_datetime(Val));
pack_value(Val) ->
    term_to_binary(Val).

pack_date(Date) ->
    agdb_format_date:format("Y-m-d", {Date, {0, 0, 0}}).

pack_datetime(DateTime) ->
    agdb_format_date:format("Y-m-d H:i:s", DateTime).

make_results(_, []) ->
    [];
make_results(_, [[]]) ->
    [];
make_results(RFields, RValues) ->
    lists:map(fun(RVList) ->
        normalize_object(maps:from_list(lists:zip(RFields, RVList))) end, RValues).

normalize_object(Map) ->
    maps:map(fun(_Key, Val) when Val == null ->
        <<"">>; (_, Val) ->
        Val end, Map).

make_bagtable(Table, BagName) ->
    <<(agb_convertor:to_binary(Table))/binary, $_, (agb_convertor:to_binary(BagName))/binary>>.

-spec query(atom(), iolist()|iodata(), list()) ->
    mysql:query_result().
query(PoolName, Sql, Params) ->
    case mysql_poolboy:query(PoolName, Sql, Params) of
        {error, Reason} ->
            ?LOG_ERROR(">>>>>>>>>>>>agdb_database_adapter_mysql query error:[~p],Sql:[~p],Params:[~p]~n",
                [Sql, Params, Reason]),
            {error, Reason};
        Result ->
            Result
    end.

