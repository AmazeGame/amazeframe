%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_cluster_memdb_adapter).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").
%% API
-export([
    init/0,
    add_table/4
]).
-export([check_table/1]).
-export([
    size/1]).
-export([
    read/1, read/2,
    read_dirty/2,
    index_read/3
]).
-export([
    write/1,
    write_dirty/1,
    write_single/2
]).
-export([
    update/3,
    update_dirty/3,
    update_counter_dirty/3
]).
-export([
    delete/2,
    delete_dirty/2,
    delete_index/3,
    delete_object/1
]).

-spec init() ->
    supervisor:startchild_ret().
init() ->
    ag_cluster_sup:start_child(ag_cluster_memdb_worker).

-spec(check_table(Table :: atom() | [atom()]) ->
    boolean()).
check_table(Table) when is_atom(Table) ->
    do_check_table(Table);
check_table(Tables) when is_list(Tables) ->
    lists:all(
        fun(Table) ->
            do_check_table(Table)
        end,
        Tables
    ).

do_check_table(Table) ->
    case catch mnesia:table_info(Table, ram_copies) of
        {'EXIT', _Reason} ->
            ?LOG_ERROR("do_check_table Table:~p exit reason:~p~n", [Table, _Reason]),
            false;
        Nodes ->
            lists:member(node(), Nodes)
    end.
%%
%%    lists:foldl(
%%        fun
%%            (Table, true) ->
%%                check_table(Table);
%%            (_Table, false) ->
%%                false
%%        end,
%%        true,
%%        Tables
%%    ).

-spec size(mnesia:table()) ->
    non_neg_integer().
size(Table) ->
    case mnesia:table_info(Table, size) of
        0 ->
            0;
        Size when is_integer(Size) ->
            Size;
        _Any ->
            0
    end.
-spec read(mnesia:table()) ->
    {ok, [tuple()]} | {failed, Reason :: term()}.
read(Table) ->
    ReadFun =
        fun() ->
            qlc:e(qlc:q([X || X <- mnesia:table(Table)]))
        end,
    case mnesia:transaction(ReadFun) of
        {aborted, Reason} ->
            ?LOG_WARNING("read error ~p Table ~p ~n", [Reason, Table]), {failed, Reason};
        {atomic, []} ->
            {ok, []};
        {atomic, Result} ->
            {ok, Result}
    end.

-spec read(mnesia:table(), Key :: term()) ->
    {ok, [tuple()]} | {failed, Reason :: term()}.
read(Table, Key) ->
    ReadFun =
        fun() ->
            mnesia:read({Table, Key})
        end,
    case mnesia:transaction(ReadFun) of
        {aborted, Reason} ->
            ?LOG_WARNING("read error ~p {~p,~p} ~n", [Reason, Table, Key]), {failed, Reason};
        {atomic, []} ->
            {ok, []};
        {atomic, Result} ->
            {ok, Result}
    end.

-spec read_dirty(mnesia:table(), Key :: term()) ->
    {ok, []} | {ok, tuple()}| {ok, [tuple()]}.
read_dirty(Table, Key) ->
    case mnesia:dirty_read({Table, Key}) of
        [] ->
            {ok, []};
        Result ->
            {ok, Result}
    end.

-spec index_read(mnesia:table(), SecondaryKey :: term(), Pos :: non_neg_integer()) ->
    {ok, []} | {ok, tuple()} | {ok, [tuple()]} | {failed, Reason :: term()}.
index_read(Table, SecondaryKey, Pos) ->
    IndexRead =
        fun() ->
            mnesia:index_read(Table, SecondaryKey, Pos)
        end,
    case mnesia:transaction(IndexRead) of
        {aborted, Reason} ->
            ?LOG_WARNING("read_index error ~p Table ~p ~p => ~p~n", [Reason, Table, SecondaryKey, Pos]),
            {failed, Reason};
        {atomic, Result} when is_list(Result) ->
            {ok, Result};
        Result ->
            ?LOG_WARNING("read_index error ~p ~n", [Result]), {failed, Result}
    end.

-spec write_single(Object :: tuple(), CheckFun :: atom()) ->
    ok | {failed, Reason :: term()}.
write_single(Object, CheckFun) ->
    WriteFun =
        fun() ->
            case mnesia:read({erlang:element(1, Object), erlang:element(2, Object)}) of
                [] ->
                    mnesia:write(Object);
                [ReadObject] ->
                    CheckFunResult = CheckFun(ReadObject),
                    if
                        CheckFunResult ->
                            mnesia:write(Object);
                        true ->
                            <<"lockerror">>
                    end
            end
        end,
    case mnesia:transaction(WriteFun) of
        {aborted, Reason} ->
            ?LOG_WARNING("write error ~p Object ~p ~n", [Reason, Object]),
            {failed, Reason};
        {atomic, ok} ->
            ok;
        {atomic, <<"lockerror">>} ->
            {failed, <<"lockerror">>}
    end.

-spec write(Object :: tuple()) ->
    ok | {failed, Reason :: term()}.
write(Object) ->
    WriteFun =
        fun() ->
            mnesia:write(Object)
        end,
    case mnesia:transaction(WriteFun) of
        {aborted, Reason} ->
            ?LOG_WARNING("write error ~p Object ~p ~n", [Reason, Object]), {failed, Reason};
        {atomic, ok} ->
            ok
    end.

-spec write_dirty(Object :: tuple()) ->
    ok.
write_dirty(Object) ->
    mnesia:dirty_write(Object).

-spec update_counter_dirty(Table :: mnesia:table(), Key :: term(), Incr :: integer()) ->
    NewVal :: integer().
update_counter_dirty(Table, Key, Incr) ->
    mnesia:dirty_update_counter(Table, Key, Incr).

%%-spec write(Table::mnesia:table(),TableKey::term(),FieldIndex::non_neg_integer(),Value::term())->
%% ok |{failed,Reason::term()}.
%%write(Table, TableKey, FieldIndex, Value) ->
%%	WriteFun =
%%		fun() ->
%%			case mnesia:read(Table, TableKey) of
%%				[] -> error;
%%				[Term] -> Object = erlang:setelement(FieldIndex, Term, Value),
%%					mnesia:write(Object)
%%			end
%%		end,
%%	case mnesia:transaction(WriteFun) of
%%		{aborted, Reason} ->
%%			?LOG_WARNING("write error ~p Table ~p ~n", [Reason, Table]), {failed, Reason};
%%		{atomic, error} ->
%%			?LOG_WARNING("write error Table ~p ~n", [Table]), {failed, "read table failed when write"};
%%		{atomic, ok} -> ok
%%	end.

%%-spec write(Table::mnesia:table(),TableKey::term(),FieldIndex::non_neg_integer(),
%% FieldKey::term(),FieldTupleValue::[tuple()])->
%% ok |{failed,Reason::term()}.
%%write(Table, TableKey, FieldIndex, FieldKey, FieldTupleValue) ->
%%	WriteFun = fun() ->
%%		case mnesia:read(Table, TableKey) of
%%			[] -> failed;
%%			[Term] -> FieldValues = erlang:element(FieldIndex, Term),
%%				NewFieldValue = case is_tuple(FieldTupleValue) of
%%									true ->
%%										if erlang:element(1, FieldTupleValue) == FieldKey ->
%%											case lists:keyfind(FieldKey, 1, FieldValues) of
%%												false -> FieldValues ++ [FieldTupleValue];
%%												_ -> lists:keyreplace(FieldKey, 1, FieldValues, FieldTupleValue)
%%											end;
%%											true ->
%%												case lists:member(FieldTupleValue, FieldValues) of
%%													false -> FieldValues ++ [FieldTupleValue];
%%													_ -> FieldValues
%%												end
%%										end;
%%									false ->
%%										case lists:member(FieldTupleValue, FieldValues) of
%%											false -> FieldValues ++ [FieldTupleValue];
%%											_ -> FieldValues
%%										end
%%								end,
%%				Object = erlang:setelement(FieldIndex, Term, NewFieldValue),
%%				mnesia:write(Object)
%%		end
%%			   end,
%%	case mnesia:transaction(WriteFun) of
%%		{aborted, Reason} ->
%%			?LOG_WARNING("write error ~p Table ~p ~n", [Reason, Table]), {failed, Reason};
%%		{atomic, failed} ->
%%			?LOG_WARNING("write error Table ~p ~n", [Table]), {failed, "read table failed when write"};
%%		{atomic, ok} -> ok
%%	end.

-spec update(Table :: mnesia:table(), TableKey :: term(), UpdateList :: [tuple()]) ->
    ok | {failed, Reason :: term()}.
update(Table, TableKey, UpdateList) ->
    UpdateFun =
        fun() ->
            case mnesia:read(Table, TableKey) of
                [] ->
                    error;
                [Term] ->
                    Object =
                        lists:foldl(
                            fun({KeyPos, Value}, Acc) ->
                                erlang:setelement(KeyPos, Acc, Value)
                            end,
                            Term,
                            UpdateList
                        ),
                    mnesia:write(Object)
            end
        end,
    case mnesia:transaction(UpdateFun) of
        {aborted, Reason} ->
            ?LOG_WARNING("update error ~p Table ~p ~n", [Reason, Table]), {failed, Reason};
        {atomic, error} ->
            ?LOG_WARNING("update error Table ~p ~n", [Table]), {failed, "read table failed when update"};
        {atomic, ok} ->
            ok
    end.

-spec update_dirty(Table :: mnesia:table(), TableKey :: term(), UpdateList :: [tuple()]) ->
    ok | error.
update_dirty(Table, TableKey, UpdateList) ->
    case mnesia:dirty_read(Table, TableKey) of
        [] ->
            error;
        [Term] ->
            Object =
                lists:foldl(
                    fun({KeyPos, Value}, Acc) ->
                        erlang:setelement(KeyPos, Acc, Value)
                    end,
                    Term,
                    UpdateList
                ),
            mnesia:dirty_write(Object),
            ok
    end.

-spec delete(Table :: mnesia:table(), Key :: term()) ->
    ok | {failed, Reason :: term()}.
delete(Table, Key) ->
    DeleteFun =
        fun() ->
            mnesia:delete({Table, Key})
        end,
    case mnesia:transaction(DeleteFun) of
        {aborted, Reason} ->
            {failed, Reason};
        {atomic, ok} ->
            ok
    end.

-spec delete_dirty(Tab :: mnesia:table(), Key :: term()) ->
    'ok'.
delete_dirty(Table, Key) ->
    mnesia:dirty_delete(Table, Key).

%%-spec delete(Table::mnesia:table(), TableKey::term(),FieldIndex::non_neg_integer(),FieldKey::term()) ->
%% 'ok'|{failed,Reason::term()}.
%%delete(Table, TableKey, FieldIndex, FieldKey) ->
%%	WriteFun = fun() ->
%%		case mnesia:read(Table, TableKey) of
%%			[] -> failed;
%%			[Term] -> FieldValues = erlang:element(FieldIndex, Term),
%%				case lists:keyfind(FieldKey, 1, FieldValues) of
%%					false ->
%%						case lists:member(FieldKey, FieldValues) of
%%							false -> ok;
%%							_ -> FieldValue = lists:delete(FieldKey, FieldValues),
%%								Object = erlang:setelement(FieldIndex, Term, FieldValue),
%%								mnesia:write(Object)
%%						end;
%%					_ -> FieldValue = lists:keydelete(FieldKey, 1, FieldValues),
%%						Object = erlang:setelement(FieldIndex, Term, FieldValue),
%%						mnesia:write(Object)
%%				end
%%		end
%%			   end,
%%	case mnesia:transaction(WriteFun) of
%%		{aborted, Reason} ->
%%			?LOG_WARNING("delete error ~p Table ~p ~n", [Reason, Table]), {failed, Reason};
%%		{atomic, failed} ->
%%			?LOG_WARNING("delete Table ~p ~n", [Table]), {failed, "read table failed when write"};
%%		{atomic, ok} -> ok
%%	end.

-spec delete_object(Object :: tuple()) ->
    ok | {failed, Reason :: term()}.
delete_object(Object) ->
    DeleteFun =
        fun() ->
            mnesia:delete_object(Object)
        end,
    case mnesia:transaction(DeleteFun) of
        {aborted, Reason} ->
            ?LOG_WARNING("delete_object error ~p Object:~p ~n", [Reason, Object]), {failed, Reason};
        {atomic, failed} ->
            ?LOG_WARNING("delete_object Object:~p ~n", [Object]), {failed, "delete_object failed"};
        {atomic, ok} ->
            ok
    end.

-spec delete_index(mnesia:table(), SecondaryKey :: term(), Pos :: non_neg_integer()) ->
    ok | {failed, Reason :: term()}.
delete_index(Table, SecondaryKey, Pos) ->
    DeleteFun =
        fun() ->
            case index_read(Table, SecondaryKey, Pos) of
                {ok, Results} ->
                    lists:foreach(
                        fun(Object) ->
                            mnesia:delete_object(Object)
                        end,
                        Results
                    ),
                    ok;
                {failed, Result} ->
                    ?LOG_WARNING("read_index error ~p when delete_index ~p => ~p ~n", [Result, SecondaryKey, Pos]),
                    {failed, Result}
            end
        end,
    case mnesia:transaction(DeleteFun) of
        {aborted, Reason} ->
            ?LOG_WARNING("delete_index error ~p Table ~p ~n", [Reason, Table]), {failed, Reason};
        {atomic, failed} ->
            ?LOG_WARNING("delete_index Table ~p ~n", [Table]), {failed, "read table failed when write"};
        {atomic, ok} ->
            ok
    end.

%%-spec transaction(Fun::function())-> ok | {ok,Results::term()} | {failed,Reason::term()}.
%%transaction(Fun) ->
%%	case mnesia:transaction(Fun) of
%%		{aborted, Reason} -> ?LOG_WARNING("transaction error ~p~n", [Reason]), {failed, Reason};
%%		{atomic, error} -> ?LOG_WARNING("transaction error2~n"), {failed, "transaction failed"};
%%		{atomic, ok} -> ok;
%%		{atomic, {ok,Results}} -> {ok,Results}
%%	end.
%%
%%-spec activity(Kind::mnesia:activity(),Fun::function())-> {'atomic', Res::term()} | {'aborted', Reason::term()}.
%%activity(Kind,Fun)->
%%	mnesia:activity(Kind,Fun).

-spec add_table(Table :: atom(), Attributes :: [any()], Indices :: list(), integer()) ->
    ok.
add_table(Table, Attributes, Indices, _) ->
    ag_cluster_memdb_worker:add_table(Table, Attributes, Indices).