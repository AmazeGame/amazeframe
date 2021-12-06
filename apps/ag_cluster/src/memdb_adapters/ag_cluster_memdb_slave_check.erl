%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_cluster_memdb_slave_check).

-include_lib("ag_base/include/agb_debuglogger.hrl").

-include("ag_cluster.hrl").
%% API
-export([check/1]).

-spec check(Config :: list()) ->
    list().
check(Config) ->
    MasterNode = ag_cluster_memdb_worker:master(),
    case check_mnesia_running(MasterNode) of
        false ->
            Config;
        true ->
            check_mnesia_table(MasterNode, Config)
    end.

check_mnesia_running(MasterNode) ->
    case mnesia:system_info(is_running) of
        yes ->
            true;
        no ->
            mnesia:start()
    end,
    case lists:member(MasterNode, nodes()) of
        false ->
            false;
        true ->
            ClusterNodes = rpc:call(MasterNode, mnesia, table_info, [schema, ram_copies]),
            case lists:member(node(), ClusterNodes) of
                true ->
                    ClusterNodes2 = mnesia:table_info(schema, ram_copies),
                    case lists:member(MasterNode, ClusterNodes2) of
                        true ->
                            true;
                        false ->
                            {ok, _} = mnesia:change_config(extra_db_nodes, [MasterNode]), true
                    end;
                false ->
                    {ok, _A} = mnesia:change_config(extra_db_nodes, [MasterNode]),
                    true
            end
    end.

check_mnesia_table(_MasterNode, []) ->
    [];
check_mnesia_table(MasterNode, [#table_config{table = Table} = TableConfig | Tail]) ->
    case catch mnesia:table_info(Table, ram_copies) of
        {'EXIT', {aborted, {no_exists, _, _}}} ->
            case rpc:call(MasterNode, mnesia, table_info, [Table, ram_copies]) of
                {badrpc, {'EXIT', {aborted, {no_exists, _, ram_copies}}}} ->
                    [TableConfig | Tail];
                Nodes ->
                    case lists:member(node(), Nodes) of
                        true ->
                            Tail;
                        false ->
                            case mnesia:add_table_copy(Table, node(), ram_copies) of
                                {atomic, ok} ->
                                    Tail;
                                {aborted, {no_exists, {_, cstruct}}} ->
                                    [TableConfig | Tail]
                            end
                    end
            end;
        Nodes ->
            check_mnesia_table(Table, Tail, Nodes)
    end.

check_mnesia_table(Table, Tail, Nodes) ->
    case lists:member(node(), Nodes) of
        true ->
            Tail;
        false ->
            {atomic, ok} = mnesia:add_table_copy(Table, node(), ram_copies),
            Tail
    end.
