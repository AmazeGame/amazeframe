%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_cluster_memdb_master_check).

-include("ag_cluster.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").
%% API
-export([check/1]).

-spec check(Config :: list()) ->
    list().
check(Config) ->
    check_mnesia_running(),
    ?LOG_WARNING("~p ADD_TABLE warning : ~p is duplicate~n", [?MODULE, Config]),
    check_mnesia_tables(Config).

check_mnesia_running() ->
    case mnesia:system_info(is_running) of
        yes ->
            true;
        no ->
            mnesia:start()
    end.

check_mnesia_tables([]) ->
    [];
check_mnesia_tables([#table_config{table = Table, attribute = Attributes, index = Indices} = TabConfig | Tail]) ->
    case catch mnesia:table_info(Table, ram_copies) of
        {'EXIT', {aborted, {no_exists, _, _}}} ->
            ?LOG_INFO("check mnesia ~p indices:~p~n", [Table, Indices]),
            case mnesia:create_table(Table, [{ram_copies, [node()]}, {attributes, Attributes}, {index, Indices}]) of
                {atomic, ok} ->
                    Tail;
                {aborted, {already_exists, _}} ->
                    Attributes = mnesia:table_info(Table, attributes),
                    Indices = mnesia:table_info(Table, index),
                    [TabConfig | Tail]
            end;
        {aborted, {already_exists, _}} ->
            ?LOG_INFO("~p ADD_TABLE warning : ~p is duplicate~n", [?MODULE, Table]),
            Attributes = mnesia:table_info(Table, attributes),
            Indices = mnesia:table_info(Table, index),
            [TabConfig | Tail];
        _Nodes ->
            ?LOG_INFO("~p ADD_TABLE warning : ~p is duplicate~n", [?MODULE, _Nodes]),
            Tail
    end.


