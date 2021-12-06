%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.28
%%%-------------------------------------------------------------------
%%%
-module(ag_node_cluster).

-include("ag_node.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").

%% API
-export([
    init/0,
    get_node_info/1,
    set_node_info/1
]).

%%%===================================================================
%%% API
%%%===================================================================
-spec init() -> ok.
init() ->
    ag_cluster_manager:add_table(ag_node_info, record_info(fields, ag_node_info), [], 0),
    ok.

-spec get_node_info(ClusterName::atom()|string()) -> undefined|tuple().
get_node_info(ClusterName) ->
    case ag_cluster_manager:read(ag_node_info, ClusterName) of
        {ok, []} ->
            undefined;
        {ok, [NodeInfo]} ->
            ?LOG_DEBUG("ag_node_cluster get_node_info:~p~n", [NodeInfo]),
            NodeInfo;
        {failed, Reason} ->
            agb_error:error("ag_node get node info error:~p~n", [Reason])
    end.

-spec set_node_info(tuple()) -> ok.
set_node_info(NodeInfo) ->
    ?LOG_DEBUG("ag_node_cluster set_node_info:~p~n", [NodeInfo]),
    ag_cluster_manager:write(NodeInfo).
%%%===================================================================
%%% Internal functions
%%%===================================================================