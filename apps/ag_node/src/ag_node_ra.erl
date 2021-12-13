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
-module(ag_node_ra).

-include_lib("ag_base/include/agb_debuglogger.hrl").
-include("ag_node.hrl").
%% API
-export([
    watch_pid/1,
    start_node/0,
    get_local_server_id/0,
    trigger_election_self/0,
    add_member/1,
    get_cluster_name/0,
    build_server_id/0,
    check_master_ra_server_start/0,
    get_server_id_by_node/1,
    remove_member/1,
    on_enter_leader/0,
    master/0
]).

%%%===================================================================
%%% API
%%%===================================================================
%% rpc call 函数，从节点启动后要使用rpc call 调用master的 add_member 把节点
%% 添加到集群中
-spec add_member({Name :: atom(), Node :: node()}) ->
    ra_server_proc:ra_cmd_ret() |
    {error, already_member} |
    {error, cluster_change_not_permitted}.
add_member(ServerId) ->
    ClusterName = get_cluster_name(),
    Leader = ra_leaderboard:lookup_leader(ClusterName),
    ra:add_member(Leader, ServerId).

%% rpc call 函数，节点启动从cluster获取ra的leader节点信息后要rpc call check_master_ra_leader
%% 函数判断ra是否是有效的
-spec check_master_ra_server_start() -> boolean().
check_master_ra_server_start() ->
    case ra:overview() of
        #{servers := ServerList} when map_size(ServerList) > 0 ->
            ?LOG_DEBUG("check_master_ra_server_start ServerList:~p~n", [ServerList]),
            true;
        _ ->
            false
    end.

-spec get_local_server_id() -> {atom(), atom()}.
get_local_server_id() ->
    ag_node_variable:getv(server_id).

-spec watch_pid(pid()) ->
    {ok, Reply :: term(), Leader :: {atom(), atom()}} |
    {error, term()} |
    {timeout, {atom(), atom()}}.
watch_pid(Pid) ->
    ServerId = get_local_server_id(),
    ra:process_command(ServerId, {watch, Pid}).

-spec trigger_election_self() -> ok.
trigger_election_self() ->
    ServerId = get_local_server_id(),
    ra:trigger_election(ServerId).

-spec start_node() -> ok | {error, term()}.
start_node() ->
    ClusterName = get_cluster_name(),
    ServerID = get_local_server_id(),
    ra:start_server(default,ClusterName, ServerID, add_machine(), []).

-spec on_enter_leader() -> no_return().
on_enter_leader() ->
    ClusterName = ag_node_ra:get_cluster_name(),
    ag_node_process:watch_node_process(),
    case ra_leaderboard:lookup_leader(ClusterName) of
        MasterId = {_, LeaderNode} when LeaderNode == node() ->
            case ag_node_cluster:get_node_info(ClusterName) of
                undefined ->
                    agb_error:error("ra cluster not reginter ag_cluster");
                #ag_node_info{master_server = {_, Node}} when Node == node() ->
                    ignore;
                NodeInfo = #ag_node_info{master_server = OldServerId = {_, OldNode}, members = Members} ->
                    NewNodes = lists:filter(fun({_, Node}) -> OldNode =/= Node end, Members),
                    ra:remove_member(MasterId, OldServerId),
                    ag_node_cluster:set_node_info(NodeInfo#ag_node_info{master_server = MasterId, members = NewNodes})
            end;
        _ ->
            ignore
    end.

-spec remove_member(undefined|{atom(), atom()}) -> ok.
remove_member(undefined) ->
    ok;
remove_member(ServerId = {_, RNode}) ->
    ClusterName = get_cluster_name(),
    MasterId = ra_leaderboard:lookup_leader(ClusterName),
    NodeInfo = #ag_node_info{members = Members} = ag_node_cluster:get_node_info(ClusterName),
    NewNodes = lists:filter(fun({_, Node}) -> RNode =/= Node end, Members),
    NewNodeInfo = NodeInfo#ag_node_info{members = NewNodes},
    ag_node_cluster:set_node_info(NewNodeInfo),
    Result = ra:remove_member(MasterId, ServerId),
    ?LOG_DEBUG("--------remove_member MasterId:~p result:~p~n", [MasterId, Result]),
    ok.

-spec get_server_id_by_node(atom()) -> undefined | {atom(), atom()}.
get_server_id_by_node(Node) ->
    ClusterName = ag_node_ra:get_cluster_name(),
    case ag_node_cluster:get_node_info(ClusterName) of
        undefined ->
            undefined;
        #ag_node_info{members = Members} ->
            case lists:filter(
                fun({_, N}) -> N == Node end, Members) of
                [] ->
                    undefined;
                [Id | _] ->
                    Id
            end
    end.

-spec(master() -> true| node()).
master() ->
    {_, Master} = ra_leaderboard:lookup_leader(ag_node_ra:get_cluster_name()),
    if
        Master =:= node() ->
            true;
        true ->
            Master
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================

build_server_id() ->
    {get_node_name(), node()}.

get_node_name() ->
    Prefix =
        case application:get_env(ag_node, node_name_prefix) of
            'undefined' ->
                "node";
            {ok, Node} ->
                Node
        end,
    PrefixB = agb_convertor:to_binary(Prefix),
    SnowflakeId = ag_idcreator:gen_newid(snowflake),
    IdBin = agb_convertor:to_binary(SnowflakeId),
    binary_to_atom(<<PrefixB/binary, IdBin/binary>>, utf8).

add_machine() ->
    {module, ag_machine_node, #{}}.

get_cluster_name() ->
    case application:get_env(ag_node, cluster_name) of
        'undefined' ->
            "cluster";
        {ok, Cluster} ->
            Cluster
    end.