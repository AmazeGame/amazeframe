%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_cluster).

-include_lib("ag_base/include/agb_debuglogger.hrl").

%% API
-export([
    init/0,
    check_cluster/0
]).

-export([
    register_as_gate_manager/1,
    register_as_playeragent_manager/1
]).
-export([
    increase_gate/1,
    decrease_gate/1
]).
-export([
    register_player/9, register_player/1,
    unregister_player/1
]).
-export([update_online_player_info/3, update_online_player_info/4]).
-export([gate_offline/0]).
-export([
    player_from_id/1,
    player_from_session/1,
    player_from_archive/1
]).
-export([
    get_temporary_security/1,
    put_temporary_security/2
]).

-include("ag_engine.hrl").

-spec init() ->
    ok.
init() ->
    ag_cluster_manager:add_table(gate_node, record_info(fields, gate_node), [], 0),
    ag_cluster_manager:add_table(agent_node, record_info(fields, agent_node), [], 0),
    ag_cluster_manager:add_table(temporary_security, record_info(fields, temporary_security), [], 0),
    ag_cluster_manager:add_table(online_player, record_info(fields, online_player), get_online_player_index(), get_ttl()),
    ok.

-spec check_cluster() ->
    boolean().
check_cluster() ->
    ag_cluster_manager:check_table([online_player, gate_node, agent_node]).

register_as_gate_manager(Node) ->
    ag_cluster_manager:write_dirty(#gate_node{node = Node, gate_count = 0}),
    ok.

register_as_playeragent_manager(Node) ->
    ag_cluster_manager:write_dirty(#agent_node{node = Node, agent_count = 0}).

increase_gate(Node) ->
    ag_cluster_manager:update_counter_dirty(gate_node, Node, 1),
    ok.

decrease_gate(Node) ->
    ag_cluster_manager:update_counter_dirty(gate_node, Node, -1),
    ok.

gate_offline() ->
    ag_cluster_manager:update_counter_dirty(gate_node, node(), -1),
    ok.

-spec register_player(Id, IdType, RWPid, RWNode, GPid, GNode, Session, Security, Archive) ->
    ok | no_return() when
    Id :: ag_game_id(),
    IdType :: ag_game_id(),
    RWPid :: pid()|disabled|undefined,
    RWNode :: node()|disabled,
    GPid :: pid()|undefined,
    GNode :: node(),
    Session :: ag_game_id(),
    Security :: ag_game_id(),
    Archive :: ag_game_id().
register_player(Id, IdType, RWPid, RWNode, GPid, GNode, Session, Security, Archive) ->
    PlayerObject = #online_player{
        id = Id,
        idtype = IdType,
        agent_pid = RWPid,
        agent_node = RWNode,
        gate_pid = GPid,
        gate_node = GNode,
        session = Session,
        security = Security,
        archive = Archive,
        login_time = agb_time:get_utc_time_seconds()
    },
    %PlayerObject.
    register_player(PlayerObject).

-spec register_player(tuple()) ->
    ok | no_return().
register_player(PlayerObject) ->
    case ag_cluster_manager:write_single(PlayerObject, {<<"login_time">>, <<"<">>, agb_time:get_utc_time_seconds() - 1}) of
        ok ->
            ok;
        Reason ->
            agb_error:error("register_player error reason:~p~n", [Reason])
    end.

-spec unregister_player(ag_game_id() | undefined) ->
    ok.
unregister_player(Id) when Id == undefined ->
    ag_cluster_manager:update_counter_dirty(agent_node, node(), -1),
    ok;
unregister_player(Id) ->
    ag_cluster_manager:delete_dirty(online_player, Id),
    ag_cluster_manager:update_counter_dirty(agent_node, node(), -1),
    ok.

-spec player_from_session(ag_game_id()| undefined) ->
    undefined | tuple().
player_from_session(undefined) -> undefined;
player_from_session(Session) ->
    case ag_cluster_manager:index_read(online_player, Session, #online_player.session) of
        {ok, [PlayerInfo]} ->
            case validate_session(PlayerInfo) of
                true ->
                    PlayerInfo;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

-spec player_from_archive(ag_game_id() | undefined) ->
    undefined | tuple().
player_from_archive(undefined) ->
    undefined;
player_from_archive(Archive) ->
    case ag_cluster_manager:index_read(online_player, Archive, #online_player.archive) of
        {ok, [PlayerInfo]} ->
            case validate_session(PlayerInfo) of
                true ->
                    PlayerInfo;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

-spec player_from_id(ag_game_id()) ->
    undefined | tuple().
player_from_id(Id) ->
    case ag_cluster_manager:read_dirty(online_player, Id) of
        {ok, [PlayerInfo]} ->
            case validate_session(PlayerInfo) of
                true ->
                    PlayerInfo;
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

validate_session(#online_player{agent_node = disabled, agent_pid = disabled}) ->
    true;
validate_session(#online_player{agent_node = RWNode, agent_pid = RWPid}) ->
    case RWNode =:= node() of
        true ->
            erlang:is_process_alive(RWPid);
        false ->
            rpc:call(RWNode, erlang, is_process_alive, [RWPid])
    end.

get_temporary_security(Id) ->
    case ag_cluster_manager:read(temporary_security, Id) of
        {ok, [#temporary_security{security = TemporarySecurity}]} ->
            TemporarySecurity;
        _ ->
            undefined
    end.

put_temporary_security(Id, TemporarySecurity) ->
    ag_cluster_manager:write_dirty(#temporary_security{id = Id, security = TemporarySecurity}).

-spec update_online_player_info(Id :: ag_game_id(), GatePid :: pid()|undefined, GateNode :: node()|undefined) ->
    ok | error.
update_online_player_info(Id, GatePid, GateNode) ->
    ?LOG_DEBUG("update_online_player_info ~p~n", [{Id, GatePid, GateNode}]),
    UpdateList = [{#online_player.gate_pid, GatePid}, {#online_player.gate_node, GateNode}],
    ag_cluster_manager:update_dirty(online_player, Id, UpdateList).

-spec update_online_player_info(Id, GatePid, GateNode, Session) -> ok | error when
    Id :: ag_game_id(),
    GatePid :: pid()|undefined,
    GateNode :: node()|undefined,
    Session :: ag_game_id().
update_online_player_info(Id, GatePid, GateNode, Session) ->
    ?LOG_DEBUG("update_online_player_info ~p~n", [{Id, GatePid, GateNode, Session}]),
    UpdateList =
        [
            {#online_player.gate_pid, GatePid},
            {#online_player.gate_node, GateNode},
            {#online_player.session, Session}
        ],
    ag_cluster_manager:update_dirty(online_player, Id, UpdateList).

get_ttl() ->
    case application:get_env(ag_engine, is_use_roleworker) of
        {ok, true} ->
            0;
        {ok, false} ->
            case application:get_env(ag_engine, session_timeout) of
                undefined ->
                    300;
                {ok, TimeOut} ->
                    TimeOut div 1000
            end
    end.

get_online_player_index() ->
    case application:get_env(ag_engine, is_use_roleworker) of
        {ok, true} ->
            [session, agent_pid, agent_node, archive];
        {ok, false} ->
            [session, archive]
    end.