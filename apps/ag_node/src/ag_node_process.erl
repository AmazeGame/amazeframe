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
-module(ag_node_process).


-behaviour(gen_server).

-include_lib("ag_base/include/agb_debuglogger.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    watch_node_process/0,
    register_state_change_event/1,
    unregister_state_change_event/1
]).

-define(SERVER, ?MODULE).
-define(MAX_RETRY_NUM, 3).
-define(WATCH_NODE_PROCESS, watch_node_process).
-define(REGISTER_STATE_CHANGE_EVENT, register_state_change_event).
-define(UNREGISTER_STATE_CHANGE_EVENT, unregister_state_change_event).
-define(CHECK_STATE_TIME, check_state_time).
-define(CHECK_NODE_INFO, 'CHECK_NODE_INFO').

-include("ag_node.hrl").

-record(state, {
    start_ra_server = false,
    ready = false,
    retry_num = 0,
    node_state = undefined :: undefined | master | slave,
    state_change_event_accepts = [] :: [pid()]
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec watch_node_process() -> ok.
watch_node_process() ->
    gen_server:cast(?MODULE, ?WATCH_NODE_PROCESS).

-spec register_state_change_event(pid()) -> ok.
register_state_change_event(AcceptPid) ->
    gen_server:cast(?MODULE, {?REGISTER_STATE_CHANGE_EVENT, AcceptPid}).

-spec unregister_state_change_event(pid()) -> ok.
unregister_state_change_event(AcceptPid) ->
    gen_server:cast(?MODULE, {?UNREGISTER_STATE_CHANGE_EVENT, AcceptPid}).

%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    process_flag(trap_exit, true),
    ServerId = ag_node_ra:build_server_id(),
    ag_node_variable:put(server_id,ServerId),
    self() ! ?CHECK_NODE_INFO,
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(?WATCH_NODE_PROCESS, State) ->
    ag_node_ra:watch_pid(self()),
    {noreply, State};
handle_cast({?REGISTER_STATE_CHANGE_EVENT, Pid},
    State = #state{state_change_event_accepts = Accepts, node_state = NodeState}) ->
    case lists:member(Pid, Accepts) of
        true ->
            {noreply, State};
        false ->
            Pid ! {?NODE_STATE_CHANGE_EVENT, NodeState},
            {noreply, State#state{state_change_event_accepts = [Pid | Accepts]}}
    end;
handle_cast({?UNREGISTER_STATE_CHANGE_EVENT, Pid},
    State = #state{state_change_event_accepts = Accepts}) ->
    {noreply, State#state{state_change_event_accepts = lists:delete(Pid, Accepts)}};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(?CHECK_NODE_INFO, State = #state{retry_num = RNum}) when RNum > ?MAX_RETRY_NUM ->
    agb_error:error("CHECK_NODE_INFO retry max count"),
    {noreply, State};
handle_info(?CHECK_NODE_INFO, State) ->
    ClusterName = ag_node_ra:get_cluster_name(),
    case ag_node_cluster:get_node_info(ClusterName) of
        undefined ->
            start_ra_master(State);
        NodeInfo ->
            ?LOG_INFO("check_node:~p~n",[NodeInfo]),
            case check_node(NodeInfo) of
                master_ready ->
                    ?LOG_INFO("master_ready~n"),
                    NewState = start_ra_salve(State),
                    add_to_ra_cluster(NodeInfo, NewState);
                start_master ->
                    ?LOG_INFO("start_master~n"),
                    start_ra_master(State);
                wait_master ->
                    ?LOG_INFO("wait_master~n"),
                    erlang:send_after(1000, self(), ?CHECK_NODE_INFO),
                    {noreply, State}
            end
    end;
handle_info(?CHECK_STATE_TIME, State = #state{node_state = NodeState}) ->
    NewState =
        case ag_node_ra:master() of
            true when NodeState == master ->
                State;
            true ->
                State1 = State#state{node_state = master},
                send_state_change_event(State1),
                State1;
            _ when NodeState == slave ->
                State;
            _ ->
                State1 = State#state{node_state = slave},
                send_state_change_event(State1),
                State1
        end,
    erlang:send_after(1000, self(), ?CHECK_STATE_TIME),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(Reason, _State) ->
    ?LOG_ERROR("ag_node_connect_process terminate reason:~p~n", [Reason]),
    ServerId = ag_node_ra:get_local_server_id(),
    ag_node_ra:remove_member(ServerId),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_node(#ag_node_info{master_server = {_, MasterNode}, members = Members}) ->
    ?LOG_INFO("check_node:master-> ~p~n",[MasterNode]),
    case net_kernel:connect_node(MasterNode) of
        true ->
            ?LOG_DEBUG("check_node node:~p is connect~n", [MasterNode]),
            case rpc:call(MasterNode, ag_node_ra, check_master_ra_server_start, []) of
                true ->
                    master_ready;
                false ->
                    start_master
            end;
        false ->
            case lists:any(fun({_, N}) -> (net_kernel:connect_node(N)) and (N =/= node()) end, Members) of
                true ->
                    wait_master;
                false ->
                    start_master
            end
    end.

add_to_ra_cluster(#ag_node_info{master_server = {_, MasterNode}}, State) when MasterNode == node() ->
    ag_node_ra:trigger_election_self(),
    ag_node_ra:watch_pid(self()),
    erlang:send_after(1000, self(), ?CHECK_STATE_TIME),
    {noreply, State#state{ready = true}};
add_to_ra_cluster(
    NodeInfo = #ag_node_info{master_server = {_, MasterNode}, members = Members},
    State = #state{retry_num = RNum}) ->
    LocalServerId = ag_node_ra:get_local_server_id(),
    case add_to_ra_cluster(MasterNode) of
        {ok, _, _} ->
            ag_node_ra:watch_pid(self()),
            ag_node_cluster:set_node_info(NodeInfo#ag_node_info{members = [LocalServerId | Members]}),
            erlang:send_after(1000, self(), ?CHECK_STATE_TIME),
            {noreply, State#state{ready = true, retry_num = 0}};
        {error, already_member} ->
            ?LOG_WARNING("add_to_ra_cluster node:[~p] is already_member", [LocalServerId]),
            ag_node_ra:watch_pid(self()),
            ag_node_cluster:set_node_info(NodeInfo#ag_node_info{members = [LocalServerId | Members]}),
            erlang:send_after(1000, self(), ?CHECK_STATE_TIME),
            {noreply, State#state{ready = true}};
        {error, cluster_change_not_permitted} ->
            ?LOG_WARNING("add_to_ra_cluster cluster_change_not_permitted master:~p~n", [MasterNode]),
            erlang:send_after(5000, self(), ?CHECK_NODE_INFO),
            {noreply, State#state{retry_num = RNum + 1}};
        {timeout, _} ->
            ?LOG_WARNING("add_to_ra_cluster timeout master:~p~n", [MasterNode]),
            erlang:send_after(100, self(), ?CHECK_NODE_INFO),
            {noreply, State#state{retry_num = RNum + 1}};
        {error, Reason} ->
            agb_error:error("add node:[~p] to ra cluster master:[~p] error reason:~p~n",
                [node(), MasterNode, Reason])
    end.

-spec add_to_ra_cluster(node()) ->
    ra_server_proc:ra_cmd_ret() |
    {error, already_member} |
    {error, cluster_change_not_permitted}.
add_to_ra_cluster(MasterNode) ->
    ServerId = ag_node_ra:get_local_server_id(),
    add_member(MasterNode, ServerId).

-spec add_member(node(), {Name :: atom(), Node :: node()}) ->
    ra_server_proc:ra_cmd_ret() |
    {error, already_member} |
    {error, cluster_change_not_permitted}.
add_member(MasterNode, ServerId) ->
    ?LOG_DEBUG("ag_node_connect_process rpc add memeber masternode:[~p],serverid:[~p]~n", [MasterNode, ServerId]),
    rpc:call(MasterNode, ag_node_ra, add_member, [ServerId]).

start_ra_master(State = #state{start_ra_server = true}) ->
    {noreply, State};
start_ra_master(State = #state{start_ra_server = false}) ->
    case ag_node_ra:start_node() of
        ok ->
            ?LOG_INFO("ag_node_ra:start_node()~n"),
            LocalServerId = ag_node_ra:get_local_server_id(),
            ?LOG_INFO("ag_node_ra:get_local_server_id:~p~n",[LocalServerId]),
            ag_node_cluster:set_node_info(
                #ag_node_info{
                    cluster = ag_node_ra:get_cluster_name(),
                    master_server = LocalServerId,
                    members = [LocalServerId]}),

            ?LOG_INFO("ag_node_ra:set_node_info~n"),

            erlang:send_after(3000, self(), ?CHECK_NODE_INFO),
            ?LOG_INFO("CHECK_NODE_INFO~n"),
            {noreply, State#state{start_ra_server = true}};
        {error, Reason} ->
            agb_error:error("start_ra_cluster error reason:~p~n", [Reason]),
            {noreply, State}
    end.

start_ra_salve(State = #state{start_ra_server = true}) ->
    State;
start_ra_salve(State = #state{start_ra_server = false}) ->
    case ag_node_ra:start_node() of
        ok ->
            State#state{start_ra_server = true};
        {error, Reason} ->
            agb_error:error("start_ra_cluster error reason:~p~n", [Reason]),
            State
    end.

send_state_change_event(#state{node_state = NodeState, state_change_event_accepts = Accepts}) ->
    lists:foreach(fun(Pid) -> Pid ! {?NODE_STATE_CHANGE_EVENT, NodeState} end, Accepts).
