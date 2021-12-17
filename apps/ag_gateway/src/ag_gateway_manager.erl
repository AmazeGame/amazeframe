%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_gateway_manager).

-include_lib("ag_base/include/agb_debuglogger.hrl").
-include_lib("ag_engine/include/ag_engine.hrl").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([
    init/1,
    callback_mode/0,
    terminate/3,
    code_change/4,

    state_check_cluster/3,
    state_start_server/3,
    state_running/3

]).

-define(SERVER, ?MODULE).
-define(MSG_CHECK_CLUSTER, 'MSG_CHECK_CLUSTER').
-define(CHECK_CLUSTER_INTERVAL, 1000).

-ifdef(use_amaze_application).
-define(PUT_LAUNCH_STATE(State), aga_launch_state:init(), aga_launch_state:put(ag_gateway, State)).
-else.
-define(PUT_LAUNCH_STATE(State), ok).
-endif.

-define(MSG_START_SERVER, 'MSG_START_SERVER').
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
    {ok, pid()}.
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, NextStateName :: atom(), NewState :: #state{}}).
init([]) ->
    self() ! ?MSG_CHECK_CLUSTER,
    {ok, state_check_cluster, #state{}}.

-spec(callback_mode() ->
    state_functions).
callback_mode() ->
    state_functions.

-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) ->
    term()).
terminate(_Reason, _StateName, _State) ->
    ok.

-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #state{}, Extra :: term()) ->
    {ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
-spec state_check_cluster(info|call, Message :: _, State :: #state{}) ->
    {next_state, NextStateName :: atom(), State :: #state{}}.
state_check_cluster(info, ?MSG_CHECK_CLUSTER, State) ->
    case ag_engine_cluster:check_cluster() of
        true ->
            %% clean all gate process
            self() ! ?MSG_START_SERVER,
            {next_state, state_start_server, State};
        false ->
            erlang:send_after(?CHECK_CLUSTER_INTERVAL, self(), ?MSG_CHECK_CLUSTER),
            {next_state, state_check_cluster, State}
    end;
state_check_cluster(call, CallData, State) ->
    ?LOG_INFO("state_check_cluster recieve ~p :not support!~n", [CallData]),
    {next_state, state_check_cluster, State}.

-spec state_start_server(info|call, Message :: _, State :: #state{}) ->
    {next_state, NextStateName :: atom(), State :: #state{}}.
state_start_server(info, ?MSG_START_SERVER, State) ->
    ?LOG_INFO("--------------------state_start_server-------------------"),
    start_server(),
    ag_engine_cluster:register_as_gate_manager(node()),
    ?PUT_LAUNCH_STATE(running),
    ag_eventdispatcher_process:fire(gateway, #{name => <<"initialized">>}),
    {next_state, state_running, State};
state_start_server(call, CallData, State) ->
    ?LOG_INFO("state_start_server recieve ~p :not support!~n", [CallData]),
    {next_state, state_start_server, State}.

-spec state_running(info|call, Message :: _, State :: #state{}) ->
    {next_state, NextStateName :: atom(), State :: #state{}}.
state_running(info, _Info, State) ->
    {next_state, state_running, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_server() ->
    {ok, HandleConfigs} = application:get_env(handles),
    [start_server(HandleConfig) || HandleConfig <- HandleConfigs].

start_server(HandleConfig) ->
    ClearPorts = get_clear_ports(HandleConfig),
    TlsPorts = get_tls_prots(HandleConfig),
    ReplyHttpHeader = get_reply_http_header(HandleConfig),
    MaxConnects = get_max_connections(HandleConfig),
    NumAcceptors = get_num_acceptors(HandleConfig),
    AppSetting = ag_engine_core:get_local_setting(),
    RouterOpt =
        #{
            reply_http_header => ReplyHttpHeader,
            app_setting => AppSetting
        },
    Handle = get_default_handler(HandleConfig),
    HandlerPlugins = get_handler_plugins(HandleConfig),
    Dispatch = cowboy_router:compile([
        {'_',
                HandlerPlugins ++
                [
                    {"/[...]", Handle, RouterOpt}
                ]
        }
    ]),

    TlsOps = case application:get_env(tls) of
                 {ok, TlsO} ->
                     TlsO;
                 _ ->
                     []
             end,
    ProtoOpts = proplists:get_value(proto_opts, HandleConfig, #{}),
    lists:foreach(
        fun(ClearPort) ->
            {ok, _} = cowboy:start_clear(
                {gateway_clear, ClearPort},
                #{max_connections => MaxConnects, num_acceptors => NumAcceptors, socket_opts => [{port, ClearPort}]},
                ProtoOpts#{env => #{dispatch => Dispatch}}
            ),
            ?LOG_INFO("cowboy start: clear_port=>~p router_option => ~p", [ClearPort, RouterOpt])
        end, ClearPorts),

    if
        TlsOps == [] ->
            ?LOG_WARNING("cowboy can not start tls without cert file: tls_port=>~p router_option => ~p",
                [TlsPorts, RouterOpt]);
        true ->
            lists:foreach(
                fun(TPort) ->
                    {ok, _} = cowboy:start_tls(
                        {gateway_tls, TPort},
                        #{
                            max_connections => MaxConnects,
                            num_acceptors => NumAcceptors,
                            socket_opts => [{port, TPort}] ++ TlsOps},
                        #{env => #{dispatch => Dispatch}}
                    ),
                    ?LOG_INFO("cowboy start: tls_port=>~p router_option => ~p", [TPort, RouterOpt])
                end, TlsPorts)
    end.

get_clear_ports(Config) ->
    case proplists:get_value(clear_ports, Config) of
        undefined ->
            case proplists:get_value(clear_port, Config) of
                undefined ->
                    [];
                ClrPort ->
                    [ClrPort]
            end;
        ClrPs ->
            ClrPs
    end.

get_tls_prots(Config) ->
    case proplists:get_value(tls_ports, Config) of
        undefined ->
            case proplists:get_value(tls_port, Config) of
                undefined ->
                    [];
                TlsPort ->
                    [TlsPort]
            end;
        TlrPs ->
            TlrPs
    end.

get_reply_http_header(Config) ->
    case proplists:get_value(reply_http_header, Config) of
        undefined ->
            {ok, ReplyHttpHeader} = application:get_env(ag_gateway, reply_http_header),
            ReplyHttpHeader;
        ReplyHeader ->
            ReplyHeader
    end.

get_max_connections(Config) ->
    proplists:get_value(max_connections, Config, infinity).

get_num_acceptors(Config) ->
    case proplists:get_value(num_acceptors, Config) of
        undefined ->
            {ok, NumAcceptors} = application:get_env(ag_gateway, num_acceptors, 16),
            NumAcceptors;
        NA ->
            NA
    end.

get_default_handler(Config) ->
    proplists:get_value(default_handler, Config, ag_gateway_handler).

get_handler_plugins(Config) ->
    proplists:get_value(handler_plugins, Config, []).
