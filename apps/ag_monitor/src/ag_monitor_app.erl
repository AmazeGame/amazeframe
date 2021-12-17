%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
%%%
-module(ag_monitor_app).

-behaviour(application).
-include_lib("ag_base/include/agb_debuglogger.hrl").

-ifdef(use_amaze_application).
-define(PUT_LAUNCH_STATE(State), aga_launch_state:init(), aga_launch_state:put(ag_monitor, State)).
-else.
-define(PUT_LAUNCH_STATE(State), ok).
-endif.


%% Application callbacks
-export([
    start/2,
    stop/1
]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    case application:get_env(is_use_prometheus) of
        {ok, true} ->
            agb_application:check_started(prometheus_httpd),
            prometheus_httpd:start(),
            case os:type() of
                {unix, _} ->
                    ?LOG_DEBUG("ag_monitor start system unix register_collector prometheus_process_collector"),
                    prometheus_registry:register_collector(prometheus_process_collector);
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end,
    case ag_monitor_sup:start_link() of
        {ok, Pid} ->
            start_system_monitor(),
            ?PUT_LAUNCH_STATE(running),
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_system_monitor() ->
    case application:get_env(monitorlist) of
        {ok, MonitorList} ->
            [start_system_monitor(Monitor) || Monitor <- MonitorList];
        _ ->
            ignore
    end.

start_system_monitor(Monitor) ->
    case ag_monitor_config:get(Monitor) of
        undefined ->
            ignore;
        {_, HandleList} ->
            [start_monitor_handle(Handle) || Handle <- HandleList]
    end.

start_monitor_handle(Handle) ->
    case ag_monitor_config:get(Handle) of
        {_, #{isopen := false}} ->
            ignore;
        {_, #{args := Args}} ->
            Handle:start(Args);
        _ ->
            ignore
    end.