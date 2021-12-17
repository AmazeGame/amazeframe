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
-module(ag_monitor_prometheus_instrumenter_handle).

-include("../../../include/ag_monitor_msg_tracking_struct.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").

-export([
    start/1,
    observe/1,
    stop/0
]).

-define(DEFAULT_DURATION_BUCKETS, [0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4]).
-define(DEFAULT_REQUEST_LABELS, [request_name, status]).
-define(DEFAULT_PUSH_LABELS, [push_name]).
-define(DEFAULT_TIME, [request_name]).
-define(DEFAULT_REGISTRY, default).
-define(DEFAULT_CONFIG, [
    {duration_buckets, ?DEFAULT_DURATION_BUCKETS},
    {request_labels, ?DEFAULT_REQUEST_LABELS},
    {push_labels, ?DEFAULT_PUSH_LABELS},
    {time_labels, ?DEFAULT_TIME},
    {registry, ?DEFAULT_REGISTRY}
]).

%% ===================================================================
%% API
%% ===================================================================

-spec observe(map()) -> ok.
observe(Metrics) ->
    dispatch_metrics(Metrics),
    ok.

-spec start(term()) -> ok.
start(_Args) ->
    setup().


-spec stop() -> ok.
stop() ->
    List =
        [
            gamebase_request_info,
            gamebase_push_info,
            gamebase_request_duration_seconds
        ],
    [prometheus_counter:remove(registry(), Name) || Name <- List],
    List1 =
        [
            gamebase_request_duration_seconds,
            gamebase_async_request_duration_seconds,
            gamebase_async_exec_duration_seconds
        ],
    [prometheus_histogram:remove(registry(), Name) || Name <- List1],
    ok.

%% @doc
%% Sets all metrics up. Call this when the app starts.
%% @end
setup() ->
    ?LOG_DEBUG("ag_monitor_prometheus_instrumenter_handle setup"),
    prometheus_counter:declare([{name, gamebase_request_info},
        {registry, registry()},
        {labels, request_labels()},
        {help, "Total number of request name"}]),
    prometheus_counter:declare([{name, gamebase_push_info},
        {registry, registry()},
        {labels, push_labels()},
        {help, "Total number of push name"}]),
    prometheus_histogram:declare([{name, gamebase_request_duration_seconds},
        {registry, registry()},
        {labels, time_labels()},
        {buckets, duration_buckets()},
        {help, "sync request time"}]),
    prometheus_histogram:declare([{name, gamebase_async_request_duration_seconds},
        {registry, registry()},
        {labels, time_labels()},
        {buckets, duration_buckets()},
        {help, "async request all time."}]),
    prometheus_histogram:declare([{name, gamebase_async_exec_duration_seconds},
        {registry, registry()},
        {labels, time_labels()},
        {buckets, duration_buckets()},
        {help, "async request async exec time."}]),
    ok.

dispatch_metrics(#{?TRACKING_MSG_TYPE := sync_request, ?TRACKING_STATUS := fin} = Metrics) ->
    #{?TRACKING_REQ_START := ReqStart, ?TRACKING_REQ_END := ReqEnd} = Metrics,
    observe(gamebase_request_duration_seconds, time_labels(Metrics), ReqEnd - ReqStart),
    inc(gamebase_request_info, request_labels(Metrics));
dispatch_metrics(#{?TRACKING_MSG_TYPE := sync_request} = Metrics) ->
    inc(gamebase_request_info, request_labels(Metrics));
dispatch_metrics(#{?TRACKING_MSG_TYPE := async_request, ?TRACKING_STATUS := fin} = Metrics) ->
    #{
        ?TRACKING_REQ_START := ReqStart,
        ?TRACKING_REQ_END := ReqEnd,
        ?TRACKING_EXEC_START := ExecStart,
        ?TRACKING_EXEC_END := ExecEnd
    } = Metrics,
    TimeLabels = time_labels(Metrics),
    observe(gamebase_async_request_duration_seconds, TimeLabels, ReqEnd - ReqStart),
    observe(gamebase_async_exec_duration_seconds, TimeLabels, ExecEnd - ExecStart),
    inc(gamebase_request_info, request_labels(Metrics));
dispatch_metrics(#{?TRACKING_MSG_TYPE := Type} = Metrics) when Type == async_request ->
    inc(gamebase_request_info, request_labels(Metrics));
dispatch_metrics(#{?TRACKING_MSG_TYPE := Type} = Metrics) when Type == push ->
    inc(gamebase_push_info, push_labels(Metrics)).

inc(Name, Labels) ->
    prometheus_counter:inc(registry(), Name, Labels, 1).

observe(Name, Labels, Value) ->
    prometheus_histogram:observe(registry(), Name, Labels, Value).

%% configuration
config() ->
    application:get_env(prometheus, cowboy_instrumenter, ?DEFAULT_CONFIG).

get_config_value(Key, Default) ->
    proplists:get_value(Key, config(), Default).

duration_buckets() ->
    get_config_value(duration_buckets, ?DEFAULT_DURATION_BUCKETS).

request_labels() ->
    get_config_value(request_labels, ?DEFAULT_REQUEST_LABELS).

push_labels() ->
    get_config_value(push_labels, ?DEFAULT_PUSH_LABELS).

time_labels() ->
    get_config_value(time_labels, ?DEFAULT_TIME).

registry() ->
    get_config_value(registry, ?DEFAULT_REGISTRY).

request_labels(Metrics) ->
    compute_labels(request_labels(), Metrics).

push_labels(Metrics) ->
    compute_labels(push_labels(), Metrics).

time_labels(Metrics) ->
    compute_labels(time_labels(), Metrics).

compute_labels(Labels, Metrics) ->
    [label_value(Label, Metrics) || Label <- Labels].

label_value(client_ip, #{?TRACKING_CLIENT_IP := ClientIp}) ->
    ClientIp;
label_value(status, #{?TRACKING_STATUS := Status}) ->
    Status;
label_value(push_name, #{?TRACKING_RESPONSE_NAME := Name}) ->
    Name;
label_value(request_name, #{?TRACKING_REQUEST_NAME := Name}) ->
    Name.