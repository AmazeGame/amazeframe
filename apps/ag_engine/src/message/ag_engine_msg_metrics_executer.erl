%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% 功能：
%%% 消息度量模块，统计没一条消息的执行时间，状态和消息信息
%%% 可以用第三方工具收集度量信息，第三方工具应已经此模块提
%%% 供的数据信息做出统计操作（遵守导出信息的数据结构）
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------

-module(ag_engine_msg_metrics_executer).
-include("ag_engine_code_defines.hrl").
-include("ag_engine_core_defines.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").

%% API
-export([
    apply/4,
    async_apply/4
]).
-export([
    reply/3,
    async_reply/3
]).
-export([
    pop_all_msg/2,
    pop_msg/2
]).
-export([
    message_to_client/3,
    text_message_to_client/3
]).
-export([
    on_ping/1,
    on_terminate/1
]).

-define(TRACKING_INFO_KEY, <<"##tracking_Info##">>).
-define(TRACKING_ID, <<"tracking_id">>).
-define(TRACKING_REQ_START, <<"tracking_req_start">>).
-define(TRACKING_REQ_END, <<"tracking_req_end">>).
-define(TRACKING_EXEC_START, <<"tracking_exec_start">>).
-define(TRACKING_EXEC_END, <<"tracking_exec_end">>).
-define(TRACKING_REQUEST_NAME, <<"tracking_request_name">>).
-define(TRACKING_RESPONSE_NAME, <<"tracking_response_name">>).
-define(TRACKING_MSG_TYPE, <<"tracking_msg_type">>).
-define(TRACKING_IN_MSG, <<"tracking_in_msg">>).
-define(TRACKING_OUT_MSG, <<"tracking_out_msg">>).
-define(TRACKING_STATUS, <<"tracking_status">>).
-define(TRACKING_CLIENT_IP, <<"tracking_client_ip">>).
-define(TRACKING_USER_ID, <<"tracking_user_id">>).

-define(ASYNC_CALL_MAX_METRICS_TIME, 5).
%%====================================================================
%% API
%%====================================================================

%% 定时触发操作x
-spec on_ping([module()]) ->
    no_return().
on_ping([Exec | Other]) ->
    check_metrics_info_time(),
    Exec:on_ping(Other).

%% gate进程退出
-spec on_terminate([module()]) ->
    no_return().
on_terminate([Exec | Other]) ->
    observer_terminate_msg(),
    Exec:on_terminate(Other).

%% gamebase接受到一个请求
-spec apply(
    Execs :: [module()],
    InMsgObject :: map()|[map()],
    BlackHoleHandler :: {module(), atom()},
    IsUseAsync :: boolean()
) ->
    {boolean(), map()} | {boolean(), binary()} | {boolean(), []}.
apply([Exec | Other], InMsgObject, BlackHoleHandler, IsUseAsync) ->
    ?LOG_INFO("ag_msg_handle_metrics apply msg:~p~n", [InMsgObject]),
    InMsgObject0 = do_apply(InMsgObject),
    Exec:apply(Other, InMsgObject0, BlackHoleHandler, IsUseAsync).

%% 异步进程开始执行异步请求
-spec async_apply(
    Execs :: [module()],
    GatePid :: pid(),
    Module :: module(),
    Message :: map()
) ->
    no_return().
async_apply([Exec | Other], GatePid, Module, #{?INTERNAL_DEFINE_ECHO := Echo} = Message) ->
    #{?TRACKING_INFO_KEY := TrackingInfo} = Echo,
    StartTime = erlang:monotonic_time(),
    NewTrackingInfo = TrackingInfo#{?TRACKING_EXEC_START => StartTime, ?TRACKING_MSG_TYPE => async_request},
    Message0 = pack_tracking_info_in_echo(Message, NewTrackingInfo),
    Exec:async_apply(Other, GatePid, Module, Message0).

%% gamebase开始向客户端回调调用结果
-spec reply([module()], tuple(), maps:map()) ->
    tuple().
reply([Exec | Other], {async, _} = Result, Echo) ->
    Exec:reply(Other, Result, Echo);
reply([Exec | Other], {_, Msg} = Result, #{?TRACKING_INFO_KEY := TrackingInfo} = Echo) when Msg == <<>>; Msg == [] ->
    #{?TRACKING_ID := TrackingId, ?TRACKING_EXEC_START := ExecStart} = TrackingInfo,
    case ag_engine_storage:get_msg_metrics_info(TrackingId) of
        undefined ->
            ?LOG_ERROR("ag_msg_handle_metrics track msg error unknown trackingid:~p~n", [TrackingId]);
        Info ->
            NowTime = erlang:monotonic_time(),
            ExecEnd = maps:get(?TRACKING_EXEC_END, Echo, NowTime),
            Type = maps:get(?TRACKING_STATUS, Echo, sync_request),
            NewInfo = Info#{
                ?TRACKING_REQ_END => NowTime,
                ?TRACKING_EXEC_START => ExecStart,
                ?TRACKING_EXEC_END => ExecEnd,
                ?TRACKING_STATUS => fin,
                ?TRACKING_MSG_TYPE => Type,
                ?TRACKING_RESPONSE_NAME => <<"NoReturn">>
            },
            ag_engine_storage:remove_msg_metrics_info(TrackingId),
            observer_request_msg(NewInfo)
    end,
    Exec:reply(Other, Result, Echo);
reply([Exec | Other], Result1, Echo) ->
    {Result, #{?MESSAGE_NAME_KEY := Name} = OutMsgObjects} = Result1,
    #{?TRACKING_INFO_KEY := TrackingInfo} = Echo,
    #{?TRACKING_ID := TrackingId, ?TRACKING_EXEC_START := ExecStart} = TrackingInfo,
    OutMsgObjects0 = unpack_tracking_info_from_echo(OutMsgObjects),
    case ag_engine_storage:get_msg_metrics_info(TrackingId) of
        undefined ->
            ?LOG_ERROR("ag_msg_handle_metrics track msg error unknown trackingid:~p~n", [TrackingId]);
        Info ->
            NowTime = erlang:monotonic_time(),
            ExecEnd = maps:get(?TRACKING_EXEC_END, Echo, NowTime),
            Type = maps:get(?TRACKING_STATUS, Echo, sync_request),
            NewInfo = Info#{
                ?TRACKING_REQ_END => NowTime,
                ?TRACKING_EXEC_START => ExecStart,
                ?TRACKING_EXEC_END => ExecEnd,
                ?TRACKING_OUT_MSG => OutMsgObjects0,
                ?TRACKING_STATUS => fin,
                ?TRACKING_MSG_TYPE => Type,
                ?TRACKING_RESPONSE_NAME => Name
            },
            ag_engine_storage:remove_msg_metrics_info(TrackingId),
            observer_request_msg(NewInfo)
    end,
    Exec:reply(Other, {Result, OutMsgObjects0}, Echo).

%% 处理异步handle的返回结果,记录异步逻辑结束时间
-spec async_reply([module()], tuple(), maps:map()) ->
    tuple().
async_reply([Exec | Other], {_, Msg} = Result, Echo) when Msg == <<>>; Msg == [] ->
    #{?TRACKING_INFO_KEY := TrackingInfo} = Echo,
    NewTrackingInfo = TrackingInfo#{?TRACKING_EXEC_END => erlang:monotonic_time()},
    Exec:async_reply(Other, Result, Echo#{?TRACKING_INFO_KEY => NewTrackingInfo});
async_reply([Exec | Other], {Result, OutMsgObjects}, Echo) ->
    #{?TRACKING_INFO_KEY := TrackingInfo} = Echo,
    NewTrackingInfo = TrackingInfo#{?TRACKING_EXEC_END => erlang:monotonic_time()},
    OutMsgObjects0 = pack_tracking_info_in_echo(OutMsgObjects, NewTrackingInfo),
    Exec:async_reply(Other, {Result, OutMsgObjects0}, Echo#{?TRACKING_INFO_KEY => NewTrackingInfo}).

-spec pop_all_msg([module()], RoleWorkerPid :: pid()) ->
    [MsgObject :: map()]|[].
pop_all_msg([Exec | Other], RoleWorkerPid) ->
    Messages = Exec:pop_all_msg(Other, RoleWorkerPid),
    observer_pop_msg(Messages, get_need_core_value(self())),
    Messages.

-spec pop_msg([module()], RoleWorkerPid :: pid()) ->
    {error, undefined} | map().
pop_msg([Exec | Other], RoleWorkerPid) ->
    case Exec:pop_msg(Other, RoleWorkerPid) of
        {error, undefined} ->
            {error, undefined};
        MsgObject ->
            observer_pop_msg(MsgObject, get_need_core_value(self())),
            MsgObject
    end.

-spec message_to_client([module()], GatePid :: pid(), ReplyObject :: term()) ->
    ok.
message_to_client([Exec | Other], GatePid, ReplyObject) when GatePid == self() ->
    observer_pop_msg(ReplyObject, get_need_core_value(GatePid)),
    Exec:message_to_client(Other, GatePid, ReplyObject);
message_to_client([Exec | Other], GatePid, ReplyObject) ->
    spawn_observer_pop_msg(ReplyObject, GatePid),
    Exec:message_to_client(Other, GatePid, ReplyObject).

-spec text_message_to_client([module()], GatePid :: pid(), ReplyObject :: term()) ->
    ok.
text_message_to_client([Exec | Other], GatePid, ReplyObject) when GatePid == self() ->
    observer_pop_msg(ReplyObject, get_need_core_value(GatePid)),
    Exec:text_message_to_client(Other, GatePid, ReplyObject);
text_message_to_client([Exec | Other], GatePid, ReplyObject) ->
    spawn_observer_pop_msg(ReplyObject, GatePid),
    Exec:text_message_to_client(Other, GatePid, ReplyObject).

%%====================================================================
%% PRIVATE
%%====================================================================
get_need_core_value(GatePid) ->
    ag_engine_gateway_module:rpc_get_core_value(GatePid, [client_ip, id]).

spawn_observer_pop_msg(ReplyObject, GatePid) ->
    Fun =
        fun() ->
            observer_pop_msg(ReplyObject, get_need_core_value(GatePid))
        end,
    spawn(Fun).

%% 在透传参数中加入trackingInfo,会重写原消息的透传数据结构
pack_tracking_info_in_echo(MsgObject, TrackingInfo) ->
    case ag_engine_message_helper:get_echo(MsgObject) of
        <<>> ->
            ag_engine_message_helper:pack_echo(#{?TRACKING_INFO_KEY => TrackingInfo}, MsgObject);
        #{?TRACKING_INFO_KEY := _, ?INTERNAL_DEFINE_ECHO := Echo} ->
            EchoMap = #{?TRACKING_INFO_KEY => TrackingInfo, ?INTERNAL_DEFINE_ECHO => Echo},
            ag_engine_message_helper:pack_echo(EchoMap, MsgObject);
        #{?TRACKING_INFO_KEY := _} ->
            ag_engine_message_helper:pack_echo(#{?TRACKING_INFO_KEY => TrackingInfo}, MsgObject);
        Echo ->
            EchoMap = #{?TRACKING_INFO_KEY => TrackingInfo, ?INTERNAL_DEFINE_ECHO => Echo},
            ag_engine_message_helper:pack_echo(EchoMap, MsgObject)
    end.

%% 去除消息中透传的trackingInfo，并还原透传参数数据结构
unpack_tracking_info_from_echo(#{?INTERNAL_DEFINE_ECHO := Echo} = MsgObject) ->
    ClientEcho = maps:get(?INTERNAL_DEFINE_ECHO, Echo, <<>>),
    MsgObject0 = maps:remove(?INTERNAL_DEFINE_ECHO, MsgObject),
    ag_engine_message_helper:pack_echo(ClientEcho, MsgObject0).

do_apply(#{?MESSAGE_NAME_KEY := Name} = InMsgObject) ->
    TrackingId = ag_idcreator:gen_newid(ets),
    TrackingInfo = #{
        ?TRACKING_ID => TrackingId,
        ?TRACKING_EXEC_START => erlang:monotonic_time()
    },
    InMsgObject0 = pack_tracking_info_in_echo(InMsgObject, TrackingInfo),
    NeedCoreInfo = ag_engine_gateway_module:rpc_get_core_value(self(), [client_ip, id]),
    MetricsInfo =
        #{
            ?TRACKING_REQUEST_NAME => Name,
            ?TRACKING_MSG_TYPE => sync_request,
            ?TRACKING_REQ_START => erlang:monotonic_time(),
            ?TRACKING_CLIENT_IP => proplists:get_value(client_ip, NeedCoreInfo),
            ?TRACKING_USER_ID => proplists:get_value(id, NeedCoreInfo),
            ?TRACKING_IN_MSG => InMsgObject
        },
    ?LOG_INFO("ag_msg_handle_metrics update_msg_metrics_info:~p~n", [{TrackingId, MetricsInfo}]),
    ag_engine_storage:update_msg_metrics_info(TrackingId, MetricsInfo),
    InMsgObject0.

%% 检测依存度量信息的触发时间是否超时
check_metrics_info_time() ->
    InfoList = maps:to_list(ag_engine_storage:get_all_msg_metrics_info()),
    NowSeconds = erlang:monotonic_time(seconds),
    lists:foreach(
        fun({Id, #{?TRACKING_REQ_START := ReqStart} = Info}) ->
            StartSeconds = erlang:convert_time_unit(ReqStart, native, seconds),
            ?LOG_INFO("check_metrics_info_time StartSeconds:~p~n", [StartSeconds]),
            if
                NowSeconds - StartSeconds >= ?ASYNC_CALL_MAX_METRICS_TIME ->
                    observer_timeout_msg(Info),
                    ag_engine_storage:remove_msg_metrics_info(Id);
                true ->
                    ignore
            end
        end,
        InfoList
    ).

observer_pop_msg([], _) ->
    ignore;
observer_pop_msg([H | _] = Messages, CoreInfo) when is_map(H) ->
    [observer_pop_msg(Msg, CoreInfo) || Msg <- Messages];
observer_pop_msg(#{?MESSAGE_NAME_KEY := Name} = Message, CoreInfo) ->
    MetricsInfo =
        #{
            ?TRACKING_RESPONSE_NAME => Name,
            ?TRACKING_MSG_TYPE => push,
            ?TRACKING_OUT_MSG => Message,
            ?TRACKING_CLIENT_IP => proplists:get_value(client_ip, CoreInfo),
            ?TRACKING_USER_ID => proplists:get_value(id, CoreInfo),
            ?TRACKING_STATUS => fin
        },
    exec_call_back(MetricsInfo);
observer_pop_msg(Message, CoreInfo) when is_binary(Message) or is_list(Message) ->
    observer_pop_msg(#{?MESSAGE_NAME_KEY => <<"none">>, <<"info">> => Message}, CoreInfo).

observer_request_msg(MetricsInfo) ->
    exec_call_back(MetricsInfo).

observer_timeout_msg(MetricsInfo) ->
    MetricsInfo0 = MetricsInfo#{?TRACKING_STATUS => timeout},
    exec_call_back(MetricsInfo0).

observer_terminate_msg() ->
    InfoList = maps:to_list(ag_engine_storage:get_all_msg_metrics_info()),
    lists:foreach(
        fun({_, MetricsInfo}) ->
            MetricsInfo0 = MetricsInfo#{?TRACKING_STATUS => terminate},
            spawn(fun() -> exec_call_back(MetricsInfo0) end)
        end,
        InfoList
    ).

exec_call_back(MetricsInfo) ->
    case application:get_env(ag_engine, msg_executer) of
        undefined ->
            ignore;
        {ok, #{msg_metrics_callback := {Module, Fun}}} ->
            try
                Module:Fun(MetricsInfo)
            catch
                E:R ->
                    ?LOG_ERROR(
                        "ag_engine_msg_metrics_executer Module:[~p] Fun:[~p] exec_call_back error:[~p],reson:[~p]~n",
                        [Module, Fun, E, R])
            end;
        _ ->
            ignore
    end.

