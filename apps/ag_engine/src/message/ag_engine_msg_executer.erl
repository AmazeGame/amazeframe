%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% 功能:
%%%     消息执行队列的调用入口
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------

-module(ag_engine_msg_executer).

-include("ag_engine_code_defines.hrl").
-include("ag_engine_core_defines.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").

-export([
    msg_single_apply/2,
    msg_carrier_apply/1, msg_carrier_apply/2,
    black_hole_apply/3
]).
-export([async_apply/3]).
-export([reply/2, reply/3]).
-export([
    pop_all_msg/1,
    pop_msg/1
]).
-export([
    message_to_client/2,
    text_message_to_client/2
]).
-export([
    on_ping/0,
    on_terminate/0
]).

%%====================================================================
%% API
%%====================================================================

%% 执行gate接受到的请求，如果发现handle是异步处理函数，则发起异步请求
%% 短连模式下，禁止使用异步处理函数
%% 单包模式
-spec msg_single_apply(InMsgObject :: map(), IsUseAsync :: boolean()) ->
    {boolean(), map()} | {boolean(), binary()} | {boolean(), []}.
msg_single_apply(InMsgObject, IsUseAsync) ->
    [Exec | Other] = get_request_msg_executer(),
    Exec:apply(Other, InMsgObject, undefined, IsUseAsync).

%% 组合包模式使用的函数
msg_carrier_apply(InMsgObjects) ->
    batch_package(InMsgObjects, true, undefined, true, []).
msg_carrier_apply(RoleWorkerPid, InMsgObjects) ->
    case batch_package(InMsgObjects, true, undefined, false, []) of
        {true, Reply} ->
            case RoleWorkerPid of
                undefined ->
                    {true, Reply};
                RoleWorker ->
                    ReplyCarrier = pop_all_msg(RoleWorker),
                    {true, Reply ++ ReplyCarrier}
            end;
        {false, OutMessages} ->
            {false, OutMessages}
    end.

%% 供模拟客户端使用的函数
-spec black_hole_apply(
    InMsgObject :: map() | [map()],
    BlackHoleHandler :: {module(), atom()},
    IsUseAsync :: boolean()
) ->
    {boolean(), map()} | {boolean(), binary()} | {boolean(), []}.
black_hole_apply(InMsgObject, BlackHoleHandler, IsUseAsync) when is_map(InMsgObject) ->
    [Exec | Other] = get_request_msg_executer(),
    Exec:apply(Other, InMsgObject, BlackHoleHandler, IsUseAsync);
black_hole_apply(InMsgObject, BlackHoleHandler, IsUseAsync) when is_list(InMsgObject) ->
    batch_package(InMsgObject, true, BlackHoleHandler, IsUseAsync, []).

%% 执行异步handle的逻辑，由异步进程调用
-spec async_apply(
    GatePid :: pid(), Module :: module(),
    Message :: map()
) ->
    no_return().
async_apply(GatePid, Module, Message) ->
    ?LOG_DEBUG("ag_engine_msg_executer async_apply GatePid:~p,Module:~p,Message:~p~n", [GatePid, Module, Message]),
    [Exec | Other] = get_request_msg_executer(),
    Exec:async_apply(Other, GatePid, Module, Message).

%% 同步handle和异步handle都会走到这个函数，本进程调用
%% 异步函数处理完成返回结果后也会调用到这里继续后面的步骤
-spec reply(
    Reply :: term(),        %%执行结构
    Echo :: map()           %%透传信息
) ->
    term().
reply(Reply, Echo) ->
    [Exec | Other] = get_request_msg_executer(),
    Result = Exec:reply(Other, Reply, Echo),
    Result.

%% 异步handle调用的回复信息函数，会给本进程发送消息调用reply/2
-spec reply(
    GatePid :: pid(),       %%接收进程pid
    Reply :: term(),        %%执行结构
    Echo :: map()           %%透传信息
) ->
    ok | term().
reply(GatePid, Reply, Echo) ->
    [Exec | Other] = get_request_msg_executer(),
    {Result, Echo0} = Exec:async_reply(Other, Reply, Echo),
    ?LOG_DEBUG("send_async_handle_result gatepid:~p,Result:~p~n", [GatePid, Result]),
    ag_engine_gateway_module:send_async_handle_result(GatePid, Result, Echo0).

-spec on_ping() ->
    no_return().
on_ping() ->
    [Exec | Other] = get_request_msg_executer(),
    Exec:on_ping(Other).

-spec on_terminate() ->
    no_return().
on_terminate() ->
    [Exec | Other] = get_request_msg_executer(),
    Exec:on_terminate(Other).

%% 执行获取全部待推送消息操作
-spec pop_all_msg(RoleWorkerPid :: pid()) ->
    [MsgObject :: map()].
pop_all_msg(RoleWorkerPid) ->
    [Exec | Other] = get_push_msg_executer(),
    Exec:pop_all_msg(Other, RoleWorkerPid).

%%　执行获取单个待推送消息操作
-spec pop_msg(RoleWorkerPid :: pid()) ->
    {error, undefined} | map().
pop_msg(RoleWorkerPid) ->
    [Exec | Other] = get_push_msg_executer(),
    Exec:pop_msg(Other, RoleWorkerPid).

%% 执行向客户端发送binary消息的操作
-spec message_to_client(GatePid :: pid(), ReplyObject :: term()) ->
    ok.
message_to_client(undefined, _ReplyObject) ->
    ok;
message_to_client(GatePid, ReplyObject) ->
    [Exec | Other] = get_push_msg_executer(),
    Exec:message_to_client(Other, GatePid, ReplyObject),
    ok.

%% 执行向客户端发送text消息的操作
-spec text_message_to_client(GatePid :: pid(), ReplyObject :: term()) ->
    ok.
text_message_to_client(undefined, _ReplyObject) ->
    ok;
text_message_to_client(GatePid, ReplyObject) ->
    [Exec | Other] = get_push_msg_executer(),
    Exec:text_message_to_client(Other, GatePid, ReplyObject),
    ok.

%%====================================================================
%% Private
%%====================================================================
%%@private 分批处理数据
batch_package([InMsgObject | T], true, BlackHoleHandler, IsUseAsync, AccOut) ->
    [Exec | Other] = get_request_msg_executer(),
    case Exec:apply(Other, InMsgObject, BlackHoleHandler, IsUseAsync) of
        {true, []} ->
            batch_package(T, true, BlackHoleHandler, IsUseAsync, AccOut);
        {true, <<>>} ->
            batch_package(T, true, BlackHoleHandler, IsUseAsync, AccOut);
        {true, RetMsg} ->
            batch_package(T, true, BlackHoleHandler, IsUseAsync, [RetMsg | AccOut]);
        {false, []} ->
            batch_package([], false, BlackHoleHandler, IsUseAsync, AccOut);
        {false, <<>>} ->
            batch_package([], false, BlackHoleHandler, IsUseAsync, AccOut);
        {false, RetMsg} ->
            batch_package([], false, BlackHoleHandler, IsUseAsync, [RetMsg | AccOut])
    end;
batch_package(_, false, _, _, AccOut) ->
    {false, lists:reverse(AccOut)};
batch_package([], true, _, _, AccOut) ->
    {true, lists:reverse(AccOut)}.

get_request_msg_executer() ->
    case application:get_env(ag_engine, msg_executer) of
        undefined ->
            [ag_engine_msg_handle_executer];
        {ok, #{msg_executer_middleware := ExecuterList}} ->
            ExecuterList ++ [ag_engine_msg_handle_executer];
        {ok, _} ->
            [ag_engine_msg_handle_executer]
    end.

get_push_msg_executer() ->
    case application:get_env(ag_engine, msg_executer) of
        undefined ->
            [ag_engine_msg_push_executer];
        {ok, #{msg_executer_middleware := ExecuterList}} ->
            ExecuterList ++ [ag_engine_msg_push_executer];
        {ok, _} ->
            [ag_engine_msg_push_executer]
    end.
