%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_msg_handle_executer).

-include("ag_engine_code_defines.hrl").
-include("ag_engine_core_defines.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").

-export([
    apply/4,
    async_apply/4,
    reply/3,
    async_reply/3
]).
-export([
    on_ping/1,
    on_terminate/1
]).

%%====================================================================
%% API
%%====================================================================
-spec apply(
    list(),
    InMsgObject :: map(),
    BlackHoleHandler :: {module(),atom()},
    IsUseAsync :: boolean()
) ->
    {boolean(), map()} | {boolean(), binary()} | {boolean(), []}.
apply(_, InMsgObject, BlackHoleHandler, IsUseAsync) ->
    do_apply(InMsgObject, BlackHoleHandler, IsUseAsync).

%% 异步执行handle函数
-spec async_apply(list(), pid(), module(), map()) ->
    {boolean(), map()} | {boolean(), binary()} | {boolean(), []}.
async_apply(_, GatePid, Module, Message) ->
    Echo = ag_engine_message_helper:get_echo(Message),
    Result = execute(Module, Message, Echo),
    ?LOG_DEBUG("ag_engine_msg_handle_executer async_apply Message:~p~n", [Message]),
    ?LOG_DEBUG("ag_engine_msg_handle_executer async_apply Result:~p~n", [Result]),
    ag_engine_msg_executer:reply(GatePid, Result, Echo).

-spec reply(list(), tuple(), map()) ->
    {boolean(), map()} | {boolean(), binary()} | {boolean(), []}.
reply(_, {async, _}, _) ->
    {true, <<>>};
reply(_, Reply, _) ->
    Reply.

async_reply(_, Result, Echo) ->
    {Result, Echo}.

-spec on_ping([module()]) ->
    no_return().
on_ping(_) ->
    ok.

-spec on_terminate([module()]) ->
    no_return().
on_terminate(_) ->
    case ag_engine_msg_handle_container:lookup_handler(?MSG_CONNECTION_CLOSE) of
        undefined ->
            ok;
        {ModuleHandler, true} ->
            async_execute(ModuleHandler,
                #{?MESSAGE_NAME_KEY=>?MSG_CONNECTION_CLOSE}, []);
        ModuleHandler ->
            execute(ModuleHandler,
                #{?MESSAGE_NAME_KEY=>?MSG_CONNECTION_CLOSE}, [])
    end,
    ok.

%%====================================================================
%% Private
%%====================================================================
do_apply(InMsgObject, BlackHoleHandler, IsUseAsync) ->
    Echo = ag_engine_message_helper:get_echo(InMsgObject),
    Result =
        case catch ag_engine_message_helper:get_name(InMsgObject) of
            {'EXIT', _} ->
                reply_error_msg(?ERROR_CODE_INTERNAL_SERVER_ERROR, Echo);
            MsgName ->
                case ag_engine_msg_handle_container:lookup_handler(MsgName) of
                    undefined ->
                        blackHoleHandle_execute(BlackHoleHandler, InMsgObject, Echo);
                    {ModuleHandler, true} when IsUseAsync == true ->
                        async_execute(ModuleHandler, InMsgObject, Echo);
                    {ModuleHandler, true} when IsUseAsync == false ->
                        ?LOG_ERROR("true_do_apply happen error, Handler:[~p] InMsgObject:[~p] not async response ~n",
                            [ModuleHandler, InMsgObject]),
                        reply_error_msg(?ERROR_CODE_INTERNAL_SERVER_ERROR, Echo);
                    ModuleHandler ->
                        execute(ModuleHandler, InMsgObject, Echo)
                end
        end,
    ag_engine_msg_executer:reply(Result, Echo).

reply_error_msg(ErrorInfo, Echo) ->
    OutMsgObj = ag_engine_message_helper:pack_code(?MSG_INTERNAL_ERROR, ErrorInfo),
    {false, ag_engine_message_helper:pack_echo(Echo, OutMsgObj)}.

blackHoleHandle_execute(BlackHoleHandler, InMsgObject, Echo) ->
    case BlackHoleHandler of
        undefined ->
            reply_error_msg(?ERROR_CODE_MSG_WITHOUT_HANDLER, Echo);
        {Handler, Function} ->
            Handler:Function(InMsgObject),
            {true, []};
        Handler when is_atom(Handler) ->
            BlackHoleHandler:handle(InMsgObject),
            {true, []}
    end.

async_execute(ModuleHandler, InMsgObject, Echo) ->
    case ag_engine_storage:get_roleworkerpid() of
        undefined ->
            ?LOG_ERROR("do_apply_by_async ModuleHandler:[~p] InMsgObject:[~p] error,not get roleworkerpid",
                [ModuleHandler, InMsgObject]),
            reply_error_msg(?ERROR_CODE_INTERNAL_SERVER_ERROR, Echo);
        RoleWorkerPid ->
            ?LOG_DEBUG("-------do_apply roleworker--------~p~n", [InMsgObject]),
            ag_engine_worker_module:worker_message_handle(RoleWorkerPid, ModuleHandler, InMsgObject),
            {async, <<>>}
    end.

%% 执行handle逻辑
execute(ModuleHandler, InMsgObject, Echo) ->
    case ModuleHandler:handle(InMsgObject) of
        {Bool, []} ->
            {Bool, []};
        {Bool, <<>>} ->
            {Bool, <<>>};
        {Bool, OutMsgObj} ->
            {Bool, ag_engine_message_helper:pack_echo(Echo, OutMsgObj)}
    end.


