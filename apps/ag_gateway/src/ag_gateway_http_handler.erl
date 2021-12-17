%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_gateway_http_handler).

-include_lib("ag_base/include/agb_debuglogger.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").
-include_lib("ag_engine/include/ag_engine_code_defines.hrl").
-include_lib("ag_engine/include/ag_engine.hrl").
-include("ag_gateway_common.hrl").

-export([init/3]).
%% API
-spec init(Req0 :: term(), Headers :: list(), Opts :: map()) ->
    {ok, Req :: term(), Opts :: map()}.
init(Req0, Headers, #{clientip := ClientIp, app_setting := AppSetting,
    proto := Proto, msgcarrier := MsgCarrier, reply_http_header := ReplyHttpHeader} = Opts) ->
    ?LOG_INFO("==>[http] init gatepid:~p :~p~n", [self(), {Headers, Opts}]),
    ag_engine_storage:clean_msg_exec_info(),
    ReplyHeader = get_reply_header(MsgCarrier, ReplyHttpHeader),
    ag_engine_storage:set_client_ip(ClientIp),
    {ok, Reply} = do_handler(Req0, Headers, Proto, AppSetting, MsgCarrier, ReplyHeader),
    {ok, Reply, Opts}.

get_reply_header(MsgCarrier, ReplyHttpHeader) ->
    if
        MsgCarrier ->
            ReplyHttpHeader#{?HEADER_MSGCARRIER_FIELD => <<"true">>};
        true ->
            ReplyHttpHeader
    end.

do_handler(Req0, Headers, Proto, AppSetting, MsgCarrier, ReplyHeader) ->
    try
        {ok, UserSetting} = validate_session_and_setup(Headers, AppSetting),
        HeadersMap = maps:from_list(Headers),
        ok = auth_validate(HeadersMap, UserSetting),
        {ok, InBin, _} = cowboy_req:read_body(Req0),
        {ok, InMsg} = do_decode(Proto, InBin, MsgCarrier, maps:get(decode_filters, UserSetting, [])),
        HeaderMsg = merge_msg_headers(HeadersMap, InMsg),
        {ok, OutMsg} = do_msg_apply(HeaderMsg, MsgCarrier, UserSetting),
        {ok, OutBin} = do_encode(Proto, OutMsg, maps:get(encode_filters, UserSetting, [])),
        {ok, cowboy_req:reply(200, ReplyHeader, OutBin, Req0)}
    catch
        {error, ErrorInfo} ->
            {ok, InBin0, _} = cowboy_req:read_body(Req0),
            ?LOG_ERROR("==>[http] gate:~p roleworker:~p c=>s InBin:~p~n",
                [self(), ag_engine_storage:get_roleworkerpid(), InBin0]),
            OutMsg1 = make_error_package(ErrorInfo, MsgCarrier),
            ?LOG_ERROR("==>[http] gate:~p roleworker:~p s=>c msg:~p~n",
                [self(), ag_engine_storage:get_roleworkerpid(), OutMsg1]),
            OutBin1 = ag_engine_protocol_codec:encode(Proto, OutMsg1, []),
            ?LOG_DEBUG("==>[http] gate:~p roleworker:~p s=>c data:~p~n",
                [self(), ag_engine_storage:get_roleworkerpid(), OutBin1]),
            {ok, cowboy_req:reply(200, ReplyHeader, OutBin1, Req0)}

    end.

validate_session_and_setup(Headers, AppSetting) ->
    case ag_engine_message_helper:get_session(Headers) of
        undefined ->
            {ok, AppSetting};
        Session ->
            case ag_engine_cluster:player_from_session(Session) of
                undefined ->
                    throw({error, ?ERROR_CODE_CANNOT_FIND_SESSION});
                #online_player{id = Id, archive = Archive, security = Security, agent_pid = RWPid} ->
                    ag_engine_storage:set_id(Id),
                    ag_engine_storage:set_security(Security),
                    ag_engine_storage:set_archive(Archive),
                    ag_engine_storage:set_session(Session),
                    ag_engine_storage:set_roleworkerpid(RWPid),
                    {ok,
                        AppSetting#{
                            ?INTERNAL_DEFINE_SECURITY => Security,
                            agent_pid => RWPid,
                            ?INTERNAL_DEFINE_ID => Id
                        }
                    }
            end
    end.

auth_validate(HeadersMap, UserSetting) ->
    case ag_engine_authenticate:validate(HeadersMap, UserSetting) of
        true ->
            ok;
        ErrorInfo ->
            throw({error, ErrorInfo})
    end.

do_decode(Proto, InBin, MsgCarrier, DecodeFilters) ->
    ?LOG_DEBUG("==>[http] gate:~p roleworker:~p c=>s data:~p~n",
        [self(), ag_engine_storage:get_roleworkerpid(), InBin]),
    case catch ag_engine_protocol_codec:decode(Proto, MsgCarrier, InBin, DecodeFilters) of
        {'EXIT', Reason} ->
            ?LOG_ERROR("==>[http] gate:~p roleworker:~p decode data:~p error reason:~p~n",
                [self(), ag_engine_storage:get_roleworkerpid(), InBin, Reason]),
            throw({error, ?ERROR_CODE_INTERNAL_SERVER_ERROR});
        InMsg ->
            ?LOG_INFO("==>[http] gate:~p roleworker:~p c=>s msg:~p~n",
                [self(), ag_engine_storage:get_roleworkerpid(), InMsg]),
            {ok, InMsg}
    end.

do_msg_apply(InMsg, MsgCarrier, UserSetting) ->
    RoleWorkerPid = maps:get(agent_pid, UserSetting, undefined),
    ag_engine_worker_module:worker_touch_me(RoleWorkerPid),
    if
        MsgCarrier ->
            {_, OutMsg} = ag_engine_msg_executer:msg_carrier_apply(RoleWorkerPid, InMsg),
            {ok, OutMsg};
        true ->
            {_, OutMsg} = ag_engine_msg_executer:msg_single_apply(InMsg, false),
            {ok, OutMsg}
    end.

do_encode(_, [], _) ->
    {ok, []};
do_encode(_, <<>>, _) ->
    {ok, <<>>};
do_encode(Proto, OutMsg, Filters) ->
    ?LOG_INFO("==>[http] gate:~p roleworker:~p s=>c msg:~p~n",
        [self(), ag_engine_storage:get_roleworkerpid(), OutMsg]),
    case catch ag_engine_protocol_codec:encode(Proto, OutMsg, Filters) of
        {'EXIT', Reason} ->
            ?LOG_ERROR("==>[http] gate:~p roleworker:~p encode msg:~p error reason:~p~n",
                [self(), ag_engine_storage:get_roleworkerpid(), OutMsg, Reason]),
            throw({error, ?ERROR_CODE_INTERNAL_SERVER_ERROR});
        OutBin ->
            ?LOG_DEBUG("==>[http] gate:~p roleworker:~p s=>c data:~p~n",
                [self(), ag_engine_storage:get_roleworkerpid(), OutBin]),
            {ok, OutBin}
    end.

make_error_package(ErrorInfo, MsgCarrier) ->
    ErrorPack =
        case ErrorInfo of
            'p_no' ->
                ag_engine_message_helper:pack_code(?MSG_INTERNAL_ERROR, ?ERROR_CODE_PACKAGE_NUMBER);
            false ->
                ag_engine_message_helper:pack_code(?MSG_INTERNAL_ERROR, ?ERROR_CODE_AUTH_PLAYER_FAILED);
            time_out ->
                ag_engine_message_helper:pack_code(?MSG_INTERNAL_ERROR, ?ERROR_CODE_ILLEGAL_REQUEST);
            require_version ->
                ag_engine_message_helper:pack_code(?MSG_INTERNAL_ERROR, ?ERROR_CODE_ILLEGAL_VERSION_REQUEST);
            _ ->
                ag_engine_message_helper:pack_code(?MSG_INTERNAL_ERROR, ErrorInfo)
        end,
    case MsgCarrier of
        true ->
            [ErrorPack];
        false ->
            ErrorPack
    end.

merge_msg_headers(Headers, MsgList) when is_list(MsgList) ->
    [maps:merge(Headers, Msg) || Msg <- MsgList];
merge_msg_headers(Headers, Msg) ->
    maps:merge(Headers, Msg).