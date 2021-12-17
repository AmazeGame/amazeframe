%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_core).

-include("ag_engine_core_defines.hrl").
-include("ag_engine_code_defines.hrl").
-include("ag_engine.hrl").


-type operation_type() :: kick_online | forbid_login | multi_login.

-export_type([operation_type/0]).

%% API
-export([
    register_withid/1,
    register_withoutid/1
]).
-export([login/1]).
-export([
    enter_with_security/1,
    enter_with_temporary/1
]).
-export([
    get_local_setting/0,
    validate/2
]).
-export([send_kick_user_msg/2]).

-spec register_withid(Message :: map()) ->
    {boolean(), MsgBody :: map()}.
register_withid(Message) ->
    case application:get_env(ag_engine, is_use_roleworker) of
        undefined ->
            ag_engine_stateful_core:register_withid(Message);
        {ok, true} ->
            ag_engine_stateful_core:register_withid(Message);
        {ok, false} ->
            ag_engine_stateless_core:register_withid(Message)
    end.

-spec register_withoutid(Message :: map()) ->
    {boolean(), MsgBody :: map()}.
register_withoutid(Message) ->
    case application:get_env(ag_engine, is_use_roleworker) of
        undefined ->
            ag_engine_stateful_core:register_withoutid(Message);
        {ok, true} ->
            ag_engine_stateful_core:register_withoutid(Message);
        {ok, false} ->
            ag_engine_stateless_core:register_withoutid(Message)
    end.

-spec login(Message :: map()) ->
    {boolean(), MsgBody :: map()}.
login(Message) ->
    case application:get_env(ag_engine, is_use_roleworker) of
        undefined ->
            ag_engine_stateful_core:login(Message);
        {ok, true} ->
            ag_engine_stateful_core:login(Message);
        {ok, false} ->
            ag_engine_stateless_core:login(Message)
    end.

-spec enter_with_security(Message :: map()) ->
    {boolean(), MsgBody :: map()}.
enter_with_security(Message) ->
    case application:get_env(ag_engine, is_use_roleworker) of
        undefined ->
            ag_engine_stateful_core:enter_with_security(Message);
        {ok, true} ->
            ag_engine_stateful_core:enter_with_security(Message);
        {ok, false} ->
            ag_engine_stateless_core:enter_with_security(Message)
    end.

-spec enter_with_temporary(Message :: map()) ->
    {boolean(), MsgBody :: map()}.
enter_with_temporary(Message) ->
    case application:get_env(ag_engine, is_use_roleworker) of
        undefined ->
            ag_engine_stateful_core:enter_with_temporary(Message);
        {ok, true} ->
            ag_engine_stateful_core:enter_with_temporary(Message);
        {ok, false} ->
            ag_engine_stateless_core:enter_with_temporary(Message)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_local_setting() ->
    map().
get_local_setting() ->
    {ok, AppId} = application:get_env(ag_engine, app_id),
    {ok, ClientKey} = application:get_env(ag_engine, app_client_key),
    {ok, VerReq} = application:get_env(ag_engine, app_client_version),
    {ok, MsgValidateSecond} = application:get_env(ag_engine, msg_validate_second),
    #{
        ?INTERNAL_DEFINE_APP_ID => AppId,
        ?INTERNAL_DEFINE_CLIENT_KEY => ClientKey,
        ?INTERNAL_DEFINE_CLIENT_VER => VerReq,
        ?INTERNAL_DEFINE_MSG_VALIDATE_SECONDS => MsgValidateSecond
    }.

validate(Message, LocalSetting) ->
    case ag_engine_authenticate:validate(Message, LocalSetting) of
        false ->
            MsgBody = #{
                ?MESSAGE_ERROR_CODE_KEY => ?ERROR_CODE_AUTH_PLAYER_FAILED
            },
            {false, MsgBody};
        require_version ->
            MsgBody = #{
                ?MESSAGE_ERROR_CODE_KEY => ?ERROR_CODE_ILLEGAL_VERSION_REQUEST
            },
            {false, MsgBody};
        time_out ->
            MsgBody = #{
                ?MESSAGE_ERROR_CODE_KEY => ?ERROR_CODE_ILLEGAL_REQUEST
            },
            {false, MsgBody};
        {error, ErrorCode} ->
            MsgBody = #{
                ?MESSAGE_ERROR_CODE_KEY => ErrorCode
            },
            {false, MsgBody};
        true ->
            true
    end.

%% add by blackcat 2019/11/5
%% 优化踢号逻辑
send_kick_user_msg(undefined, _) ->
    ignore;
send_kick_user_msg(_, undefined) ->
    ignore;
send_kick_user_msg(GatePid, KickMsg) ->
    GatePid ! {kick_user, kick_user_other_login, KickMsg}.
