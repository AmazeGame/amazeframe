%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_inner_authenticate).
-include_lib("ag_base/include/agb_debuglogger.hrl").
-include("ag_engine_core_defines.hrl").
-include("ag_engine.hrl").

%% API
-export([
    validate/2,
    sign/1,
    check_peer_time/2
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% 实现两种签名函数
%% @end
-spec sign(list()) ->
    binary().
sign([AppId, Time, ClientKey]) ->
    Input = iolist_to_binary([AppId, Time, ClientKey]),
    list_to_binary(agb_hex:bin_to_hexlstr(erlang:md5(Input)));
sign([AppId, Time, ClientKey, Security]) ->
    ?LOG_DEBUG("inner_sign :~p", [{AppId, Time, ClientKey, Security}]),
    Input = iolist_to_binary([Security, Time]),
    Security2 = list_to_binary(agb_hex:bin_to_hexlstr(erlang:md5(Input))),
    Input2 = iolist_to_binary([AppId, Time, ClientKey, Security2]),
    list_to_binary(agb_hex:bin_to_hexlstr(erlang:md5(Input2))).

-spec check_peer_time(PeerTime :: integer() | string(), integer()) ->
    boolean().
check_peer_time(PeerTime, ValidateSeconds)
    when is_list(PeerTime) orelse is_binary(PeerTime) orelse is_integer(PeerTime) ->
    PeerUnixTime = agb_convertor:to_integer(PeerTime),
    LocalUnixTime = erlang:system_time(?AUTH_SYSTEM_TYPE),
    Lower = LocalUnixTime - ValidateSeconds,
    Upper = LocalUnixTime + ValidateSeconds,
    Upper >= PeerUnixTime andalso Lower =< PeerUnixTime;
check_peer_time(_, _) ->
    false.

%% @doc
%% 内部实现的包头验证函数， LocalSetting如果包含INTERNAL_DEFINE_SECURITY,使用sign([AppId, Time, ClientKey, Security])
%% 计算签名，否则使用sign([AppId, Time, ClientKey])计算签名
%% @end
-spec validate(PeerInfo :: map(), LocalSetting :: map()) ->
    true | false | require_version | time_out | p_no|  {error, ErrCode :: integer()}.
validate(PeerInfo,
    #{
        ?INTERNAL_DEFINE_APP_ID := AppId,
        ?INTERNAL_DEFINE_CLIENT_VER := VerReq,
        ?INTERNAL_DEFINE_MSG_VALIDATE_SECONDS := ValidateSeconds
    } = LocalSetting) ->
    case ag_engine_message_helper:get_value(?INTERNAL_DEFINE_APP_ID, PeerInfo) of
        PeerAppId when AppId /= PeerAppId ->
            false;
        _PeerAppId ->
            validate_peer_info(PeerInfo, ValidateSeconds, VerReq, LocalSetting)
    end.

validate_peer_info(PeerInfo, ValidateSeconds, VerReq, LocalSetting) ->
    PeerTime = ag_engine_message_helper:get_value(?INTERNAL_DEFINE_TIME, PeerInfo),
    case ag_engine_authenticate:check_peer_time(PeerTime, ValidateSeconds) of
        true ->
            PeerPVersion = ag_engine_message_helper:get_value(?INTERNAL_DEFINE_CLIENT_VER, PeerInfo),
            VerResult = agb_version:compare_version(PeerPVersion, VerReq),
            if
                VerResult >= 0 ->
                    case validate_sign(PeerInfo, LocalSetting) of
                        true ->
                            ag_engine_authenticate:check_package_number(PeerInfo, LocalSetting);
                        false ->
                            false
                    end;
                true ->
                    require_version
            end;
        false ->
            time_out
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
validate_sign(PeerInfo,
    #{
        ?INTERNAL_DEFINE_APP_ID := AppId,
        ?INTERNAL_DEFINE_CLIENT_KEY := ClientKey
    } = LocalSetting) ->
    PeerTime = ag_engine_message_helper:get_value(?INTERNAL_DEFINE_TIME, PeerInfo),
    PeerSign = ag_engine_message_helper:get_value(?INTERNAL_DEFINE_SIGN, PeerInfo),
    Sign =
        case maps:is_key(?INTERNAL_DEFINE_SECURITY, LocalSetting) of
            true ->
                Security = maps:get(?INTERNAL_DEFINE_SECURITY, LocalSetting),
                ag_engine_authenticate:sign([AppId, PeerTime, ClientKey, Security]);
            false ->
                ag_engine_authenticate:sign([AppId, PeerTime, ClientKey])
        end,
    Sign == PeerSign.