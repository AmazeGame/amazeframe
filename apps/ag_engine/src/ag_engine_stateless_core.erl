%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_stateless_core).


-include("ag_engine_core_defines.hrl").
-include("ag_engine_code_defines.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").
-include("ag_engine.hrl").
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

-spec register_withid(Message :: map()) ->
    {boolean(), MsgBody :: map()}.
register_withid(#{?INTERNAL_DEFINE_ID := Id} = Message) ->
    GateNode = node(),
    GatePid = self(),
    NewMessage = Message#{?INTERNAL_DEFINE_CLIENT_IP => ag_engine_storage:get_client_ip()},
    case ag_engine_cluster:player_from_id(Id) of
        undefined ->
            create_start(NewMessage, GatePid, GateNode);
        #online_player{id = Id, security = Security} = OnlinePlayer ->
            case login_online_player(NewMessage, OnlinePlayer) of
                {false, MsgBody} ->
                    {false, MsgBody};
                {true, MsgBody} ->
                    {true, MsgBody#{
                        ?INTERNAL_DEFINE_ID => Id,
                        ?INTERNAL_DEFINE_SECURITY => Security
                    }}
            end
    end.

-spec register_withoutid(Message :: map()) ->
    {boolean(), MsgBody :: map()}.
register_withoutid(Message) ->
    IdType =
        case maps:is_key(?INTERNAL_DEFINE_ID_TYPE, Message) of
            true -> maps:get(?INTERNAL_DEFINE_ID_TYPE, Message);
            false -> ?INTERNAL_DEFINE_ID_TYPE_SELF
        end,
    Id = ag_idcreator:gen_newid(),
    UserContext = Message#{
        ?INTERNAL_DEFINE_ID_TYPE => IdType,
        ?INTERNAL_DEFINE_ID => Id,
        ?INTERNAL_DEFINE_CLIENT_IP => ag_engine_storage:get_client_ip()
    },
    create_start(UserContext, self(), node()).

-spec login(Message :: map()) ->
    {boolean(), MsgBody :: map()}.
login(#{?INTERNAL_DEFINE_ID := Id} = Message) ->
    GateNode = node(),
    GatePid = self(),
    case ag_engine_cluster:player_from_id(Id) of
        undefined ->
            case maps:is_key(?INTERNAL_DEFINE_ID_TYPE, Message) of
                false ->
                    MsgBody = #{?MESSAGE_ERROR_CODE_KEY => ?ERROR_CODE_CANNOT_FIND_DEVICE},
                    {false, MsgBody};
                true ->
                    exist_start(Message#{?INTERNAL_DEFINE_CLIENT_IP => ag_engine_storage:get_client_ip()}, GatePid, GateNode)
            end;
        OnlinePlayer ->
            login_online_player(Message#{?INTERNAL_DEFINE_CLIENT_IP => ag_engine_storage:get_client_ip()}, OnlinePlayer)
    end.

-spec enter_with_security(Message :: map()) ->
    {boolean(), MsgBody :: map()}.
enter_with_security(#{?INTERNAL_DEFINE_SESSION := Session} = Message) ->
    case ag_engine_cluster:player_from_session(Session) of
        undefined ->
            MsgBody = #{?MESSAGE_ERROR_CODE_KEY => ?ERROR_CODE_CANNOT_FIND_SESSION},
            {false, MsgBody};
        #online_player{id = Id, security = Security} = OnlinePlayer ->
            LS0 = ag_engine_core:get_local_setting(),
            LocalSetting = LS0#{
                ?INTERNAL_DEFINE_SECURITY => Security
            },
            case ag_engine_core:validate(Message, LocalSetting) of
                {false, MsgBody} ->
                    {false, MsgBody};
                true ->
                    storage_info(OnlinePlayer),
                    ag_engine_cluster:update_online_player_info(Id, self(), node()),
                    {true, #{}}
            end
    end.

-spec enter_with_temporary(Message :: map()) ->
    {boolean(), MsgBody :: map()}.
enter_with_temporary(#{?INTERNAL_DEFINE_SESSION := Session} = Message) ->
    case ag_engine_cluster:player_from_session(Session) of
        undefined ->
            MsgBody = #{?MESSAGE_ERROR_CODE_KEY => ?ERROR_CODE_CANNOT_FIND_SESSION},
            {false, MsgBody};
        #online_player{id = Id} = OnlinePlayer ->
            LS0 = ag_engine_core:get_local_setting(),
            TemporarySecurity = ag_engine_cluster:get_temporary_security(Id),
            LocalSetting = LS0#{?INTERNAL_DEFINE_CLIENT_KEY => TemporarySecurity},
            case ag_engine_core:validate(Message, LocalSetting) of
                {false, MsgBody} ->
                    {false, MsgBody};
                true ->
                    storage_info(OnlinePlayer),
                    ag_engine_cluster:update_online_player_info(Id, self(), node()),
                    LocalTime = integer_to_binary(erlang:system_time(?AUTH_SYSTEM_TYPE)),
                    #{?INTERNAL_DEFINE_APP_ID := AppId, ?INTERNAL_DEFINE_CLIENT_VER := VerReq} = LocalSetting,
                    TemporarySecurity2 = ag_engine_authenticate:sign([AppId, LocalTime, VerReq]),
                    WithSecurityReply = ag_engine_authenticate:make_auth_temporary(TemporarySecurity2, #{}),
                    ag_engine_cluster:put_temporary_security(Id, TemporarySecurity2),
                    {true, WithSecurityReply}
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
login_online_player(Message, OnlinePlayer = #online_player{gate_pid = GatePid}) ->
    case ag_engine_login_event:on_login_online_player(Message) of
        {kick_online, KickMsg} ->
            ag_engine_core:send_kick_user_msg(GatePid, KickMsg),
            case application:get_env(ag_engine, login_new_session, true) of
                true ->
                    login_new_session_by_online(Message, OnlinePlayer);
                false ->
                    login_retain_session_by_online(Message, OnlinePlayer)
            end;
        {forbid_login, ForbidMsg} ->
            {false, ForbidMsg#{?MESSAGE_ERROR_CODE_KEY => ?ERROR_CODE_IS_ONLINE}};
        {multi_login, _} ->
            login_retain_session_by_online(Message, OnlinePlayer)
    end.

login_new_session_by_online(Message, #online_player{id = Id, security = Security} = OnlinePlayer) ->
    LS0 = ag_engine_core:get_local_setting(),
    LocalSetting = LS0#{?INTERNAL_DEFINE_SECURITY => Security},
    case ag_engine_authenticate:validate(Message, LocalSetting) of
        false ->
            MsgBody = #{?MESSAGE_ERROR_CODE_KEY => ?ERROR_CODE_AUTH_PLAYER_FAILED},
            {false, MsgBody};
        true ->
            NewSession = ag_idcreator:gen_newid(),
            Session = agb_convertor:to_binary(NewSession),
            storage_info(OnlinePlayer#online_player{session = Session}),
            ag_engine_cluster:update_online_player_info(Id, self(), node(), Session),
            MsgBody = #{
                ?INTERNAL_DEFINE_SESSION => Session
            },
            {true, MsgBody}
    end.

login_retain_session_by_online(Message, #online_player{security = Security, session = Session} = OnlinePlayer) ->
    LS0 = ag_engine_core:get_local_setting(),
    LocalSetting = LS0#{?INTERNAL_DEFINE_SECURITY => Security},
    case ag_engine_authenticate:validate(Message, LocalSetting) of
        false ->
            MsgBody = #{?MESSAGE_ERROR_CODE_KEY => ?ERROR_CODE_AUTH_PLAYER_FAILED},
            {false, MsgBody};
        true ->
            storage_info(OnlinePlayer),
            MsgBody = #{?INTERNAL_DEFINE_SESSION => Session},
            {true, MsgBody}
    end.

storage_info(#online_player{id = Id, idtype = IdType, security = Security, archive = Archive, session = Session}) ->
    ag_engine_storage:set_id(Id),
    ag_engine_storage:set_idtype(IdType),
    ag_engine_storage:set_session(Session),
    ag_engine_storage:set_archive(Archive),
    ag_engine_storage:set_security(Security).

create_start(#{?INTERNAL_DEFINE_ID := Id, ?INTERNAL_DEFINE_ID_TYPE := IdType} = UserContext, GatePid, GateNode) ->
    NewSession = ag_idcreator:gen_newid(),
    Session = agb_convertor:to_binary(NewSession),
    {Archive, Security} = ag_engine_archive:create_user(UserContext),
    case ag_engine_cluster:player_from_archive(Archive) of
        undefined ->
            ok;
        #online_player{id = OldID} ->
            _ = ag_engine_cluster:unregister_player(OldID),
            ok
    end,
    _ = ag_engine_cluster:register_player(Id, IdType, disabled, disabled, GatePid, GateNode, Session, Security, Archive),
    MsgBody = #{
        ?INTERNAL_DEFINE_ID => Id,
        ?INTERNAL_DEFINE_SECURITY => Security,
        ?INTERNAL_DEFINE_SESSION => Session
    },
    {true, MsgBody}.

exist_start(#{?INTERNAL_DEFINE_ID := Id, ?INTERNAL_DEFINE_ID_TYPE := IdType} = UserContext, GatePid, GateNode) ->
    case ag_engine_archive:load_user(UserContext) of
        {Archive, Security} ->
            NewSession = ag_idcreator:gen_newid(),
            Session = agb_convertor:to_binary(NewSession),
            case ag_engine_cluster:player_from_archive(Archive) of
                undefined ->
                    ok;
                #online_player{id = OldID} ->
                    ag_engine_cluster:unregister_player(OldID),
                    ok
            end,
            ag_engine_cluster:register_player(Id, IdType, disabled, disabled, GatePid, GateNode, Session, Security, Archive),
            storage_info(#online_player{
                id = Id,
                idtype = IdType,
                security = Security,
                archive = Archive,
                session = Session}
            ),
            MsgBody = #{?INTERNAL_DEFINE_SESSION => Session},
            {true, MsgBody};
        _ ->
            {false, #{?MESSAGE_ERROR_CODE_KEY => ?ERROR_CODE_USER_NOT_EXIST}}
    end.

