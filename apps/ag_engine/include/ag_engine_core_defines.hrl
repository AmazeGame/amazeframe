%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-ifndef(__ENGINE_CORE_DEFINES_HRL_).
-define(__ENGINE_CORE_DEFINES_HRL_,1).


-define(MESSAGE_NAME_KEY,           <<"name">>).
-define(MESSAGE_SESSION_KEY,     	<<"session">>).
-define(MESSAGE_NAME_KEY_ATOM,         name).
-define(MESSAGE_ERROR_CODE_KEY,     <<"code">>).
-define(MESSAGE_ERROR_INFO_KEY,     <<"info">>).



-define(INTERNAL_DEFINE_ID_TYPE_SELF,	<<"fotogame">>).

-define(INTERNAL_DEFINE_ID,         <<"id">>).
-define(INTERNAL_DEFINE_ID_TYPE,    <<"type">>).
-define(INTERNAL_DEFINE_SECURITY,   <<"security">>).
-define(INTERNAL_DEFINE_ARCHIEVE,   <<"archive">>).
-define(INTERNAL_DEFINE_SESSION,    <<"session">>).
-define(INTERNAL_DEFINE_GAME_SERVER,    	<<"ag_game_server">>).
-define(INTERNAL_DEFINE_ECHO,       <<"echo">>).
-define(INTERNAL_DEFINE_PASSWORD,   <<"password">>).
-define(INTERNAL_DEFINE_DEVICEID,   <<"deviceid">>).
-define(INTERNAL_DEFINE_3RD_TOKEN,   <<"3rdtoken">>).
-define(INTERNAL_DEFINE_CLIENT_IP,      <<"ip">>).

-define(INTERNAL_DEFINE_APP_ID,      <<"appid">>).
-define(INTERNAL_DEFINE_CLIENT_KEY,  <<"clientkey">>).
-define(INTERNAL_DEFINE_MSG_VALIDATE_SECONDS,  60).


-define(INTERNAL_DEFINE_TIME,       <<"time">>).
-define(INTERNAL_DEFINE_SIGN,       <<"sign">>).
-define(INTERNAL_DEFINE_CLIENT_VER,  <<"pver">>).

-define(INTERNAL_DEFINE_PACKAGE_NUMBER, <<"packagenumber">>).



%%系统内部错误
-define(MSG_INTERNAL_ERROR,             ag_engine:get_inerror_name_key()).

-define(MSG_CONNECTION_CLOSE,           <<"connection_close">>).

%%  "session":Session
%%  "x-application-id": appid,
%%  "time": Time,
%%  "sign": temporary_token,
%%  "proto-version": version

-endif.%__GEN_GAME_CORE_DEFAILES_HRL_
