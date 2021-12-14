%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------

-ifndef(_DEBUG_LOGGER_AGENT_H_).
-define(_DEBUG_LOGGER_AGENT_H_, 1).

-define(PRINT(Format, Args), io:format(Format, Args)).
-define(PRINT(Format), io:format(Format)).

-define(LOG_TYPE_DEV, logtype_develop).

-define(LOCATION, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
    line => ?LINE,
    file => ?FILE
}).

-define(LOG_EMERGENCY(A), ?DO_LOG(emergency, [A, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_EMERGENCY(A, B), ?DO_LOG(emergency, [A, B, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_ALERT(A), ?DO_LOG(alert, [A, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_ALERT(A, B), ?DO_LOG(alert, [A, B, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_CRITICAL(A), ?DO_LOG(critical, [A, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_CRITICAL(A, B), ?DO_LOG(critical, [A, B, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_ERROR(A), ?DO_LOG(error, [A, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_ERROR(A, B), ?DO_LOG(error, [A, B, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_WARNING(A), ?DO_LOG(warning, [A, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_WARNING(A, B), ?DO_LOG(warning, [A, B, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_NOTICE(A), ?DO_LOG(notice, [A, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_NOTICE(A, B), ?DO_LOG(notice, [A, B, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_INFO(A), ?DO_LOG(info, [A, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_INFO(A, B), ?DO_LOG(info, [A, B, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_DEBUG(A), ?DO_LOG(debug, [A, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).
-define(LOG_DEBUG(A, B), ?DO_LOG(debug, [A, B, ?LOCATION#{logtype => ?LOG_TYPE_DEV}])).

%%%-----------------------------------------------------------------
%%% Internal, i.e. not intended for direct use in code - use above
%%% macros instead!
-define(DO_LOG(Level, Args),
    case logger:allow(Level, ?MODULE) of
        true ->
            erlang:apply(logger, macro_log, [?LOCATION, Level | Args]);
        false ->
            ok
    end).

-endif.

