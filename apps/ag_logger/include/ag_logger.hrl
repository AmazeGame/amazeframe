-author("Adrianx Lau <adrianx.lau@gmail.com>").
-ifndef(_AMAZE_GAME_LOGGER_H_).
-define(_AMAZE_GAME_LOGGER_H_,1).

-define(PRINT(Format, Args), io:format(Format, Args)).
-define(PRINT(Format), io:format(Format)).

-define(LOG_TYPE_BI,logtype_bi).
-define(LOG_TYPE_OP,logtype_operation).
-define(LOG_TYPE_DEV,logtype_develop).

-define(LOCATION,#{mfa=>{?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY},
    line=>?LINE,
    file=>?FILE}).

-define(LOG_EMERGENCY(A),?DO_LOG(emergency,[A,?LOCATION])).
-define(LOG_EMERGENCY(A,B),?DO_LOG(emergency,[A,B,?LOCATION])).

-define(LOG_ALERT(A),?DO_LOG(alert,[A,?LOCATION])).
-define(LOG_ALERT(A,B),?DO_LOG(alert,[A,B,?LOCATION])).

-define(LOG_CRITICAL(A),?DO_LOG(critical,[A,?LOCATION])).
-define(LOG_CRITICAL(A,B),?DO_LOG(critical,[A,B,?LOCATION])).

-define(LOG_ERROR(A),?DO_LOG(error,[A,?LOCATION])).
-define(LOG_ERROR(A,B),?DO_LOG(error,[A,B,?LOCATION])).

-define(LOG_WARNING(A),?DO_LOG(warning,[A,?LOCATION])).
-define(LOG_WARNING(A,B),?DO_LOG(warning,[A,B,?LOCATION])).

-define(LOG_NOTICE(A),?DO_LOG(notice,[A,?LOCATION])).
-define(LOG_NOTICE(A,B),?DO_LOG(notice,[A,B,?LOCATION])).

-define(LOG_INFO(A),?DO_LOG(info,[A,?LOCATION])).
-define(LOG_INFO(A,B),?DO_LOG(info,[A,B,?LOCATION])).

-define(LOG_DEBUG(A),?DO_LOG(debug,[A,?LOCATION])).
-define(LOG_DEBUG(A,B),?DO_LOG(debug,[A,B,?LOCATION])).

%%%-----------------------------------------------------------------
%%% Internal, i.e. not intended for direct use in code - use above
%%% macros instead!
-define(DO_LOG(Level,Args),
    case logger:allow(Level,?MODULE) of
        true ->
            apply(logger,macro_log,[?LOCATION,Level|Args]);
        false ->
            ok
    end).

-endif.
