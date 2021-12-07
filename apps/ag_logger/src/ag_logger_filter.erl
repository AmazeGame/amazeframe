%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.28
%%%-------------------------------------------------------------------
%%%
-module(ag_logger_filter).


-include("ag_logger.hrl").

%% API
-export([bi_log/2,op_log/2,dev_log/2]).


%% 数据中心log筛选器，只通过logtype为type_bi的log
-spec bi_log(LogEvent,_) -> logger:filter_return() when
    LogEvent :: logger:log_event().
bi_log(LogEvent,_)->
    filter_bi_log(LogEvent).

filter_bi_log(Event = #{meta:=Meta}) ->
    case maps:find( logtype,Meta ) of
        {ok,Type} ->
            case Type of
                ?LOG_TYPE_BI ->
                    Event;
                _->
                    stop
            end;
        error ->
            stop
    end;
filter_bi_log(_) ->
    ignore.


%% 操作log筛选器，只通过logtype为logtype_operation的log
-spec op_log(LogEvent,_) -> logger:filter_return() when
    LogEvent :: logger:log_event().
op_log(LogEvent,_) ->
    filter_op_log(LogEvent).

filter_op_log(Event = #{meta:=Meta}) ->
    case maps:find( logtype,Meta ) of
        {ok,Type} ->
            case Type of
                ?LOG_TYPE_OP ->
                    Event;
                _->
                    stop
            end;
        error ->
            stop
    end;
filter_op_log(_) ->
    ignore.


-spec dev_log(LogEvent,_) -> logger:filter_return() when
    LogEvent :: logger:log_event().

dev_log(LogEvent,_) ->
    filter_dev_log(LogEvent).

filter_dev_log(Event = #{meta:=Meta}) ->
    case maps:find( logtype,Meta ) of
        {ok,Type} ->
            if
                Type == ?LOG_TYPE_BI ; Type == ?LOG_TYPE_OP ->
                    stop;
                true ->
                    Event
            end ;
        error ->
            Event
    end;
filter_dev_log(_) ->
    ignore.

