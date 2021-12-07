%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.11.02
%%%-------------------------------------------------------------------
%%%
-module(ag_debuglogger_filter).

-include_lib("ag_base/include/agb_debuglogger.hrl").

%% API
-export([dev_log/2]).

-spec dev_log(LogEvent, _) ->
    logger:filter_return()
    when LogEvent :: logger:log_event().

dev_log(LogEvent, _) ->
    filter_dev_log(LogEvent).

filter_dev_log(Event = #{level:= error}) ->
    Event;
filter_dev_log(Event = #{meta:=Meta}) ->
    case maps:find(logtype, Meta) of
        {ok, Type} ->
            if
                Type =/= ?LOG_TYPE_DEV ->
                    stop;
                true ->
                    Event
            end;
        error ->
            stop
    end;
filter_dev_log(_) ->
    ignore.
