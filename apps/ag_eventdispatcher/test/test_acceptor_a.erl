%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.29
%%%-------------------------------------------------------------------
%%%
-module(test_acceptor_a).

-include_lib("ag_base/include/agb_debuglogger.hrl").

-behaviour(ag_eventdispatcher_acceptor).
%% API
-export([
    on_event/2
]).

-spec on_event(term(), term()) ->
    tuple().
on_event(System, Event) ->
    ?LOG_DEBUG("recv system:~p event:~p", [System, Event]),
    {EventName, Pid} = Event,
    Pid ! {EventName, ?MODULE}.
