%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
%%%
-module(ag_monitor_message_api).

-include("ag_monitor_msg_tracking_struct.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").

-callback start(term()) -> ok.
-callback stop() -> ok.
-callback observe(map()) -> ok.
%% API
-export([observe/1]).

observe(Metrics) ->
    case ag_monitor_config:get(messagemonitor) of
        undefined ->
            ignore;
        {_, HandleList} ->
            [observe(Handle, Metrics) || Handle <- HandleList]
    end.

observe(Handle, Metrics) ->
    case ag_monitor_config:get(Handle) of
        {_, #{isopen := true}} ->
            Handle:observe(Metrics),
            ok;
        _ ->
            ignore
    end.