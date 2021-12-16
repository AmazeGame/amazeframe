%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2017-2020 VMware, Inc. or its affiliates.  All rights reserved.
%%
%% @hidden
-module(ag_machine_node).
-behaviour(ra_machine).

-compile({no_auto_import,[apply/3]}).

-include("ag_node.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").

-export([
    init/1,
    apply/3,
    state_enter/2
]).

init(_) ->
    #{}.

apply(_Meta, {watch, Pid}, State) ->
    ?LOG_INFO("---------watch :~p~n",[Pid]),
    {State, ok, [{monitor, process, Pid}]};
apply(_Meta, {down, Pid, _X}, State) ->
    ?LOG_INFO("---------nodedown node :~p ~p~n", [node(Pid),_X]),
    case ra_leaderboard:lookup_leader(ag_node_ra:get_cluster_name()) of
        {_, Node} when Node == node() ->
            DownServerId = ag_node_ra:get_server_id_by_node(node(Pid)),
            ag_node_ra:remove_member(DownServerId);
        _ ->
            ignore
    end,
    {State, ok, []};
apply(_, {timeout, on_leader}, State) ->
    ?LOG_INFO("--------- timeout,on_leader~n"),
    ag_node_ra:on_enter_leader(),
    {State, ok, []};
apply(_Meta, _A, State) ->
    ?LOG_INFO("apply:~p~n",[_A]),
    {State, ok, []}.

state_enter(leader, _) ->
    ?LOG_INFO("---------node:~p enter leader", [node()]),
    [{timer, on_leader, 1}];
state_enter(follower, _) ->
    ?LOG_INFO("---------node:~p enter follower", [node()]),
    [];
state_enter(Act, _) ->
    ?LOG_INFO("state_enter:~p~n",[Act]),
    [].




