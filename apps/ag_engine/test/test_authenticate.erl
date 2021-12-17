%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @copyright (C) 2019, Harbour Studios
%%% @doc
%%%
%%% @end
%%% Created : 05. 七月 2019 17:45
%%%-------------------------------------------------------------------
-module(test_authenticate).

-behaviour(ag_engine_authenticate).

-include_lib("ag_base/include/ag_debuglogger.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").
%% API
-export([validate/2, sign/1, check_peer_time/1]).

-spec validate(PeerInfo :: map(), LocalSetting :: map()) ->
    true|false|p_no|require_version|time_out|{error, ErrCode :: integer()}.
validate(_PeerInfo, _LocalSetting) ->
    true.

sign(_Args) ->
    111111.

check_peer_time(_Time) ->
    true.
