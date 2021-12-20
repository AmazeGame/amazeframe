%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(test_authenticate2).

-behaviour(ag_engine_authenticate).

-include_lib("ag_base/include/agb_debuglogger.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").
%% API
-export([validate/2]).

-spec validate(PeerInfo :: map(), LocalSetting :: map()) ->
    true|false|p_no|require_version|time_out|{error, ErrCode :: integer()}.
validate(_PeerInfo, _LocalSetting) ->
    true.