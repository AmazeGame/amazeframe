%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(test_platform_fb).

-behaviour(ag_engine_platform).

-include_lib("ag_base/include/agb_debuglogger.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").

-export([
    auth_types/0,
    validate/2,
    transform_id/2,
    restore_id/2
]).
%% API
%%% 一个fb账号只能绑定一个账号
auth_types()->
    [<<"fb">>].

validate(_Type, _SecurityInfo) ->
    ok.

transform_id(_Type,Id)->
    <<Id/binary,"@facebook">>.

restore_id(_,Id)->
    binary:part(Id, {0, byte_size(Id) - 9}).