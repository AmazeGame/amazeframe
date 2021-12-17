%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @copyright (C) 2019, Harbour Studios
%%% @doc
%%% 账号绑定fb
%%% @end
%%% Created : 16. 五月 2019 12:14
%%%-------------------------------------------------------------------
-module(test_platform_fb).

-behaviour(ag_engine_platform).

-include_lib("ag_base/include/ag_debuglogger.hrl").
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