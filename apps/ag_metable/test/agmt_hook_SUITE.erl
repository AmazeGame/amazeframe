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
-module(ag_metable_hook_SUITE).

-import(ct_helper, [config/2]).
-include("ag_metable.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([all/0,test_table_transform/1]).

all() ->
    [{group, basic},test_util,test_to_value].


test_table_transform(_Config) ->
    TableName1_atom = player,
    TableName1_binary = <<"player">>,
    TableName1_str = "player",
    ?assertEqual('PLAYER',ag_metable_hook:table_transform(file,TableName1_atom,trans_upper)),
    ?assertEqual(<<"PLAYER">>,ag_metable_hook:table_transform(file,TableName1_binary,trans_upper)),
    ?assertEqual("PLAYER",ag_metable_hook:table_transform(file,TableName1_str,trans_upper)).