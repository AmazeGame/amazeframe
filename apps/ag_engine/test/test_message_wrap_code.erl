%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(test_message_wrap_code).

-behaviour(ag_engine_message_wrap_code).

%% API
-export([get_infos/0]).

get_infos()->
    [
        {100,<<"100">>},
        {200,<<"200">>},
        {300,<<"300">>},
        {400,<<"400">>}
    ].