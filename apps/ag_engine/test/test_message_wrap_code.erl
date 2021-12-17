%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @copyright (C) 2019, Harbour Studios
%%% @doc
%%%
%%% @end
%%% Created : 08. 七月 2019 12:07
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