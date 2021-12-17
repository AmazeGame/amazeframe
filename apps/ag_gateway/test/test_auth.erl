%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(test_auth).

-behaviour(ag_engine_authenticate).

-include_lib("ag_engine/include/ag_engine_core_defines.hrl").

%% API
-export([validate/2]).


%%validate(_,_)->
%%    {true,false}.

validate(PeerInfo,_)->
    case maps:find(<<"validate">>,PeerInfo) of
        {ok,V} ->
            agb_convertor:to_atom(V);
        error ->
            true
    end.
