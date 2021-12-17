%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_gateway_common).

%% API
-export([get_opag_key/3]).

-spec get_opag_key(term(), list()|map(), term()) ->
    term().
get_opag_key(Key, Opts, Default) when is_list(Opts) ->
    get_opag_key(Key, maps:from_list(Opts), Default);
get_opag_key(Key, Opts, Default) when is_map(Opts) ->
    maps:get(Key, Opts, Default).