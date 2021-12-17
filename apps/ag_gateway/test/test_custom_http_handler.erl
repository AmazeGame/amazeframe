%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(test_custom_http_handler).

-include_lib("ag_base/include/ag_debuglogger.hrl").
-export([init/2]).


-spec init(Req :: map(), Opts :: list()) -> {cowboy_websocket, Req :: map(), Opts :: list(), Opt :: map()} | {stop, Req :: map(), Opts :: list()}.
init(Req, Opts) ->

    ?LOG_INFO(">>>>>>>>>>>>>>>villas_heartbeat_handler rev QueryString:~p~n", [Req]),
    {ok, Body, _} = cowboy_req:read_body(Req),
    ?LOG_INFO(">>>>>>>>>>>>>>>villas_heartbeat_handler rev Body:~p~n", [Body]),

    {ok, cowboy_req:reply(200, #{}, agb_json:encode(#{}), Req), Opts}.