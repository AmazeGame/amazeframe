%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.28
%%%-------------------------------------------------------------------
%%%
-module(ag_logger_app).

-behaviour(application).
-include("ag_logger.hrl").

-export([start/2, stop/1]).
-export([test_log/0]).

start(_StartType, _StartArgs) ->
    ag_logger_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
test_log()->
    ?LOG_DEBUG("-----clean_old_info exist------ Result: ~p~n",[{#{<<"echo">> => 0,<<"name">> => <<"RSaveDB">>}}]).
