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
-module(ag_idcreator_app).


-behaviour(application).

-include_lib("ag_base/include/agb_debuglogger.hrl").
%% Application callbacks
-export([
    start/0, start/2,
    stop/1
]).

-export([
    gen_newid/0, gen_newid/1, gen_newid/2,
    gen_batchid/1, gen_batchid/2, gen_batchid/3
]).


%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    case ag_idcreator_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

-spec(start() ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start() ->
    application:start(ag_idcreator).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) ->
    term()).
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec gen_newid() -> term().
gen_newid() ->
    gen_newid(randchar).

-spec gen_newid(randchar|intacc|snowflake|ets) -> term().
gen_newid(randchar) ->
    ag_idcreator_randchar:gen_newid();
gen_newid(intacc) ->
    gen_newid(intacc, default);
gen_newid(snowflake) ->
    ag_idcreator_snowflake:gen_newid();
gen_newid(ets) ->
    ag_idcreator_id_ets:gen_newid().

-spec gen_newid(intacc, Biz :: atom()) -> term().
gen_newid(intacc, Biz) ->
    ag_idcreator_accumulator:gen_newid(Biz).

-spec gen_batchid(Count :: integer()) -> [term()].
gen_batchid(Count) ->
    gen_batchid(randchar, Count).

-spec gen_batchid(randchar|intacc|snowflake|ets, Count :: integer()) -> [term()].
gen_batchid(randchar, Count) ->
    ag_idcreator_randchar:gen_batchid(Count);
gen_batchid(intacc, Count) ->
    gen_batchid(intacc, default, Count);
gen_batchid(snowflake, Count) ->
    ag_idcreator_snowflake:gen_batchid(Count);
gen_batchid(ets, Count) ->
    ag_idcreator_id_ets:gen_batchid(Count).

-spec gen_batchid(intacc, Biz :: atom(), Count :: integer()) -> [term()].
gen_batchid(intacc, Biz, Count) ->
    ag_idcreator_accumulator:gen_batchid(Biz, Count).
