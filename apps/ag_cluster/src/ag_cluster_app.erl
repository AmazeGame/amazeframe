%%%-------------------------------------------------------------------
%%% @author ayongbc <ayongbc@sina.com> 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_cluster_app).

-behaviour(application).
-include_lib("ag_base/include/agb_debuglogger.hrl").

%% Application callbacks
-export([
    start/2,
    stop/1
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
    StartArgs :: term()
) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    {ok, Pid} = ag_cluster_sup:start_link(),
    case application:get_env(adapter) of
        undefined ->
            ?LOG_INFO("ag_cluster_manager init  gen_env adapter undefined"),
            {error, "ag_clusterconfigerror"};
        {'ok', Adapter} ->
            ?LOG_INFO("ag_cluster_manager init  build adapter ~p~n", [Adapter]),
            ag_cluster_variable:put(adapter, Adapter),
            Adapter:init(),
            {ok, Pid}
    end.

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
