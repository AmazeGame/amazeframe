%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
%%%
-module(ag_bizlogger_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}
    | ignore.
init([]) ->
    {ok, BizList} = application:get_env(ag_bizlogger,biz_list),
    
    Fun =
        fun(#{biz := Biz} = BizConfig) ->
            Name = atom_to_list(ag_bizlogger_process_sup) ++ "_" ++ atom_to_list(Biz),
            {Name, {ag_bizlogger_process_sup, start_link, [BizConfig]}, permanent, 5000, supervisor, [ag_bizlogger_process_sup]}
        end,
    ChildList = lists:map(Fun, BizList),
    {ok, {{one_for_one, 5, 10}, ChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
