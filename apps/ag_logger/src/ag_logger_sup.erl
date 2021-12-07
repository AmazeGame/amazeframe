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
%%%-------------------------------------------------------------------
%% @doc ag_logger top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ag_logger_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).
-export([start_child/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILDA(I, Type,Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    ChildSpecs = [
        ?CHILD(ag_logger_manager, worker)
    ],
    {ok, { {one_for_one, 5, 10}, ChildSpecs} }.

-spec start_child(ChildSpec::supervisor:child_spec() | (List :: [term()]) )->supervisor:startchild_ret() .
start_child(ChildSpec)->
    supervisor:start_child(ag_logger_sup,ChildSpec).

%% internal functions
