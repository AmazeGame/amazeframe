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
-module(ag_bizlogger_process_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_PROCESS_NUM, 5).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(#{biz := Biz} = Args) ->
    supervisor:start_link({local, make_proc_biz_name(Biz)}, ?MODULE, [Args]).

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
init([#{biz := Biz, process_num := Count, handle := Handle, handle_config := HandleConfig}]) ->
    Count1 = if Count > ?MAX_PROCESS_NUM -> ?MAX_PROCESS_NUM; true -> Count end,
    ChildList =
        lists:map(
            fun(Index) ->
                Name = apply(Handle, make_process_name, [Biz, Index]),
                MfcTuple = {Handle, start_link, [HandleConfig#{index => Index, biz => Biz}]},
                {Name, MfcTuple, permanent, 5000, worker, [Handle]}
            end, lists:seq(1, Count1)),
    {ok, {{one_for_one, 5, 10}, ChildList}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_proc_biz_name(Biz) when is_binary(Biz) ->
    make_proc_biz_name(binary_to_list(Biz));
make_proc_biz_name(Biz) when is_atom(Biz) ->
    make_proc_biz_name(atom_to_list(Biz));
make_proc_biz_name(Biz) when is_list(Biz) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ Biz).