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
-module(ag_monitor_config).

%% API
-export([
    init/0,
    put/2,
    get/1
]).

-spec init() ->
    atom().
init() ->
    agb_ets:init(table()),
    case application:get_env(monitorlist) of
        {ok, MonitorList} ->
            [init_monitor(Monitor) || Monitor <- MonitorList];
        _ ->
            ignore
    end.

init_monitor(Monitor) ->
    case application:get_env(Monitor) of
        {ok, MonitorConfig} ->
            HandleList = proplists:get_value(handlelist, MonitorConfig),
            ag_monitor_config:put(Monitor, HandleList),
            init_handle(HandleList, MonitorConfig);
        _ ->
            ignore
    end.

init_handle([], _) ->
    ignore;
init_handle([Handle | T], MessageMonitor) ->
    case proplists:get_value(Handle, MessageMonitor) of
        undefined ->
            ignore;
        Args ->
            ag_monitor_config:put(Handle, Args)
    end,
    init_handle(T, MessageMonitor).

-spec table() ->
    atom().
table() ->
    'ag_monitor_config'.

-spec put(Key :: any(), Value :: term()) ->
    boolean().
put(Key, Value) ->
    agb_ets:put(table(), Key, Value).

-spec get(Key :: any()) ->
    term().
get(Key) ->
    case agb_ets:lookup(table(), Key) of
        [] ->
            undefined;
        Obj ->
            Obj
    end.

