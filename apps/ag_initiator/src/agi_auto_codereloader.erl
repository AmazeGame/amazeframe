%%%-------------------------------------------------------------------
%%% @author ayongbc <ayongbc@sina.com>
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.09
%%%-------------------------------------------------------------------
-module(agi_auto_codereloader).
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").

-export([
    start/0,
    stop/0,
    start_link/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {last :: calendar:datetime(), tref :: reference()}).

%% External API
%% @spec start() -> ServerRet
%% @doc Start the reloader.
-spec start() -> {'ok', pid()} | 'ignore' | {'error', term()}.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> ok
%% @doc Stop the reloader.
stop() ->
    gen_server:call(?MODULE, stop).

%% @spec is_changed(atom()) -> boolean()
%% @doc true if the loaded module is a beam with a vsn attribute
%%      and does not match the on-disk beam file, returns false otherwise.
is_changed(M) ->
    try
        module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
    catch _:_ ->
        false
    end.

%% @spec start_link() -> ServerRet
%% @doc Start the reloader.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
%% @spec init([]) -> {ok, State}
%% @doc gen_server init, opens the server in an initial state.
-spec init(Args :: term()) -> {ok, State :: #state{}}.
init([]) ->
    TRef = erlang:send_after(1000, self(), doit),
    {ok, #state{last = stamp(), tref = TRef}}.

%% @spec handle_call(Args, From, State) -> tuple()
%% @doc gen_server callback.
handle_call(stop, _From, State) ->
    {stop, shutdown, stopped, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

%% @spec handle_cast(Cast, State) -> tuple()
%% @doc gen_server callback.
handle_cast(_Req, State) ->
    {noreply, State}.

%% @spec handle_info(Info, State) -> tuple()
%% @doc gen_server callback.
handle_info(doit, State) ->
    Now = stamp(),
    doit(State#state.last, Now),
    TRef = erlang:send_after(5000, self(), doit),
    {noreply, State#state{last = Now, tref = TRef}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% @doc gen_server termination callback.
terminate(_Reason, State) ->
    erlang:cancel_timer(State#state.tref),
    ok.

%% @spec code_change(_OldVsn, State, _Extra) -> State
%% @doc gen_server code_change callback (trivial).
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%% Internal API
module_vsn({M, Beam, _Fn}) ->
    {ok, {M, Vsn}} = beam_lib:version(Beam),
    Vsn;
module_vsn(L) when is_list(L) ->
    {_, Attrs} = lists:keyfind(attributes, 1, L),
    {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
    Vsn.

doit(From, To) ->
    [doit(From, To, Module, Filename) || {Module, Filename} <- code:all_loaded(), is_list(Filename)].

doit(From, To, Module, Filename) ->
    case {file:read_file_info(Filename), is_changed(Module)} of
        {_, true} ->
            reload(Module);
        {{ok, #file_info{mtime = Mtime}}, false} when Mtime >= From, Mtime < To ->
            reload(Module);
        {{ok, _}, _} ->
            unmodified;
        {{error, enoent}, _} ->
            %% The Erlang compiler deletes existing .beam files if
            %% recompiling fails.  Maybe it's worth spitting out a
            %% warning here, but I'd want to limit it to just once.
            gone;
        {{error, Reason}, _} ->
            ?LOG_INFO("Error reading ~s's file info: ~p~n", [Filename, Reason]),
            error
    end.

reload(Module) ->
    ?LOG_INFO("[~p] is loading ...", [Module]),
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            ?LOG_INFO("[~p] has been loaded.~n", [Module]),
            case erlang:function_exported(Module, test, 0) of
                true ->
                    ?LOG_INFO(" - Calling ~p:test() ...", [Module]),
                    case catch Module:test() of
                        ok ->
                            ?LOG_INFO("[~p] test is finished.~n", [Module]),
                            reload;
                        Reason ->
                            ?LOG_INFO(" fail: ~p.~n", [Reason]),
                            reload_but_test_failed
                    end;
                false ->
                    reload
            end;
        {error, Reason} ->
            ?LOG_INFO(" fail: ~p.~n", [Reason]),
            error
    end.

stamp() ->
    erlang:localtime().
%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
