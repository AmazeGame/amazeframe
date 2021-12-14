-module(agb_recompiler).
-include_lib("kernel/include/file.hrl").
-include("agb_debuglogger.hrl").

-behaviour(gen_server).
-export([
    start/0,
    start_link/0,
    stop/0
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% TOOL API
-export([
    compile_app/1,
    copy_ext/6
]).

-record(state, {last :: calendar:datetime()}).
-define(SEONDS_RECOMPILE, 10).
%% External API

%% @spec start() -> ServerRet
%% @doc Start the reloader.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @spec start_link() -> ServerRet
%% @doc Start the reloader.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> ok
%% @doc Stop the reloader.
stop() ->
    gen_server:call(?MODULE, stop).

%% gen_server callbacks

%% @spec init([]) -> {ok, State :: #state{}}
%% @doc gen_server init, opens the server in an initial state.
init([]) ->
    erlang:send_after(timer:seconds(1), self(), doit), %% run at once ,first time!
    {ok, #state{last = stamp()}}.

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
    _ = doit(State#state.last, Now),
    erlang:send_after(timer:seconds(?SEONDS_RECOMPILE), self(), doit),
    {noreply, State#state{last = Now}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% @doc gen_server termination callback.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(_OldVsn, State, _Extra) -> State
%% @doc gen_server code_change callback (trivial).
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

doit(_From, _To) ->
    case make:all() of
        up_to_date ->
            ?LOG_INFO("[recompile] Check finish @ ~p ~n", [stamp()]);
        error ->
            ?LOG_INFO("[recompile] Check finish @ ~p ~n", [stamp()])
    end.

stamp() ->
    erlang:localtime().

compile_app(Args) ->
    case Args of
        [InputDir, OutDir] ->
            compile_app(InputDir, OutDir);
        _ ->
            ?LOG_INFO("Error input for compile_app")
    end.

compile_app(InputDir, OutDir) ->
    NewInput =
        if
            is_atom(InputDir) ->
                atom_to_list(InputDir);
            true ->
                InputDir
        end,
    NewOut =
        if
            is_atom(OutDir) ->
                atom_to_list(OutDir);
            true ->
                OutDir
        end,
    copy_ext(NewInput, NewOut,
        ".app.src", ".app",
        "Copy success app:~p~n",
        "Copy failed app:~p~n").

get_filename(Path) ->
    case string:rchr(Path, $/) of
        0 ->
            Path;
        I ->
            string:sub_string(Path, I + 1)
    end.

convert_ext(File, SrcExt, NewExt) ->
    case string:rstr(File, SrcExt) of
        0 ->
            File ++ NewExt;
        I ->
            string:sub_string(File, 1, I - 1) ++ NewExt
    end.

convert_dest_path(SrcFile, OutDir, SrcExt, NewExt) ->
    FileName = get_filename(SrcFile),
    case lists:last(OutDir) of
        $/ ->
            OutDir ++ convert_ext(FileName, SrcExt, NewExt);
        _ ->
            OutDir ++ "/" ++ convert_ext(FileName, SrcExt, NewExt)
    end.

get_file_md5(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
            erlang:md5(Binary);
        _ ->
            <<>>
    end.

copy_ext(InputDir, OutDir, SrcExt, NewExt, OKPrompt, FailPrompt) ->
    CopyOp =
        fun(SrcFile) ->
            case string:right(SrcFile, string:len(SrcExt)) of
                SrcExt ->
                    DstFile = convert_dest_path(SrcFile, OutDir, SrcExt, NewExt),
                    SrcMd5 = get_file_md5(SrcFile),
                    DstMd5 = get_file_md5(DstFile),
                    do_compare_and_cp_file(SrcMd5, DstMd5, SrcFile, DstFile, OKPrompt, FailPrompt);
                _ ->
                    nothing
            end
        end,
    ExtFilter = ".*" ++ SrcExt,
    filelib:fold_files(InputDir,
        ExtFilter,
        true,
        fun(F, _Acc) ->
            CopyOp(F)
        end,
        []
    ).

do_compare_and_cp_file(SrcMd5, DstMd5, SrcFile, DstFile, OKPrompt, FailPrompt) ->
    if
        SrcMd5 =:= DstMd5 ->
            ignore;
        true ->
            case file:copy(SrcFile, DstFile) of
                {ok, _} ->
                    case OKPrompt of
                        [] ->
                            ignore;
                        _ ->
                            ?LOG_INFO(OKPrompt, [DstFile])
                    end;
                _ ->
                    case FailPrompt of
                        [] ->
                            ignore;
                        _ ->
                            ?LOG_INFO(FailPrompt, [DstFile])
                    end
            end
    end.
