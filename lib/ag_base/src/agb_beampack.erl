%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------
-module(agb_beampack).

%% API
-export([
    pack/2,
    ensure/1
]).

-define(STATIC_FILE_NAME,"static.gz").
-define(SHENBANG_COMMENT,"").


-spec pack(EScript :: string(), Beams :: list()) ->
    boolean().

pack(EScript, Beams) ->
    BeamFileList =
        lists:map(
            fun(Beam) ->
                ensure(Beam)
            end,
            Beams
        ),
    case agb_file:check_files_time(BeamFileList, EScript) of
        time_out ->
            gen_beam_bundle(EScript, BeamFileList), true;
        _ ->
            false
    end.
-spec ensure(Module :: atom()) ->
    [] | string().
ensure(Module) ->
    case code:which(Module) of
        non_existing ->
            [];
        File ->
            File
    end.

gen_beam_bundle(EScript, BeamLists) ->
    ZipFile = ?STATIC_FILE_NAME,
    Read =
        fun(F) ->
            {ok, B} = file:read_file(filename:absname(F)), B
        end,
    Squash =
        fun(L) ->
            [{filename:basename(F), Read(F)} || F <- L]
        end,
    Zip =
        fun(Beams) ->
            {ok, {_, Z}} = zip:create(ZipFile, Squash(Beams), [{compress, all}, memory]), Z
        end,
    {ok, Binary} = escript:create(binary, [shebang, {comment, ?SHENBANG_COMMENT}, {archive, Zip(BeamLists)}]),
    file:write_file(EScript, Binary),
    file:change_mode(EScript, 8#777).
