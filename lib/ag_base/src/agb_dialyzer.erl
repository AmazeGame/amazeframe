%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------

-module(agb_dialyzer).


-export([
    build_plt/1,
    run/2
]).
-spec build_plt(OutPlt :: string()) ->
    [dialyzer:dial_warning()].
build_plt(OutPlt) ->
    LibDirs = filelib:wildcard(filename:absname_join(code:root_dir(), "lib/*/ebin")),
    IncludeDirs = filelib:wildcard(filename:absname_join(code:root_dir(), "lib/*/include")),
    Option = [
        {output_plt, OutPlt},
        {analysis_type, plt_build},
        {files_rec, LibDirs},
        {include_dirs, IncludeDirs}
    ],
    dialyzer:run(Option).

-spec run(InputPlt :: string(), OutLog :: string()) ->
    any().
run(InputPlt, OutLog) ->
    LibDirs = filelib:wildcard(filename:absname_join(code:root_dir(), "lib/*/ebin")),
    DepDirs = filelib:wildcard("deps/*/ebin"),
    IncludeDirs = filelib:wildcard(filename:absname_join(code:root_dir(), "lib/*/include")),
    Option = [
        {init_plt, InputPlt},
        {analysis_type, plt_check},
        {files_rec, ["ebin" | LibDirs ++ DepDirs]},
        {include_dirs, IncludeDirs},
        {get_warnings, true},
        {output_file, OutLog}
    ],
    dialyzer:run(Option).
