%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------

-module(agb_file).

-include_lib("kernel/include/file.hrl").
-include("agb_debuglogger.hrl").

-export([
    append_to_file/2,
    append_term_to_file/2,
    write_file/3,
    write_term_to_file/2,
    get_file_all_line/1,
    ensure_dir/1,
    get_dir_mtime/1,
    trim_mid_filename/1,
    zip_mid_filename/2,
    get_mid_name/1,
    dir_today_modify/1,

    wild_dir/2,
    get_file_md5_string/1,
    get_file_md5/1,
    check_files_time/2,
    check_file_time/2,
    copy_file_tree/3,
    remove_dir_tree/1,
    clear_cr_wild/1,
    calc_abs_filename/1
]).

%%
%% Local Functions
%%
-spec append_to_file(File :: string(), AppendAllLines :: iodata()) ->
    ok | {error, Reason} when
    Reason :: file:posix() | badarg | system_limit.
append_to_file(File, AppendAllLines) ->
    case file:open(File, [append]) of
        {ok, F} ->
            file:write(F, AppendAllLines),
            file:close(F),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_file_all_line(FileName :: string()) ->
    string() | binary() | {error, Reason} when
    Reason :: file:posix()
    | badarg
    | terminated
    | {no_translation, unicode, latin1}.
get_file_all_line(FileName) ->
    case file:open(FileName, [read]) of
        {ok, F} ->
            read_file_loop(F);
        {error, Reason} ->
            {error, Reason}
    end.

read_file_loop(F) ->
    case file:read_line(F) of
        {ok, Data} ->
            [Data | read_file_loop(F)];
        eof ->
            file:close(F),
            [];
        _Error ->
            file:close(F),
            []
    end.

sure_dir_str(Dir) ->
    case lists:last(Dir) of
        $/ ->
            Dir;
        _ ->
            Dir ++ "/"
    end.

-spec ensure_dir(Dir) ->
    'ok' | {'error', Reason} when
    Dir :: file:filename_all() | file:dirname_all(),
    Reason :: file:posix().
ensure_dir(Dir) ->
    filelib:ensure_dir(sure_dir_str(Dir)).

-spec trim_mid_filename(FileName :: string()) ->
    string().
trim_mid_filename(FileName) ->
    I = string:rchr(FileName, $.),
    if
        I < 2 ->
            FileName;
        true ->
            N1 = string:substr(FileName, 1, I - 1),
            Ex1 = string:substr(FileName, I),
            I1 = string:rchr(N1, $.),
            if
                I1 < 2 ->
                    FileName;
                true ->
                    N2 = string:substr(N1, 1, I1 - 1),
                    N2 ++ Ex1
            end
    end.

-spec zip_mid_filename(FileName :: string(), Mid :: string()) ->
    string().
zip_mid_filename(FileName, Mid) ->
    I = string:rchr(FileName, $.),
    if
        I < 1 ->
            FileName ++ "." ++ Mid;
        true ->
            N = string:substr(FileName, 1, I),
            E = string:substr(FileName, I),
            N ++ Mid ++ E
    end.

-spec get_mid_name(FileName :: string()) ->
    string() | [].
get_mid_name(FileName) ->
    I = string:rchr(FileName, $.),
    if
        I < 2 ->
            [];
        true ->
            N1 = string:substr(FileName, 1, I - 1),
            I1 = string:rchr(N1, $.),
            if
                I1 < 1 ->
                    [];
                true ->
                    string:substr(N1, I1 + 1)
            end
    end.

-spec get_dir_mtime(Dir :: file:name_all()) ->
    string().
get_dir_mtime(Dir) ->
    {ok, FileInfo} = file:read_file_info(Dir),
    agb_string:datetime_to_string(FileInfo#file_info.mtime).

-spec dir_today_modify(Dir :: file:name_all()) ->
    boolean().
dir_today_modify(Dir) ->
    {ok, FileInfo} = file:read_file_info(Dir),
    {Date, _} = FileInfo#file_info.mtime,
    Date =:= erlang:date().

-spec write_file(Dir, File, String) ->
    ok | {error, Reason} when
    Dir :: file:name_all(),
    File :: file:name_all(),
    String :: iodata(),
    Reason :: file:posix() | badarg | terminated | system_limit.
write_file(Dir, File, String) ->
    OutFile = filename:join(Dir, File),
    case file:write_file(OutFile, String) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            ok
    end.

-spec get_file_md5(File :: file:name_all()) ->
    binary().
get_file_md5(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
            erlang:md5(Binary);
        _ ->
            <<>>
    end.

-spec get_file_md5_string(File :: file:name_all()) ->
    string().
get_file_md5_string(File) ->
    agb_hex:bin_to_hexlstr(get_file_md5(File)).

-spec check_file_time(Input :: string(), OutFile :: string()) ->
    time_out | newest | same_time.
check_file_time(Input, OutFile) ->
    case file:read_file_info(OutFile, [{time, posix}]) of
        {ok, #file_info{mtime = OutMt}} ->
            case file:read_file_info(Input, [{time, posix}]) of
                {ok, #file_info{mtime = Mt}} ->
                    if
                        Mt > OutMt ->
                            time_out;
                        Mt =:= OutMt ->
                            same_time;
                        true ->
                            newest
                    end;
                {error, _Reason} ->
                    newest
            end;
        {error, _} ->
            time_out
    end.
-spec check_files_time(InputFiles :: [string()], OutFile :: string()) ->
    time_out | newest | same_time.
check_files_time(InputFiles, OutFile) ->
    case file:read_file_info(OutFile, [{time, posix}]) of
        {ok, #file_info{mtime = OutMt}} ->
            lists:foldl(
                fun
                    (_File, time_out) ->
                        time_out;
                    (File, newest) ->
                        read_file_for_newest(File, OutMt);
                    (File, same_time) ->
                        read_file_for_same_time(File, OutMt)
                end,
                same_time,
                InputFiles
            );
        {error, _} ->
            time_out
    end.

read_file_for_newest(File, OutMt) ->
    case file:read_file_info(File, [{time, posix}]) of
        {ok, #file_info{mtime = Mt}} ->
            if Mt > OutMt ->
                time_out;
                true ->
                    newest
            end;
        {error, Reason} ->
            Reason2 = agb_string:sprintf("read_file_info(~s)-> ~p", [File, Reason]),
            error(Reason2)
    end.

read_file_for_same_time(File, OutMt) ->
    case file:read_file_info(File, [{time, posix}]) of
        {ok, #file_info{mtime = Mt}} ->
            if Mt > OutMt ->
                time_out;
                Mt =:= OutMt ->
                    same_time;
                true ->
                    newest
            end;
        {error, Reason} ->
            Reason2 = agb_string:sprintf("read_file_info(~s)-> ~p", [File, Reason]),
            error(Reason2)
    end.

-spec append_term_to_file(File :: file:name_all(), Term :: term()) ->
    no_return().
append_term_to_file(File, Term) ->
    case file:open(File, [append]) of
        {ok, F} ->
            io:fwrite(F, "~p.~n", [Term]),
            file:close(F);
        {error, _} ->
            ?LOG_INFO("open file error~n")
    end.

-spec write_term_to_file(File :: file:name_all(), Term :: term()) ->
    no_return().
write_term_to_file(File, Term) ->
    case file:open(File, [write]) of
        {ok, F} ->
            io:fwrite(F, "~p.~n", [Term]),
            file:close(F);
        {error, _} ->
            ?LOG_INFO("open file error~n")
    end.

-spec copy_file_tree(InWildcard :: string(), OutDir :: string(), CallBack :: term()) ->
    nothing | ok.
copy_file_tree(InWildcard, OutDir, CallBack) ->
    case filelib:wildcard(InWildcard) of
        [] ->
            nothing;
        Files ->
            lists:foreach(
                fun(F) ->
                    D = filename:dirname(F),
                    AbsOutDir = filename:absname_join(OutDir, D),
                    agb_file:ensure_dir(AbsOutDir),
                    AbsFile = filename:absname_join(OutDir, F),
                    file:copy(F, AbsFile),
                    case is_function(CallBack) of
                        true ->
                            CallBack(F, AbsFile);
                        false ->
                            ignore
                    end
                end,
                Files
            )
    end.

-spec remove_dir_tree(Dir :: string()) ->
    ok.
remove_dir_tree(Dir) ->
    remove_all_files(".", [Dir]).

remove_all_files(Dir, Files) ->
    lists:foreach(
        fun(File) ->
            FilePath = filename:join([Dir, File]),
            {ok, FileInfo} = file:read_file_info(FilePath),
            case FileInfo#file_info.type of
                directory ->
                    {ok, DirFiles} = file:list_dir(FilePath),
                    remove_all_files(FilePath, DirFiles),
                    file:del_dir(FilePath);
                _ ->
                    file:delete(FilePath)
            end
        end, Files).

clear_cr(File) ->
    {ok, Fd} = file:open(File, [read]),
    NewContent = do_clear_cr2(Fd),
    file:close(Fd),
    file:write_file(File, NewContent).

do_clear_cr2(Fd) ->
    case file:read_line(Fd) of
        {ok, Result} ->
            Result ++ do_clear_cr2(Fd);
        _ ->
            []
    end.

-spec clear_cr_wild(Wild :: file:filename() | file:dirname()) ->
    ok.
clear_cr_wild(Wild) ->
    Files = filelib:wildcard(Wild),
    lists:foreach(fun(File) ->
        clear_cr(File) end, Files).

%%%-------------------------------------------------------------------
%%%
%%%   Scan Utilities
%%%
%%%-------------------------------------------------------------------
%%

list_dir(Dir, Base) ->
    BaseDir = sure_dir_str(Base),
    {ok, Files} = file:list_dir(Dir),
    lists:map(
        fun(F) ->
            File = filename:absname_join(Dir, F),
            IsDir = filelib:is_dir(File),
            if IsDir ->
                T = sure_dir_str(File),
                {T, T--BaseDir};
                true ->
                    {File, File -- BaseDir}
            end
        end, Files).


-spec wild_dir(Dir :: file:name_all(), Ignors :: [string()]) ->
    [string()].
wild_dir(Dir, Ignors) ->
    do_wild_dir(Dir, Ignors, list_dir(Dir, Dir)).

do_wild_dir(_BaseDir, _Ignors, []) ->
    [];
do_wild_dir(BaseDir, Ignors, [{File, Relative} | Files]) ->
    case is_ignore(Ignors, Relative) of
        true ->
            do_wild_dir(BaseDir, Ignors, Files);
        false ->
            case filelib:is_dir(File) of
                true ->
                    SubDirs = list_dir(File, BaseDir),
                    do_wild_dir(BaseDir, Ignors, SubDirs) ++ do_wild_dir(BaseDir, Ignors, Files);
                false ->
                    [File | do_wild_dir(BaseDir, Ignors, Files)]
            end
    end.

do_is_ignore(Ignor, File) ->
    BaseName = filename:basename(File),
    case Ignor of
        "*." ++ ExtName ->
            ExtLen = string:len(ExtName),
            ExtName == string:right(BaseName, ExtLen);
        _ ->
            case string:tokens(Ignor, "*") of
                [_Nochange] ->
                    string:str(File, Ignor) == 1;
                [Prefix, Left | _] ->
                    case string:str(File, Prefix) == 1 of
                        true ->
                            filename:extension(File) == Left;
                        false ->
                            false
                    end
            end
    end.

is_ignore(Ignors, File) ->
    lists:foldl(
        fun
            (_Ignor, true) ->
                true;
            (Ignor, false) ->
                do_is_ignore(Ignor, File)
        end, false, Ignors).

-spec calc_abs_filename(Path :: file:name_all()) ->
    file:name_all().
calc_abs_filename(Path) ->
    Names = filename:split(Path),
    DroppedNames = lists:foldl(
        fun
            (I, []) ->
                [I];
            ("..", [_L | T]) ->
                T;
            (".", R) ->
                R;
            (I, R) ->
                [I | R]
        end, [], Names),
    filename:join(lists:reverse(DroppedNames)).
