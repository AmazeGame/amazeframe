%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------
-module(agb_codename).

-export([main/1]).

-spec main(Args :: [string()]) ->
    any().
main(Args) ->
    run(Args).

run(Args) ->
    PropList = parse_args(Args, []),
    Output = get_output_from_prop_list(PropList),
    InputFiles = get_input_from_prop_list(PropList),
    EnableSrc = get_enable_src_from_prop_list(PropList),
    Clean = get_clean_from_prop_list(PropList),
    Compile = get_compile_from_prop_list(PropList),
    if
        InputFiles == [] ->
            io:format("[code name tool] need input file~n");
        Output == [] ->
            io:format("[code name tool] need output file~n");
        true ->
            if
                Clean ->
                    clean(InputFiles, Output, EnableSrc),
                    if
                        Compile ->
                            compile(InputFiles, Output, EnableSrc);
                        true ->
                            ignore
                    end;
                true ->
                    compile(InputFiles, Output, EnableSrc)
            end
    end.

get_output_from_prop_list(PropList) ->
    case lists:keyfind(output, 1, PropList) of
        false ->
            "";
        {output, OutFile} ->
            OutFile
    end.

get_input_from_prop_list(PropList) ->
    Rs =
        lists:filter(
            fun
                ({input, _}) ->
                    true;
                (_) ->
                    false
            end,
            PropList
        ),
    case Rs of
        [] ->
            [];
        Inputs ->
            lists:flatmap(
                fun({_, Input}) ->
                    filelib:wildcard(Input)
                end,
                Inputs
            )
    end.

get_enable_src_from_prop_list(PropList) ->
    case lists:keyfind(noerl, 1, PropList) of
        {noerl, true} ->
            false;
        _ ->
            true
    end.

get_clean_from_prop_list(PropList) ->
    case lists:keyfind(clean, 1, PropList) of
        {clean, true} ->
            true;
        _ ->
            false
    end.

get_compile_from_prop_list(PropList) ->
    case lists:keyfind(compile, 1, PropList) of
        {compile, true} ->
            true;
        _ ->
            false
    end.

parse_args([], Properties) ->
    Properties;
parse_args(["--output", File | Args], Properties) ->
    parse_args(Args, [{output, File} | Properties]);
parse_args(["--input" | Args], Properties) ->
    {Left, Files} = parse_filename(Args, []),
    parse_args(Left, [{input, Files} | Properties]);
parse_args(["--noerl" | Args], Properties) ->
    parse_args(Args, [{noerl, true} | Properties]);
parse_args(["--compile" | Args], Properties) ->
    parse_args(Args, [{compile, true} | Properties]);
parse_args(["--clean" | Args], Properties) ->
    parse_args(Args, [{clean, true} | Properties]).

parse_filename([[$- | _] = A | Args], Inputs) ->
    {[A | Args], lists:reverse(Inputs)};
parse_filename([File | Args], Inputs) ->
    parse_filename(Args, [File | Inputs]).

clean(_Hrls, OutFile, _EnableSrc) ->
    Ext = filename:extension(OutFile),
    BaseFile = filename:basename(OutFile, Ext),
    OutDir = filename:dirname(OutFile),

    OutBeam = filename:absname_join(OutDir, BaseFile ++ ".beam"),
    OutSrc = filename:absname_join(OutDir, BaseFile ++ ".erl"),
    case file:read_file_info(OutBeam) of
        {ok, _} ->
            file:delete(OutBeam),
            io:format("clean:~s~n", [OutBeam]);
        _ ->
            ok
    end,
    case file:read_file_info(OutSrc) of
        {ok, _} ->
            file:delete(OutSrc),
            io:format("clean:~s~n", [OutSrc]);
        _ ->
            ok
    end.

compile(Hrls, OutFile, EnableSrc) ->
    case agb_file:check_files_time(Hrls, OutFile) of
        time_out ->
            Ext = filename:extension(OutFile),
            BaseFile = filename:basename(OutFile, Ext),
            OutDir = filename:dirname(OutFile),
            MacNames = files_defines(Hrls),
            CodeMap = string:join(
                lists:map(
                    fun({Code, CodeName}) ->
                        ToWrite = {Code, atom_to_binary(CodeName, utf8)},
                        sprintf("\t\t~p", [ToWrite])
                    end,
                    MacNames
                ),
                ",\n"
            ),
            CodeString = "-module(" ++ BaseFile ++
                ").-behaviour(agb_message_wrap_code).-export([get_infos/0]).get_infos()->[\n"
                ++ CodeMap ++ "\n\t].",
            {_Module, Binary} = agb_dynamic_compile:from_string(CodeString, [debug_info]),
            if
                EnableSrc ->
                    agb_file:write_file(OutDir, BaseFile ++ ".erl", CodeString);
                true ->
                    ignore
            end,
            agb_file:write_file(OutDir, BaseFile ++ ".beam", Binary),
            io:format("generated:~s~n", [BaseFile ++ ".beam"]);
        _ ->
            ok
    end.

sprintf(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

files_defines([]) ->
    [];
files_defines([Hrl | Hrls]) ->
    file_defines(Hrl) ++ files_defines(Hrls).

file_defines(Hrl) ->
    Content =
        case file:read_file(Hrl) of
            {ok, Binary} ->
                binary_to_list(Binary);
            {error, _} ->
                []
        end,
    {ok, Tokens, _lines} = erl_scan:string(Content),
    proc_define(Tokens).

proc_define([]) ->
    [];
proc_define([{'-', _}, {atom, _, define}, {'(', _}, {_, _, Name} | Tokens]) ->
    case Tokens of
        [{',', _}, {integer, _, Number} | LeftTokens] ->
            [{Number, Name} | proc_define(LeftTokens)];
        [{',', _}, {'-', _}, {integer, _, Number} | LeftTokens] ->
            [{-Number, Name} | proc_define(LeftTokens)];
        [{',', _}, {atom, _, _} | LeftTokens] ->
            proc_define(skip_token(LeftTokens));
        [{',', _}, {_, _, _} | LeftTokens] ->
            proc_define(skip_token(LeftTokens));
        [{'(', _} | LeftTokens] ->
            proc_define(skip_token(LeftTokens));
        LeftTokens ->
            proc_define(skip_token(LeftTokens))
    end;
proc_define([_H | Tokens]) ->
    proc_define(skip_token(Tokens)).

skip_token([]) ->
    [];
skip_token([{dot, _} | Tokens]) ->
    Tokens;
skip_token([_H | Tokens]) ->
    skip_token(Tokens).
