%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(load_mock_modules).


%% API
-export([compile_mock/1]).

compile_mock(Path)->
    ErlFile = filelib:wildcard(filename:absname_join(Path,"*.erl")),
    IncludeDirs = [begin {i,Dir} end||Dir<- Path],
    Fun =
        fun(FileName)->
            case compile:file(FileName,[{outdir,Path},report,verbose]++IncludeDirs) of
                {ok,T} ->
                    unload_code(T),
                    {ok,T};
                Err -> Err
            end
        end,
    [Fun(F) || F <- ErlFile],
    BeamFiles = filelib:wildcard(filename:absname_join(Path,"*.beam")),
    [code:load_abs(filename:rootname(F)) || F <- BeamFiles] .

unload_code(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod),
    code:delete(Mod),
    ok.