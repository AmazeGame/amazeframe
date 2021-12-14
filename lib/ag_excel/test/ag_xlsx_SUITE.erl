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
-module(ag_xlsx_SUITE).

-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
%% API
-export([all/0]).
-export([test_read_xlsx/1]).
all() ->
    [test_read_xlsx].

%%%测试表数据

-define(TableFields,["id","name","age","level","icon","show","weight"]).

%%
%%%------------------------------------

test_read_xlsx(Config) ->
    LoadDir = config(data_dir, Config) ++ "test.xlsx",
    ct:pal("data_dir:~p~n",[LoadDir]),
    RowHandler =
        fun(_SheetName, [1 | Row],Context) ->
            {next_row,Context++ [Row]};
            (_SheetName, [_Line | Row],Context) ->
                {next_row,Context++ [Row]}
        end,
    InputContext = [],
    LoadContext =
        case ag_excel:read(LoadDir,InputContext,RowHandler) of
            {error,Reason}-> ct:pal("read error:~p~n",[Reason]);
            {ok}-> ct:pal("~n");
            Context-> ct:pal("Context ~p",[Context]),Context
        end,
    [F|_T] = LoadContext,
    ?assertEqual(?TableFields,F).
