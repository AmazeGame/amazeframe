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
-module(ag_excel).

-export([read/3]).

-spec(read(XlsxFile :: string(),InputContext::term(), RowHandler :: atom() | function()) ->
	{error, Resaon :: atom()} | {error, Resaon :: string()} |
	undefined | tuple()).
read(XlsxFile,InputContext, RowHandler)->
    ag_excel_reader:read(XlsxFile,InputContext, RowHandler).