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
-module(ag_bizlogger_process_adapter).

-callback write(atom(), binary()|iolist()) ->    ok.

-callback make_process_name(Biz::any(),Index::integer()) ->    atom().
