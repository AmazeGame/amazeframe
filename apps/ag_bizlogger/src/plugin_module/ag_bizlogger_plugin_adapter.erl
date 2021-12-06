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
-module(ag_bizlogger_plugin_adapter).


-callback header(Args::term()) -> map().

-callback do_combine_header_opt(Header :: maps:map(), Log :: maps:map()) -> maps:map().