%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.29
%%%-------------------------------------------------------------------
%%%
-module(ag_eventdispatcher_acceptor).

-type sender()  :: atom().
-type event() :: tuple()|map().

-export_type([sender/0]).
-export_type([event/0]).
%% API
-callback on_event(System :: sender(), Event :: event()) ->
    ok.
