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

-define(NODE_INFO_TABLE, '#node_info_table#').
-define(NODE_STATE_CHANGE_EVENT, node_state_change_event).

-record(ag_node_info, {
    cluster :: atom()|string(),
    master_server = undefined :: term() | undefined,
    members = undefined :: [term()] | undefined
}).