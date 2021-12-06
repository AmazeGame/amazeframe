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
-ifndef(_GEN_MEMORY_DB_HRL_).
-define(_GEN_MEMORY_DB_HRL_, 1).

-define(REDIS_POOL, ag_cluster_config:getv(redis_pool)).
-define(MONGODB_POOL, ag_cluster_config:getv(mongodb_pool)).

-record(table_config, {
    table          :: atom(),
    attribute = [] :: list(),
    index = []     :: list(),
    ttl = 0        :: non_neg_integer()
}).

-endif.
