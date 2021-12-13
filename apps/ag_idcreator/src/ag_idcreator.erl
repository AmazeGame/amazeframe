%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @author ayongbc
%%% @email  adrianx.lau@gmail.com adrianx@163.com 
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.12.11
%%%-------------------------------------------------------------------
%%%
-module(ag_idcreator).

-export([
    gen_newid/0, gen_newid/1, gen_newid/2,
    gen_batchid/1, gen_batchid/2, gen_batchid/3
]).


-spec gen_newid() -> term().
gen_newid() ->
    gen_newid(randchar).

-spec gen_newid(randchar|intacc|snowflake|ets) -> term().
gen_newid(randchar) ->
    ag_idcreator_randchar:gen_newid();
gen_newid(intacc) ->
    ag_idcreator_accumulator:gen_newid(default);
gen_newid(snowflake) ->
    ag_idcreator_snowflake:gen_newid();
gen_newid(ets) ->
    ag_idcreator_id_ets:gen_newid().

-spec gen_newid(intacc, Biz :: atom()) -> term().
gen_newid(intacc, Biz) ->
    ag_idcreator_accumulator:gen_newid(Biz).

-spec gen_batchid(Count :: integer()) -> [term()].
gen_batchid(Count) ->
    gen_batchid(randchar, Count).

-spec gen_batchid(randchar|intacc|snowflake|ets, Count :: integer()) -> [term()].
gen_batchid(randchar, Count) ->
    ag_idcreator_randchar:gen_batchid(Count);
gen_batchid(intacc, Count) ->
    ag_idcreator_accumulator:gen_batchid(default, Count);
gen_batchid(snowflake, Count) ->
    ag_idcreator_snowflake:gen_batchid(Count);
gen_batchid(ets, Count) ->
    ag_idcreator_id_ets:gen_batchid(Count).

-spec gen_batchid(intacc, Biz :: atom(), Count :: integer()) -> [term()].
gen_batchid(intacc, Biz, Count) ->
    ag_idcreator_accumulator:gen_batchid(Biz, Count).
