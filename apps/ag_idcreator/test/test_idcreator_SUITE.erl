%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.11.02
%%%-------------------------------------------------------------------
%%%
-module(test_idcreator_SUITE).


%% API
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all()->
    [
        test_agicrandchar,
        test_agicintacc,
        test_agicsnowflake,
        test_agicets,
        test_newid_randchar_efficiency,
        test_newid_accumulator_efficiency,
        test_newid_sonwflake_efficiency,
        test_batchid_randchar_efficiency,
        test_batchid_accumulator_efficiency,
        test_batchid_sonwflake_efficiency
    ].

init_per_suite(_Config) ->
    logger:set_primary_config(level,all),
    AppConfig =
        [
            {id_randchar,[{gameid,1001},{bucket,1}]},
            {id_accumulator,[
                {pools,id_accumulator_redis_pool},
                {driver,redis},
                {option,
                    {
                        [{size, 20}, {max_overflow, 2}],
                        [{host, "127.0.0.1"}, {port, 6010}, {database, 7}, {password, ""}, {reconnect_sleep, 100}]
                    }
                },
                {biz,[{default,3000},{intacc1,1000},{intacc2,2000}]}
            ]} ,     %% 自增id生成器参数
            {id_snowflake,[{dc_id,random},{worker_id,random}]}      %%snowflake id生成器参数
        ],
    agb_application:apply_application_env(ag_idcreator,AppConfig),
    application:ensure_all_started(ag_database),
    timer:sleep(5000),
    application:ensure_all_started(ag_idcreator),
    _Config.

end_per_suite(_Config)->
    ok.


init_per_testcase(_, _Config) ->
    _Config.
end_per_testcase(_, _Config) ->
    %application:stop(),
    ok.


test_agicrandchar(_Config)->
    application:ensure_all_started(ag_database),
    timer:sleep(5000),
    application:ensure_all_started(ag_idcreator),
    NewId0 = ag_idcreator:gen_newid(),
    NewId1 = ag_idcreator:gen_newid(randchar),
    ct:log("---------test_agicprocess newid0:~p,newid1:~p",[NewId0,NewId1]),
    ?assertNotEqual(NewId0,NewId1),
    ag_idcreator:gen_batchid(5).

test_agicintacc(_Config)->
    NewId0 = ag_idcreator:gen_newid(intacc),
    NewId1 = ag_idcreator:gen_newid(intacc),
    NewId2 = ag_idcreator:gen_newid(intacc,intacc1),
    ct:log("---------test_agicprocess newid0:~p,newid1:~p newid2:~p",[NewId0,NewId1,NewId2]),
    BatchId0 = ag_idcreator:gen_batchid(intacc,5),
    BatchId1 = ag_idcreator:gen_batchid(intacc,intacc2,5),
    ct:log("---------test_agicprocess BatchId:~p",[{BatchId0,BatchId1}]),
    ?assertEqual(1,         NewId1-NewId0).

test_agicets(_Config)->
    NewId0 = ag_idcreator:gen_newid(),
    NewId1 = ag_idcreator:gen_newid(ets),
    ct:log("---------test_agicprocess newid0:~p,newid1:~p",[NewId0,NewId1]),
    BatchId = ag_idcreator:gen_batchid(ets,5),
    ct:log("---------test_agicprocess BatchId:~p",[BatchId]),
    ?assertNotEqual(NewId0,NewId1).

test_agicsnowflake(_Config)->
    NewId0 = ag_idcreator:gen_newid(snowflake),
    NewId1 = ag_idcreator:gen_newid(snowflake),
    ct:log("---------test_agicprocess newid0:~p,newid1:~p",[NewId0,NewId1]),
    BatchId = ag_idcreator:gen_batchid(snowflake,5),
    ct:log("---------test_agicprocess BatchId:~p",[BatchId]),
    ?assertNotEqual(NewId0,NewId1).

test_newid_randchar_efficiency(_Config)->
    test_newid_efficiency(randchar,100000).

test_newid_accumulator_efficiency(_Config)->
    test_newid_efficiency(intacc,100000).

test_newid_sonwflake_efficiency(_Config)->
    test_newid_efficiency(snowflake,100000).

test_newid_efficiency(Type,Count) ->
    R = lists:map( fun(_) ->  ag_idcreator:gen_newid(Type) end,lists:seq(0,Count) ),
    ?assertEqual(erlang:length(R),          erlang:length(lists:usort(R))).

test_batchid_randchar_efficiency(_Config)->
    test_batchid_efficiency(randchar,4000,100).

test_batchid_accumulator_efficiency(_Config)->
    test_batchid_efficiency(intacc,4000,100).

test_batchid_sonwflake_efficiency(_Config)->
    test_batchid_efficiency(snowflake,4000,100).

test_batchid_efficiency(Type,BatchNum,Count) ->
    R = lists:map( fun(_) ->  ag_idcreator:gen_batchid(Type,BatchNum) end,lists:seq(0,Count) ),
    ?assertEqual(erlang:length(R),          erlang:length(lists:usort(R))).
