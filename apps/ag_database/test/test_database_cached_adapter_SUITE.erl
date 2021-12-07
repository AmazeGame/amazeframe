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
-module(test_database_cached_adapter_SUITE).


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(ct_helper, [config/2]).
%% API
-compile(export_all).

all() ->
    [{group, mysql_redis_pool}, {group, mongodb_redis_pool}].

groups() ->
    TestList = [
        test_insert_one,
        test_find_none,
        test_find_keys,
        test_update_one,
        test_insert_bagobj,
        test_update_one_bagobj,
        test_find_bagobj,
        test_delete_one,
        test_delete_keys,
        test_remove_one_bagobj,
        test_remove_multi_bagobj,
        test_clean_bagobj,
        test_set_cached_obj_expire_time,
        test_set_cached_bagobj_expire_time
    ],
    [
        {mysql_redis_pool, [sequence], TestList},
        {mongodb_redis_pool, [sequence], TestList}
    ].

init_per_suite(Config) ->
    logger:set_primary_config(level, all),
    application:ensure_all_started(ag_database),
    timer:sleep(2000),
    Config.

end_per_suite(_Config) ->
    application:stop(ag_database),
    ok.

init_per_group(mysql_redis_pool, Config) ->
    ct:pal("---------init_per_group xlsx_reader"),
    PoolName = mysql_pool,
    CreateDatabase = <<"CREATE DATABASE IF NOT EXISTS test ">>,
    mysql_poolboy:query(PoolName, CreateDatabase, 500),
    mysql_poolboy:query(PoolName, <<"use test">>, 500),

    Sql_Drop_Test_Table = <<"drop table test">>,
    mysql_poolboy:query(mysql_pool, Sql_Drop_Test_Table, 500),
    Sql_Drop_Test_Cards_Table = <<"drop table test_cards">>,
    mysql_poolboy:query(mysql_pool, Sql_Drop_Test_Cards_Table, 500),
    agdb_cached_adapter_redis:flushdb(redis_pool),

    Create_Test_Cards_Table_Sql = <<"CREATE TABLE IF NOT EXISTS `test_Cards`
        (
              `cardname` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci  NOT NULL,
              `name` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL,
              `count` int(11) NULL DEFAULT NULL,
              PRIMARY KEY (`name`,`cardname`) USING BTREE
        ) ENGINE = InnoDB CHARACTER SET = utf8 COLLATE = utf8_general_ci ROW_FORMAT = Dynamic">>,
    ct:pal("Create_Test_Cards_Table_Sql ~p~n", [Create_Test_Cards_Table_Sql]),
    mysql_poolboy:query(PoolName, Create_Test_Cards_Table_Sql, 500),

    Create_Test_Table_Sql = <<"CREATE TABLE IF NOT EXISTS `test`
        (
              `name` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL,
              `home` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL,
              `league` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL,
              PRIMARY KEY (`name`) USING BTREE
        ) ENGINE = InnoDB CHARACTER SET = utf8 COLLATE = utf8_general_ci ROW_FORMAT = Dynamic">>,
    ct:pal("Create_Test_Cards_Table_Sql ~p~n", [Create_Test_Table_Sql]),
    mysql_poolboy:query(PoolName, Create_Test_Table_Sql, 500),
    [{database_cached_pool, mysql_redis_pool} | Config];
init_per_group(mongodb_redis_pool, Config) ->
    ct:pal("---------init_per_group mongodb_redis_pool"),
    [{database_cached_pool, mongodb_redis_pool} | Config];
init_per_group(_, _) ->
    ok.

end_per_group(mysql_redis_pool, _) ->
    Sql_Drop_Test_Table = <<"drop table test">>,
    mysql_poolboy:query(mysql_pool, Sql_Drop_Test_Table, 500),
    Sql_Drop_Test_Cards_Table = <<"drop table test_Cards">>,
    mysql_poolboy:query(mysql_pool, Sql_Drop_Test_Cards_Table, 500),
    agdb_cached_adapter_redis:flushdb(redis_pool),
    ok;
end_per_group(mongodb_redis_pool, _) ->
    agdb_database_adapter_mongodb:delete(mongodb_pool, test, #{}),
    agdb_cached_adapter_redis:flushdb(redis_pool),
    ok;
end_per_group(_, _) ->
    ok.

test_insert_one(Config) ->
    ct:log("---------test_insert_one Config:~p", [Config]),
    PoolName = proplists:get_value(database_cached_pool, Config),
    ?assertMatch(
        ok,
        agdb_database_cached_adapter:insert_one(PoolName, test, <<"name">>, <<"Yankees">>,
            #{<<"name">> => <<"Yankees">>,
                <<"home">> => <<"New York">>,
                <<"league">> => <<"American">>}
        )),
    ?assertMatch(
        {error, _},
        agdb_database_cached_adapter:insert_one(PoolName, test, <<"name">>, <<"Yankees">>,
            #{<<"name">> => <<"Yankees">>,
                <<"home">> => <<"New York">>,
                <<"league">> => <<"American">>}
        )).

test_find_none(Config) ->
    PoolName = proplists:get_value(database_cached_pool, Config),
    FindOneResult0 = agdb_database_cached_adapter:find_one(PoolName, test, <<"name">>, <<"Yankees">>),
    ct:pal("-------------test_find_none FindOneResult0:~p", [FindOneResult0]),
    ?assertEqual(#{<<"home">> => <<"New York">>, <<"league">> => <<"American">>, <<"name">> => <<"Yankees">>}, FindOneResult0),

    FindOneResult1 = agdb_database_cached_adapter:find_one(PoolName, test, <<"name">>, <<"Yankees">>, [<<"name">>]),
    ct:pal("-------------test_find_none FindOneResult0:~p", [FindOneResult1]),
    ?assertEqual(#{<<"name">> => <<"Yankees">>}, FindOneResult1),

    agdb_cached_adapter_redis:del(redis_pool, make_key(test, <<"Yankees">>)),
    ?assertEqual(FindOneResult0, agdb_database_cached_adapter:find_one(PoolName, test, <<"name">>, <<"Yankees">>)),
    agdb_cached_adapter_redis:del(redis_pool, make_key(test, <<"Yankees">>)),
    ?assertEqual(FindOneResult1, agdb_database_cached_adapter:find_one(PoolName, test, <<"name">>, <<"Yankees">>, [<<"name">>])),
    ?assertEqual(undefined, agdb_database_cached_adapter:find_one(PoolName, test, <<"name">>, <<"notexist">>)).

test_find_keys(Config) ->
    PoolName = proplists:get_value(database_cached_pool, Config),
    agdb_database_cached_adapter:insert_one(PoolName, test, <<"name">>, <<"Mets">>,
        #{<<"name">> => <<"Mets">>, <<"home">> => <<"New York">>, <<"league">> => <<"National">>}),
    ?assertEqual(
        [
            #{<<"home">> => <<"New York">>, <<"league">> => <<"American">>, <<"name">> => <<"Yankees">>},
            #{<<"home">> => <<"New York">>, <<"league">> => <<"National">>, <<"name">> => <<"Mets">>}
        ],
        agdb_database_cached_adapter:find_keys(PoolName, test, <<"name">>, [<<"Yankees">>, <<"Mets">>])
    ),
    ?assertEqual(
        [],
        agdb_database_cached_adapter:find_keys(PoolName, test, <<"name">>, [<<"notexist1">>, <<"notexist2">>])
    ),
    ?assertEqual(
        [
            #{<<"home">> => <<"New York">>, <<"league">> => <<"American">>, <<"name">> => <<"Yankees">>}
        ],
        agdb_database_cached_adapter:find_keys(PoolName, test, <<"name">>, [<<"Yankees">>, <<"notexist">>])
    ).

test_update_one(Config) ->
    PoolName = proplists:get_value(database_cached_pool, Config),
    ?assertEqual(
        ok,
        agdb_database_cached_adapter:update_one(PoolName, test, <<"name">>, <<"Mets">>, #{<<"home">> => <<"BJ">>, <<"league">> => <<"BJ">>})).

test_insert_bagobj(Config) ->
    PoolName = proplists:get_value(database_cached_pool, Config),
    ?assertEqual(
        ok,
        agdb_database_cached_adapter:insert_bagobj(
            PoolName, test, <<"Cards">>,
            #{
                owner_key => <<"name">>,
                owner_k_value => <<"Yankees">>,
                element_key => <<"cardname">>,
                element_k_value => <<"dianying">>
            },
            #{<<"cardname">> => <<"dianying">>, <<"count">> => 10})),
    ?assertEqual(
        ok,
        agdb_database_cached_adapter:insert_bagobj_and_set_cachedTime(
            PoolName, test, <<"Cards">>,
            #{
                owner_key => <<"name">>,
                owner_k_value => <<"Yankees">>,
                element_key => <<"cardname">>,
                element_k_value => <<"wumei">>
            },
            #{<<"cardname">> => <<"wumei">>, <<"count">> => 100}, 86400)).

test_update_one_bagobj(Config) ->
    PoolName = proplists:get_value(database_cached_pool, Config),
    ?assertEqual(
        ok,
        agdb_database_cached_adapter:update_one_bagobj(
            PoolName, test, <<"Cards">>,
            #{
                owner_key => <<"name">>,
                owner_k_value => <<"Yankees">>,
                element_key => <<"cardname">>,
                element_k_value => <<"dianying">>
            },
            #{<<"cardname">> => <<"dianying">>, <<"count">> => 1})),
    ?assertEqual(
        ok,
        agdb_database_cached_adapter:update_one_bagobj(
            PoolName, test, <<"Cards">>,
            #{
                owner_key => <<"name">>,
                owner_k_value => <<"Yankees">>,
                element_key => <<"cardname">>,
                element_k_value => <<"wumei">>
            },
            #{<<"cardname">> => <<"wumei">>, <<"count">> => 10})).

test_find_bagobj(Config) ->
    PoolName = proplists:get_value(database_cached_pool, Config),
    BagObjSel = #{
        owner_key => <<"name">>,
        owner_k_value => <<"Yankees">>,
        element_key => <<"cardname">>
    },
    Result = agdb_database_cached_adapter:find_bagobj(PoolName, test, <<"Cards">>, BagObjSel),
    ?assertEqual(
        ordsets:from_list(
            [
                #{<<"cardname">> => <<"dianying">>, <<"count">> => 1},
                #{<<"cardname">> => <<"wumei">>, <<"count">> => 10}
            ]
        ),
        ordsets:from_list(Result)
    ),

    BagKey = make_bag_key(test, <<"Cards">>, <<"Yankees">>),
    agdb_cached_adapter_redis:del(redis_pool, BagKey),
    Result1 =
        agdb_database_cached_adapter:find_bagobj(
            PoolName, test, <<"Cards">>,BagObjSel),
    ?assertEqual(
        ordsets:from_list(
            [
                #{<<"cardname">> => <<"dianying">>, <<"count">> => 1},
                #{<<"cardname">> => <<"wumei">>, <<"count">> => 10}
            ]
        ),
        ordsets:from_list(Result1)
    ),

    agdb_cached_adapter_redis:del(redis_pool, BagKey),
    ?assertError(
        _,
        agdb_database_cached_adapter:find_bagobj(
            PoolName, test, <<"Cards">>,
            #{
                owner_key => <<"name">>,
                owner_k_value => <<"Yankees">>,
                element_key => <<"notexist">>
            })
    ),
    agdb_cached_adapter_redis:del(redis_pool, BagKey),
    ?assertEqual(
        [],
        agdb_database_cached_adapter:find_bagobj(
            PoolName, test, <<"Cards">>,
            #{
                owner_key => <<"name">>,
                owner_k_value => <<"notexist">>,
                element_key => <<"cardname">>
            })
    ).

test_delete_one(Config) ->
    PoolName = proplists:get_value(database_cached_pool, Config),
    ?assertEqual(ok, agdb_database_cached_adapter:delete_one(PoolName, test, <<"name">>, <<"Mets">>)).

test_delete_keys(Config) ->
    PoolName = proplists:get_value(database_cached_pool, Config),
    agdb_database_cached_adapter:insert_one(PoolName, test, <<"name">>, <<"Mets">>,
        #{<<"name">> => <<"Mets0">>, <<"home">> => <<"New York">>, <<"league">> => <<"National">>}),
    agdb_database_cached_adapter:insert_one(PoolName, test, <<"name">>, <<"Mets">>,
        #{<<"name">> => <<"Mets1">>, <<"home">> => <<"New York">>, <<"league">> => <<"National">>}),
    agdb_database_cached_adapter:insert_one(PoolName, test, <<"name">>, <<"Mets">>,
        #{<<"name">> => <<"Mets2">>, <<"home">> => <<"New York">>, <<"league">> => <<"National">>}),
    ?assertEqual(ok, agdb_database_cached_adapter:delete_keys(PoolName, test, <<"name">>, [<<"Mets0">>, <<"Mets1">>, <<"Mets2">>])).

test_remove_one_bagobj(Config) ->
    PoolName = proplists:get_value(database_cached_pool, Config),
    ?assertEqual(
        ok,
        agdb_database_cached_adapter:remove_one_bagobj(
            PoolName, test, <<"Cards">>,
            #{
                owner_key => <<"name">>,
                owner_k_value => <<"Yankees">>,
                element_key => <<"cardname">>,
                element_k_value => <<"wumei">>
            })).

test_remove_multi_bagobj(Config) ->
    PoolName = proplists:get_value(database_cached_pool, Config),
    agdb_database_cached_adapter:insert_bagobj(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"Yankees">>,
            element_key => <<"cardname">>,
            element_k_value => <<"wumei">>
        },
        #{<<"cardname">> => <<"wumei">>, <<"count">> => 100}),
    agdb_database_cached_adapter:insert_bagobj(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"Yankees">>,
            element_key => <<"cardname">>,
            element_k_value => <<"jialefu">>
        },
        #{<<"cardname">> => <<"jialefu">>, <<"count">> => 1000}),
    agdb_database_cached_adapter:insert_bagobj(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"Yankees">>,
            element_key => <<"cardname">>,
            element_k_value => <<"meilianmei">>
        },
        #{<<"cardname">> => <<"meilianmei">>, <<"count">> => 500}),
    ?assertEqual(ok, agdb_database_cached_adapter:remove_multi_bagobj(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"Yankees">>,
            element_key => <<"cardname">>,
            element_k_value => [<<"wumei">>, <<"jialefu">>, <<"meilianmei">>]
        })).

test_clean_bagobj(Config) ->
    PoolName = proplists:get_value(database_cached_pool, Config),
    agdb_database_cached_adapter:insert_bagobj(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"Yankees">>,
            element_key => <<"cardname">>,
            element_k_value => <<"wumei">>
        },
        #{<<"cardname">> => <<"wumei">>, <<"count">> => 100}),
    agdb_database_cached_adapter:insert_bagobj(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"Yankees">>,
            element_key => <<"cardname">>,
            element_k_value => <<"jialefu">>
        },
        #{<<"cardname">> => <<"jialefu">>, <<"count">> => 1000}),
    agdb_database_cached_adapter:insert_bagobj(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"Yankees">>,
            element_key => <<"cardname">>,
            element_k_value => <<"meilianmei">>
        },
        #{<<"cardname">> => <<"meilianmei">>, <<"count">> => 500}),
    ?assertEqual(ok, agdb_database_cached_adapter:clean_bagobj(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"Yankees">>
        })).

test_set_cached_obj_expire_time(Config) ->
    PoolName = proplists:get_value(database_cached_pool, Config),
    AddObj =
        #{
            <<"name">> => <<"ttl1">>,
            <<"home">> => <<"New York">>,
            <<"league">> => <<"American">>
        },
    agdb_database_cached_adapter:insert_one_and_set_cached_time(PoolName, test, <<"name">>, <<"ttl1">>, AddObj, 86400),
    ObjectKey = make_key(test, <<"ttl1">>),
    assert_redis_expirt_time(ObjectKey, 0),
    agdb_database_cached_adapter:find_one_and_set_cached_time(PoolName, test, <<"name">>, <<"ttl1">>, 100000),
    assert_redis_expirt_time(ObjectKey, 86400),

    agdb_cached_adapter_redis:del(redis_pool, ObjectKey),
    agdb_database_cached_adapter:find_one_and_set_cached_time(PoolName, test, <<"name">>, <<"ttl1">>, 100000),
    assert_redis_expirt_time(ObjectKey, 86400),

    agdb_database_cached_adapter:update_one_and_set_cached_time(PoolName, test, <<"name">>, <<"ttl1">>, AddObj, 110000),
    assert_redis_expirt_time(ObjectKey, 100000),
    agdb_database_cached_adapter:find_one_and_set_cached_time(PoolName, test, <<"name">>, <<"ttl1">>, [<<"name">>], 120000),
    assert_redis_expirt_time(ObjectKey, 110000),

    agdb_cached_adapter_redis:del(redis_pool, ObjectKey),
    agdb_database_cached_adapter:update_one_and_set_cached_time(PoolName, test, <<"name">>, <<"ttl1">>, #{<<"name">> => <<"ttl11">>}, 110000),
    Value = agdb_database_cached_adapter:find_one_and_set_cached_time(PoolName, test, <<"name">>, <<"ttl1">>, 100000),
    ?assertMatch(#{
        <<"name">> := <<"ttl11">>,
        <<"home">> := <<"New York">>,
        <<"league">> := <<"American">>
    }, Value).

assert_redis_expirt_time(Key, BaseTime) ->
    {ok, Time} = agdb_cached_adapter_redis:ttl(redis_pool, Key),
    ?assertEqual(true, Time > BaseTime).

test_set_cached_bagobj_expire_time(Config) ->
    PoolName = proplists:get_value(database_cached_pool, Config),
    agdb_database_cached_adapter:insert_bagobj_and_set_cachedTime(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"ttl1">>,
            element_key => <<"cardname">>,
            element_k_value => <<"wumei">>
        },
        #{<<"cardname">> => <<"wumei">>, <<"count">> => 100}, 1000),
    agdb_database_cached_adapter:insert_bagobj_and_set_cachedTime(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"ttl1">>,
            element_key => <<"cardname">>,
            element_k_value => <<"jialefu">>
        },
        #{<<"cardname">> => <<"jialefu">>, <<"count">> => 1000}, 1000),
    agdb_database_cached_adapter:insert_bagobj_and_set_cachedTime(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"ttl1">>,
            element_key => <<"cardname">>,
            element_k_value => <<"meilianmei">>
        },
        #{<<"cardname">> => <<"meilianmei">>, <<"count">> => 500}, 1000),

    CacheKey = make_bagobj_key(test, <<"Cards">>, <<"ttl1">>, <<"wumei">>),
    CacheBagKey = make_bag_key(test, <<"Cards">>, <<"ttl1">>),
    assert_redis_expirt_time(CacheKey, 10),
    assert_redis_expirt_time(CacheBagKey, 10),

    agdb_database_cached_adapter:find_bagobj_and_set_cached_time(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"ttl1">>,
            element_key => <<"cardname">>
        },2000),
    assert_redis_expirt_time(CacheKey, 1000),
    assert_redis_expirt_time(CacheBagKey, 1000),

    % 删除缓存后测试从db加载的缓存
    agdb_cached_adapter_redis:del(redis_pool, CacheBagKey),
    agdb_database_cached_adapter:find_bagobj_and_set_cached_time(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"ttl1">>,
            element_key => <<"cardname">>
        }, 2000),
    assert_redis_expirt_time(CacheKey, 1000),
    assert_redis_expirt_time(CacheBagKey, 1000),

    agdb_database_cached_adapter:update_one_bagobj_and_set_cached_time(
        PoolName, test, <<"Cards">>,
        #{
            owner_key => <<"name">>,
            owner_k_value => <<"ttl1">>,
            element_key => <<"cardname">>,
            element_k_value => <<"wumei">>
        },
        #{<<"cardname">> => <<"wumei">>, <<"count">> => 1000}, 3000),
    assert_redis_expirt_time(CacheKey, 2000),
    assert_redis_expirt_time(CacheBagKey, 2000).

make_key(Table, Value) ->
    <<(agb_convertor:to_binary(Table))/binary, $., (agb_convertor:to_binary(Value))/binary, $.>>.

make_bagobj_key(Table, BagName, Owner, Key) ->
    <<(agb_convertor:to_binary(Table))/binary, $., (agb_convertor:to_binary(BagName))/binary, $.,
        (agb_convertor:to_binary(Owner))/binary, $., (agb_convertor:to_binary(Key))/binary, $.>>.

make_bag_key(Table, BagName, Owner) ->
    <<(agb_convertor:to_binary(Table))/binary, $., (agb_convertor:to_binary(BagName))/binary, $.,
        (agb_convertor:to_binary(Owner))/binary>>.

