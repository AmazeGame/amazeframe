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
-module(test_database_adapter_mysql_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
%% API
-compile(export_all).

all() ->
    [{group, basic}, test_make_where_string].

groups() ->
    [{basic, [sequence], [
        insert_test,
        find_one_test,
        find_test,
        count_test,
        replace_test,
        update_test,
        add_one_bagobj_test,
        update_one_bagobj_test,
        remove_bagobj_test,
        delete_bagobj_test,
        delete_test,
%%        ensure_index_test,
        pack_value_test
    ]}].

init_per_suite(_Config) ->
    logger:set_primary_config(level, all),
    application:ensure_all_started(ag_database),
    timer:sleep(2000),

    PoolName = mysql_pool,
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

    _Config.

end_per_suite(_Config) ->
    PoolName = mysql_pool,
    Sql_Drop_Test_Table = <<"drop table test">>,
    mysql_poolboy:query(PoolName, Sql_Drop_Test_Table, 500),
    Sql_Drop_Test_Cards_Table = <<"drop table test_Cards">>,
    mysql_poolboy:query(PoolName, Sql_Drop_Test_Cards_Table, 500),

    application:stop(ag_database),
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

insert_test(_Config) ->
    ct:pal("insert_test begin"),
    PoolName = mysql_pool,
    ?assertMatch(
        ok,
        agdb_database_adapter_mysql:insert(PoolName, test, [
            #{<<"name">> => <<"Yankees">>,
                <<"home">> => <<"New York">>,
                <<"league">> => <<"American">>},
            #{<<"name">> => <<"Mets">>,
                <<"home">> => <<"New York">>,
                <<"league">> => <<"National">>}
        ])),
    ?assertMatch(
        ok,
        agdb_database_adapter_mysql:insert(PoolName, test,
            #{<<"name">> => <<"Phillies">>,
                <<"home">> => <<"Philadelphia">>,
                <<"league">> => <<"National">>}
        )).

find_one_test(_Config) ->
    ct:pal("find_one_test begin"),
    PoolName = mysql_pool,
    Doc0 = agdb_database_adapter_mysql:find_one(PoolName, test, #{<<"name">>=><<"Yankees">>}),
    ct:pal("-------findonetest doc0:~p", [Doc0]),
    ?assertEqual(agdb_database_adapter_mysql:find_one(PoolName, test, #{<<"name">>=><<"Yankees1111">>}), undefined),
    Doc1 = agdb_database_adapter_mysql:find_one(PoolName, test, [{<<"name">>, 'equals', <<"Yankees">>}], [<<"name">>]),
    ct:pal("-------findonetest doc1:~p", [Doc1]).

find_test(_Config) ->
    ct:pal("find_test begin"),
    PoolName = mysql_pool,
    Doc0 = agdb_database_adapter_mysql:find(PoolName, test, #{<<"name">>=><<"Yankees">>}),
    ct:pal("-------findonetest doc0:~p", [Doc0]),
    Doc1 = agdb_database_adapter_mysql:find(PoolName, test, #{}, [<<"name">>, <<"league">>]),
    ct:pal("-------findonetest doc1:~p", [Doc1]).

count_test(_Config) ->
    PoolName = mysql_pool,
    ?assertEqual(3, agdb_database_adapter_mysql:count(PoolName, test, #{})),
    ?assertEqual(1, agdb_database_adapter_mysql:count(PoolName, test, #{<<"name">> => <<"Phillies">>})).


replace_test(_Config) ->
    ct:pal("replace_test begin"),
    PoolName = mysql_pool,
    ?assertMatch(
        ok,
        agdb_database_adapter_mysql:replace(PoolName, test, [
            #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>,
                <<"home">> => <<"BeiJing">>,
                <<"league">> => <<"China">>}
        ])),
    ?assertMatch(
        ok,
        agdb_database_adapter_mysql:replace(PoolName, test,
            #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>,
                <<"home">> => <<"BeiJing">>,
                <<"league">> => <<"China">>}
        )),
    print_find_info(PoolName, test, #{}).

update_test(_Config) ->
    PoolName = mysql_pool,
    ?assertEqual(ok, agdb_database_adapter_mysql:update(PoolName, test, #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>}, #{<<"home">> => <<"ShangHai">>})).

add_one_bagobj_test(_Config) ->
    PoolName = mysql_pool,
    ?assertEqual(ok, agdb_database_adapter_mysql:add_one_bagobj(PoolName, test, <<"Cards">>,
        #{<<"name">>=><<"ayongbc <ayongbc@sina.com>">>},
        #{<<"cardname">>=><<"dianying">>, <<"count">>=>10})),
    ?assertEqual(ok, agdb_database_adapter_mysql:add_one_bagobj(PoolName, test, <<"Cards">>,
        #{<<"name">>=><<"ayongbc <ayongbc@sina.com>">>},
        #{<<"cardname">>=><<"wumei">>, <<"count">>=>100})),
    CardInfo = agdb_database_adapter_mysql:find_bag(PoolName, test, <<"Cards">>,
        #{<<"name">>=><<"ayongbc <ayongbc@sina.com>">>}),
    ct:pal("--------add_one_bagobj_test find_bag CardInfo:~p", [CardInfo]).

update_one_bagobj_test(_Config) ->
    PoolName = mysql_pool,
    ?assertEqual(ok, agdb_database_adapter_mysql:update_one_bagobj(PoolName, test, <<"Cards">>,
        #{<<"name">>=><<"wumei">>},
        #{<<"name">>=><<"ayongbc <ayongbc@sina.com>">>},
        #{<<"cardname">>=><<"wumei">>, <<"count">>=>3})),
    CardInfo = agdb_database_adapter_mysql:find_bag(PoolName, test, <<"Cards">>, #{<<"name">>=><<"ayongbc <ayongbc@sina.com>">>}),
    ct:pal("--------update_one_bagobj_test find_bag CardInfo:~p", [CardInfo]).

remove_bagobj_test(_Config) ->
    PoolName = mysql_pool,
    ?assertEqual(ok, agdb_database_adapter_mysql:remove_bagobj(PoolName, test, <<"Cards">>,
        #{<<"name">>=><<"dianying">>},
        #{<<"cardname">>=><<"ayongbc <ayongbc@sina.com>">>})),
    CardInfo = agdb_database_adapter_mysql:find_bag(PoolName, test, <<"Cards">>, #{<<"name">>=><<"ayongbc <ayongbc@sina.com>">>}),
    ct:pal("--------remove_bagobj_test find_bag CardInfo:~p", [CardInfo]).

delete_bagobj_test(_Config) ->
    PoolName = mysql_pool,
    ?assertEqual(ok, agdb_database_adapter_mysql:delete_bagobj(PoolName, test, <<"Cards">>, #{<<"name">>=><<"ayongbc <ayongbc@sina.com>">>})),
    print_find_info(PoolName, test_cards, #{<<"name">>=><<"ayongbc <ayongbc@sina.com>">>}).

delete_test(_Config) ->
    PoolName = mysql_pool,
    agdb_database_adapter_mysql:delete(PoolName, test, #{<<"league">>=><<"China">>}),
    print_find_info(PoolName, test, #{<<"league">>=><<"China">>}).

print_find_info(PoolName, TableName, Selector) ->
    Doc = agdb_database_adapter_mysql:find(PoolName, TableName, Selector),
    ct:pal("-------findonetest doc0:~p", [Doc]).

pack_value_test(_Config) ->
    agdb_database_adapter_mysql:make_where_string(#{<<"name">> => "benben",
        home => #{<<"city">> => <<"BeiJing">>, <<"state">> => 'BJ'},
        num => one,
        <<"league">> => <<"China">>, money => undefined, level => null, gold => [],
        time =>{{2019, 6, 11}, {18, 0, 0}}, age=>35, date => {2019, 6, 11}, trem => {a, b, c, d}}).

test_make_where_string(_Config) ->
    ?assertEqual(
        {<<"where `k1` = ? and `k2` = ? and `k3` is ? and `k4` is ?">>, [<<"v1">>, <<"v2">>, <<"null">>, <<"null">>]},
        agdb_database_adapter_mysql:make_where_string(#{<<"k1">> => <<"v1">>, <<"k2">> => <<"v2">>, <<"k3">>=>undefined, <<"k4">>=>null})
    ),
    ?assertEqual(
        {<<"where `k1` = ? and `k2` = ? and `k3` is ? and `k4` is ?">>, [<<"v1">>, <<"v2">>, <<"null">>, <<"null">>]},
        agdb_database_adapter_mysql:make_where_string([{<<"k1">>, <<"v1">>}, {<<"k2">>, <<"v2">>}, {<<"k3">>, undefined}, {<<"k4">>, null}])
    ),
    ?assertEqual(
        {<<"where `k1` = ? and `k2` = ? and `k3` is ? and `k4` is ?">>, [<<"v1">>, <<"v2">>, <<"null">>, <<"null">>]},
        agdb_database_adapter_mysql:make_where_string([{<<"k1">>, 'equals', <<"v1">>}, {<<"k2">>, 'equals', <<"v2">>},
            {<<"k3">>, 'equals', undefined}, {<<"k4">>, 'equals', null}])
    ),
    ?assertEqual(
        {<<"where `k1` = ? and `k2` = ? and `k3` is ? and `k4` is ?">>, [<<"v1">>, <<"v2">>, <<"null">>, <<"null">>]},
        agdb_database_adapter_mysql:make_where_string([{<<"k1">>, '=', <<"v1">>}, {<<"k2">>, '=', <<"v2">>},
            {<<"k3">>, '=', undefined}, {<<"k4">>, '=', null}])
    ),
    ?assertEqual(
        {<<"where `k1` != ? and `k2` != ? and `k3` is not ? and `k4` is not ?">>, [<<"v1">>, <<"v2">>, <<"null">>, <<"null">>]},
        agdb_database_adapter_mysql:make_where_string([{<<"k1">>, 'not_equals', <<"v1">>}, {<<"k2">>, 'not_equals', <<"v2">>},
            {<<"k3">>, 'not_equals', undefined}, {<<"k4">>, 'not_equals', null}])
    ),
    ?assertEqual(
        {<<"where `k1` != ? and `k2` != ? and `k3` is not ? and `k4` is not ?">>, [<<"v1">>, <<"v2">>, <<"null">>, <<"null">>]},
        agdb_database_adapter_mysql:make_where_string([{<<"k1">>, '!=', <<"v1">>}, {<<"k2">>, '!=', <<"v2">>},
            {<<"k3">>, '!=', undefined}, {<<"k4">>, '!=', null}])
    ),
    ?assertEqual(
        {<<"where `k1` > ? and `k2` > ? and `k3` < ? and `k4` < ?">>, [<<"v1">>, <<"v2">>, <<"v3">>, <<"v4">>]},
        agdb_database_adapter_mysql:make_where_string([{<<"k1">>, 'gt', <<"v1">>}, {<<"k2">>, '>', <<"v2">>},
            {<<"k3">>, 'lt', <<"v3">>}, {<<"k4">>, '<', <<"v4">>}])
    ),
    ?assertEqual(
        {<<"where `k1` >= ? and `k2` >= ? and `k3` <= ? and `k4` <= ?">>, [<<"v1">>, <<"v2">>, <<"v3">>, <<"v4">>]},
        agdb_database_adapter_mysql:make_where_string([{<<"k1">>, 'ge', <<"v1">>}, {<<"k2">>, '>=', <<"v2">>},
            {<<"k3">>, 'le', <<"v3">>}, {<<"k4">>, '<=', <<"v4">>}])
    ),
    ?assertEqual(
        {<<"where `k1` like ?">>, [<<"%v1%">>]},
        agdb_database_adapter_mysql:make_where_string([{<<"k1">>, 'like', <<"v1">>}])
    ),
    ?assertEqual(
        {<<"where `k1` in (?,?,?,?,?) and `k2` >= ? and `k2` <= ?">>, [1, 2, 3, 4, 5, 0, 100]},
        agdb_database_adapter_mysql:make_where_string([{<<"k1">>, 'in', [1, 2, 3, 4, 5]}, {<<"k2">>, 'in', {0, 100}}])
    ),
    ?assertEqual(
        {<<"where `k1` not in (?,?,?,?,?) and `k2` < ? and `k2` > ?">>, [1, 2, 3, 4, 5, 0, 100]},
        agdb_database_adapter_mysql:make_where_string([{<<"k1">>, 'not_in', [1, 2, 3, 4, 5]}, {<<"k2">>, 'not_in', {0, 100}}])
    ).

