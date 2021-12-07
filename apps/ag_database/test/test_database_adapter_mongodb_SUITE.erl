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
-module(test_database_adapter_mongodb_SUITE).


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
%% API
-compile(export_all).

all() ->
    [{group, basic}, test_make_selector_string, connect_test].

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
        ensure_index_test,
        pack_value_test
    ]}].

init_per_suite(Config) ->
    logger:set_primary_config(level, all),
    application:ensure_all_started(ag_database),
    timer:sleep(2000),
    Config.

end_per_suite(_Config) ->
    application:stop(ag_database),
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

init_per_group(_Group, Config) ->
    agdb_database_adapter_mongodb:delete(mongodb_pool, test, #{}),
    Config.

end_per_group(_Group, _Config) ->
    agdb_database_adapter_mongodb:delete(mongodb_pool, test, #{}),
    ok.

insert_test(_Config) ->
    ct:pal("insert_test begin"),
    PoolName = mongodb_pool,
    ?assertMatch(
        ok,
        agdb_database_adapter_mongodb:insert(PoolName, test, [
            #{<<"name">> => <<"Yankees">>,
                <<"home">> => #{<<"city">> => <<"New York">>, <<"state">> => <<"NY">>},
                <<"league">> => <<"American">>},
            #{<<"name">> => <<"Mets">>,
                <<"home">> => #{<<"city">> => <<"New York">>, <<"state">> => <<"NY">>},
                <<"league">> => <<"National">>}
        ])),
    ?assertMatch(
        {error, {1, _}},
        agdb_database_adapter_mongodb:insert(PoolName, test, [
            #{<<"name">> => <<"Yankees01">>,
                <<"home">> => #{<<"city">> => <<"New York">>, <<"state">> => <<"NY">>},
                <<"league">> => <<"American">>},
            #{<<"name">> => <<"Mets">>,
                <<"home">> => #{<<"city">> => <<"New York">>, <<"state">> => <<"NY">>},
                <<"league">> => <<"National">>}
        ])),
    ?assertMatch(
        {error, _},
        agdb_database_adapter_mongodb:insert(mongodb_pool, test,
            #{<<"name">> => <<"Yankees01">>,
                <<"home">> => #{<<"city">> => <<"New York">>, <<"state">> => <<"NY">>},
                <<"league">> => <<"American">>}
        )),
    ?assertMatch(
        ok,
        agdb_database_adapter_mongodb:insert(PoolName, test, [
            #{<<"name">> => <<"Phillies">>,
                <<"home">> => #{<<"city">> => <<"Philadelphia">>, <<"state">> => <<"PA">>},
                <<"league">> => <<"National">>},
            #{<<"name">> => <<"Red Sox">>,
                <<"home">> => #{<<"city">> => <<"Boston">>, <<"state">> => <<"MA">>},
                <<"league">> => <<"American">>}
        ])),
    ?assertMatch(
        {error, {1, _}},
        agdb_database_adapter_mongodb:insert(PoolName, test, [
            #{<<"name">> => <<"Phillies01">>,
                <<"home">> => #{<<"city">> => <<"Philadelphia">>, <<"state">> => <<"PA">>},
                <<"league">> => <<"National">>},
            #{<<"name">> => <<"Red Sox">>,
                <<"home">> => #{<<"city">> => <<"Boston">>, <<"state">> => <<"MA">>},
                <<"league">> => <<"American">>}
        ])),
    ?assertMatch(
        {error, _},
        agdb_database_adapter_mongodb:insert(PoolName, test, [
            #{<<"name">> => <<"Phillies01">>,
                <<"home">> => #{<<"city">> => <<"Philadelphia">>, <<"state">> => <<"PA">>},
                <<"league">> => <<"National">>}
        ])),
    agdb_database_adapter_mongodb:delete(PoolName, test, [{<<"name">>, 'in', [<<"Phillies01">>, <<"Yankees01">>]}]).

find_one_test(_Config) ->
    ct:pal("find_one_test begin"),
    PoolName = mongodb_pool,
    Doc0 = agdb_database_adapter_mongodb:find_one(PoolName, test, #{<<"name">> => <<"Yankees">>}),
    ct:pal("-------findonetest doc0:~p", [Doc0]),
    ?assertEqual(agdb_database_adapter_mongodb:find_one(PoolName, test, #{<<"name">> => <<"Yankees1111">>}), undefined),
    Doc1 = agdb_database_adapter_mongodb:find_one(PoolName, test, [{<<"name">>, 'equals', <<"Yankees">>}], [<<"name">>]),
    ct:pal("-------findonetest doc1:~p", [Doc1]).

find_test(_Config) ->
    ct:pal("find_test begin"),
    PoolName = mongodb_pool,
    Doc0 = agdb_database_adapter_mongodb:find(PoolName, test, #{<<"name">> => <<"Yankees">>}),
    ct:pal("-------findonetest doc0:~p", [Doc0]),
    Doc1 = agdb_database_adapter_mongodb:find(PoolName, test, #{}, [<<"name">>, <<"league">>]),
    ct:pal("-------findonetest doc1:~p", [Doc1]),
    Doc2 = agdb_database_adapter_mongodb:find(PoolName, test, #{}, [<<"name">>, <<"league">>], 0, 0),
    ct:pal("-------findonetest doc1:~p", [Doc2]).

count_test(_Config) ->
    PoolName = mongodb_pool,
    ?assertEqual(4, agdb_database_adapter_mongodb:count(PoolName, test, #{})),
    ?assertEqual(1, agdb_database_adapter_mongodb:count(PoolName, test, #{<<"name">> => <<"Phillies">>})),
    ?assertEqual(2, agdb_database_adapter_mongodb:count(PoolName, test, #{}, #{limit => 2})).

replace_test(_Config) ->
    ct:pal("replace_test begin"),
    PoolName = mongodb_pool,
    ?assertMatch(
        ok,
        agdb_database_adapter_mongodb:replace(PoolName, test, [
            #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>,
                <<"home">> => #{<<"city">> => <<"BeiJing">>, <<"state">> => <<"BJ">>},
                <<"league">> => <<"China">>}
        ])),
    print_find_info(PoolName, #{}).

update_test(_Config) ->
    ct:pal("replace_test update_test"),
    PoolName = mongodb_pool,
    ?assertEqual(ok, agdb_database_adapter_mongodb:update(PoolName, test, #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>},
        #{<<"age">> => <<"24">>})),
    print_find_info(PoolName, #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>}),
    ?assertEqual(ok, agdb_database_adapter_mongodb:update(PoolName, test, #{<<"name">> => <<"Gillen">>}, #{<<"name">> => <<"Gillen">>,
        <<"home">> => #{<<"city">> => <<"BeiJing">>, <<"state">> => <<"BJ">>},
        <<"league">> => <<"China">>}, true, true)),
    ?assertEqual(1, agdb_database_adapter_mongodb:count(PoolName, test, #{<<"name">> => <<"Gillen">>})),
%%    ?assertEqual(ok,agdb_database_adapter_mongodb:update(PoolName,test,#{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>},
%%        #{<<"age">> => <<"34">>},false,false,{<<"w">>,1})),
    print_find_info(PoolName, #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>}).

add_one_bagobj_test(_Config) ->
    PoolName = mongodb_pool,
    ?assertEqual(ok, agdb_database_adapter_mongodb:add_one_bagobj(PoolName, test, <<"Cards">>,
        #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>},
        #{<<"name">> => <<"dianying">>, <<"count">> => 10})),
    ?assertEqual(ok, agdb_database_adapter_mongodb:add_one_bagobj(PoolName, test, <<"Cards">>,
        #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>},
        #{<<"name">> => <<"wumei">>, <<"count">> => 100})),
    CardInfo = agdb_database_adapter_mongodb:find_bag(PoolName, test, <<"Cards">>,
        #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>}),
    ct:pal("--------add_one_bagobj_test find_bag CardInfo:~p", [CardInfo]).

update_one_bagobj_test(_Config) ->
    PoolName = mongodb_pool,
    ?assertEqual(ok, agdb_database_adapter_mongodb:update_one_bagobj(PoolName, test, <<"Cards">>,
        #{<<"name">> => <<"wumei">>},
        #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>},
        #{<<"name">> => <<"wumei">>, <<"count">> => 3})),
    CardInfo = agdb_database_adapter_mongodb:find_bag(PoolName, test, <<"Cards">>, #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>}),
    ct:pal("--------update_one_bagobj_test find_bag CardInfo:~p", [CardInfo]).

remove_bagobj_test(_Config) ->
    PoolName = mongodb_pool,
    ?assertEqual(ok, agdb_database_adapter_mongodb:remove_bagobj(PoolName, test, <<"Cards">>,
        #{<<"name">> => <<"wumei">>},
        #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>})),
    CardInfo = agdb_database_adapter_mongodb:find_bag(PoolName, test, <<"Cards">>, #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>}),
    ct:pal("--------remove_bagobj_test find_bag CardInfo:~p", [CardInfo]).

delete_bagobj_test(_Config) ->
    PoolName = mongodb_pool,
    ?assertEqual(ok, agdb_database_adapter_mongodb:delete_bagobj(PoolName, test, <<"Cards">>, #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>})),
    print_find_info(PoolName, #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>}).

delete_test(_Config) ->
    PoolName = mongodb_pool,
    print_find_info(PoolName, #{<<"league">> => <<"China">>}),
    agdb_database_adapter_mongodb:delete(PoolName, test, #{<<"league">> => <<"China">>}).
%%    agdb_database_adapter_mongodb:insert(PoolName,test,[
%%        #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>,
%%            <<"home">> => #{<<"city">> => <<"BeiJing">>, <<"state">> => <<"BJ">>},
%%            <<"league">> => <<"China">>},
%%        #{<<"name">> => <<"ayongbc <ayongbc@sina.com>1">>,
%%            <<"home">> => #{<<"city">> => <<"BeiJing">>, <<"state">> => <<"BJ">>},
%%            <<"league">> => <<"China">>},
%%        #{<<"name">> => <<"ayongbc <ayongbc@sina.com>2">>,
%%            <<"home">> => #{<<"city">> => <<"BeiJing">>, <<"state">> => <<"BJ">>},
%%            <<"league">> => <<"China">>}
%%    ]),
%%    agdb_database_adapter_mongodb:delete_one(PoolName,test,#{<<"league">>=><<"China">>}),
%%    print_find_info(PoolName,#{<<"league">>=><<"China">>}),
%%    agdb_database_adapter_mongodb:delete_limit(PoolName,test,#{<<"league">>=><<"China">>},2),
%%    print_find_info(PoolName,#{<<"league">>=><<"China">>}),
%%    agdb_database_adapter_mongodb:delete_one(PoolName,test,#{<<"league">>=><<"China">>}),
%%    print_find_info(PoolName,#{<<"league">>=><<"China">>}).

ensure_index_test(_Config) ->
    PoolName = mongodb_pool,
    %#{<<"key">> => #{<<"index">> => 1}, <<"name">> => <<"MyI">>, <<"unique">> => true, <<"dropDups">> => true}
    agdb_database_adapter_mongodb:ensure_index(PoolName, test, #{<<"key">> => #{<<"name">> => 1}, <<"name">> => <<"_name">>, <<"unique">> => true, <<"dropDups">> => true}),
    agdb_database_adapter_mongodb:insert(PoolName, test, [
        #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>,
            <<"home">> => #{<<"city">> => <<"BeiJing">>, <<"state">> => <<"BJ">>},
            <<"league">> => <<"China">>}
    ]),
    ?assertMatch(
        {error, {1, _}},
        agdb_database_adapter_mongodb:insert(PoolName, test, [
            #{<<"name">> => <<"ayongbc <ayongbc@sina.com>1111">>,
                <<"home">> => #{<<"city">> => <<"BeiJing">>, <<"state">> => <<"BJ">>},
                <<"league">> => <<"China">>},
            #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>,
                <<"home">> => #{<<"city">> => <<"BeiJing">>, <<"state">> => <<"BJ">>},
                <<"league">> => <<"China">>}
        ])),
    ?assertMatch(
        {error, _},
        agdb_database_adapter_mongodb:insert(PoolName, test, [
            #{<<"name">> => <<"ayongbc <ayongbc@sina.com>">>,
                <<"home">> => #{<<"city">> => <<"BeiJing">>, <<"state">> => <<"BJ">>},
                <<"league">> => <<"China">>}
        ])).

print_find_info(PoolName, Selector) ->
    Doc = agdb_database_adapter_mongodb:find(PoolName, test, Selector),
    ct:pal("-------findonetest doc0:~p", [Doc]).

test_make_selector_string(_Config) ->
    ?assertEqual(
        {<<"k1">>, <<"v1">>},
        agdb_database_adapter_mongodb:make_selector_string(#{<<"k1">> => <<"v1">>})
    ),
    ?assertEqual(
        {<<"k1">>, <<"v1">>, <<"k2">>, <<"v2">>},
        agdb_database_adapter_mongodb:make_selector_string(#{<<"k1">> => <<"v1">>, <<"k2">> => <<"v2">>})
    ),
    ?assertEqual(
        {<<"k1">>, <<"v1">>},
        agdb_database_adapter_mongodb:make_selector_string([{<<"k1">>, <<"v1">>}])
    ),
    ?assertEqual(
        {<<"k1">>, <<"v1">>, <<"k2">>, <<"v2">>},
        agdb_database_adapter_mongodb:make_selector_string([{<<"k1">>, <<"v1">>}, {<<"k2">>, <<"v2">>}])
    ),
    ?assertEqual(
        {<<"k1">>, <<"v1">>, <<"k2">>, <<"v2">>},
        agdb_database_adapter_mongodb:make_selector_string([{<<"k1">>, 'equals', <<"v1">>}, {<<"k2">>, '=', <<"v2">>}])
    ),
    ?assertEqual(
        {<<"k1">>, {'$ne', <<"v1">>}, <<"k2">>, {'$ne', <<"v2">>}},
        agdb_database_adapter_mongodb:make_selector_string([{<<"k1">>, 'not_equals', <<"v1">>}, {<<"k2">>, 'not_equals', <<"v2">>}])
    ),
    ?assertEqual(
        {<<"k1">>, {'$ne', <<"v1">>}, <<"k2">>, {'$ne', <<"v2">>}},
        agdb_database_adapter_mongodb:make_selector_string([{<<"k1">>, '!=', <<"v1">>}, {<<"k2">>, '!=', <<"v2">>}])
    ),
    ?assertEqual(
        {<<"k1">>, {'$lte', 100}, <<"k1">>, {'$gte', 0}},
        agdb_database_adapter_mongodb:make_selector_string([{<<"k1">>, 'in', {0, 100}}])
    ),
    ?assertEqual(
        {<<"k1">>, {'$in', [0, 1, 2, 3]}},
        agdb_database_adapter_mongodb:make_selector_string([{<<"k1">>, 'in', [0, 1, 2, 3]}])
    ),
    ?assertEqual(
        {'$or', [{<<"k1">>, {'$lt', 0}}, {<<"k1">>, {'$gt', 100}}]},
        agdb_database_adapter_mongodb:make_selector_string([{<<"k1">>, 'not_in', {0, 100}}])
    ),
    ?assertEqual(
        {<<"k1">>, {'$nin', [0, 1, 2, 3]}},
        agdb_database_adapter_mongodb:make_selector_string([{<<"k1">>, 'not_in', [0, 1, 2, 3]}])
    ),
    ?assertEqual(
        {<<"k1">>, {'$gt', <<"v1">>}, <<"k2">>, {'$lt', <<"v2">>}},
        agdb_database_adapter_mongodb:make_selector_string([{<<"k1">>, '>', <<"v1">>}, {<<"k2">>, '<', <<"v2">>}])
    ),
    ?assertEqual(
        {<<"k1">>, {'$gt', <<"v1">>}, <<"k2">>, {'$lt', <<"v2">>}},
        agdb_database_adapter_mongodb:make_selector_string([{<<"k1">>, 'gt', <<"v1">>}, {<<"k2">>, 'lt', <<"v2">>}])
    ),
    ?assertEqual(
        {<<"k1">>, {'$gte', <<"v1">>}, <<"k2">>, {'$lte', <<"v2">>}},
        agdb_database_adapter_mongodb:make_selector_string([{<<"k1">>, '>=', <<"v1">>}, {<<"k2">>, '<=', <<"v2">>}])
    ),
    ?assertEqual(
        {<<"k1">>, {'$gte', <<"v1">>}, <<"k2">>, {'$lte', <<"v2">>}},
        agdb_database_adapter_mongodb:make_selector_string([{<<"k1">>, 'ge', <<"v1">>}, {<<"k2">>, 'le', <<"v2">>}])
    ).

pack_value_test(_Config) ->
    agdb_database_adapter_mongodb:make_selector_string(#{<<"name">> => "benben",
        home => #{<<"city">> => <<"BeiJing">>, <<"state">> => 'BJ'}, <<"num">> => one,
        <<"league">> => <<"China">>, money => undefined, level => null, gold => [], money1 => 'undefined', level => 'null',
        time => {{2019, 6, 11}, {18, 0, 0}}, age => 35, date => {2019, 6, 11}, trem => {a, b, c, d}}).

connect_test(_Config) ->
    ?assertMatch(
        ok,
        agdb_database_adapter_mongodb:connect(test121, {
            [],
            {unknown, ["127.0.0.1:27017"]},
            [{pool_size, 50}, {max_overflow, 10}],
            [{database, <<"maze_server1">>}, {w_mode, safe}, {r_mode, slave_ok}]
        })),
    ?assertMatch(
        ok,
        agdb_database_adapter_mongodb:connect(test122, {
            [],
            {unknown, ["127.0.0.1:27017"]},
            [{pool_size, 50}, {max_overflow, 10}],
            [{database, <<"maze_server1">>}, {w_mode, safe}, {r_mode, slave_ok}]
        })),
    timer:sleep(2000).

