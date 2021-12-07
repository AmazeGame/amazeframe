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
-module(test_database_adapter_redis_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
%% API
-compile(export_all).

all() ->
    [test_string,test_hash,test_list,test_set,test_orderset,test_key,test_other].

init_per_suite(_Config) ->
    logger:set_primary_config(level,all),
    application:ensure_all_started(ag_database),
    timer:sleep(2000),
    _Config.

end_per_suite(_Config) ->
    PoolName = redis_pool,
    agdb_cached_adapter_redis:flushdb(PoolName),
    application:stop(ag_database),
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

test_string(_Config)->
    PoolName = redis_pool,
    ?assertEqual(agdb_cached_adapter_redis:set(PoolName,<<"string1">>,<<"value1">>),            {ok,saved}),
    ?assertEqual(agdb_cached_adapter_redis:get(PoolName,<<"string1">>),                         {ok,<<"value1">>}),
    ?assertEqual(agdb_cached_adapter_redis:get(PoolName,<<"notexistkey">>),                     {ok,undefined}),

    ?assertEqual(agdb_cached_adapter_redis:setex(PoolName,<<"string1">>,<<"value2">>,60),       {ok,saved}),
    ?assertEqual(agdb_cached_adapter_redis:get(PoolName,<<"string1">>),                         {ok,<<"value2">>}),
    ?assertEqual(agdb_cached_adapter_redis:ttl(PoolName,<<"string1">>),                         {ok,60}),

    agdb_cached_adapter_redis:set(PoolName,<<"intkey">>,10),
    agdb_cached_adapter_redis:incr(PoolName,<<"intkey">>),
    ?assertEqual(agdb_cached_adapter_redis:get(PoolName,<<"intkey">>),                          {ok,<<"11">>}),
    agdb_cached_adapter_redis:incrby(PoolName,<<"intkey">>,10),
    ?assertEqual(agdb_cached_adapter_redis:get(PoolName,<<"intkey">>),                          {ok,<<"21">>}),
    agdb_cached_adapter_redis:decr(PoolName,<<"intkey">>),
    ?assertEqual(agdb_cached_adapter_redis:get(PoolName,<<"intkey">>),                          {ok,<<"20">>}),
    agdb_cached_adapter_redis:decrby(PoolName,<<"intkey">>,10),
    ?assertEqual(agdb_cached_adapter_redis:get(PoolName,<<"intkey">>),                          {ok,<<"10">>}),

    ?assertEqual(agdb_cached_adapter_redis:append(PoolName,<<"append">>,<<"v3">>),              {ok,2}),
    ?assertEqual(agdb_cached_adapter_redis:append(PoolName,<<"append">>,<<"v4v5">>),            {ok,6}),
    ?assertEqual(agdb_cached_adapter_redis:get(PoolName,<<"append">>),                          {ok,<<"v3v4v5">>}),

    Keys = [<<"string1">>,<<"intkey">>,<<"string2">>],
    ?assertEqual(agdb_cached_adapter_redis:mget(PoolName,Keys),                                 {ok,[<<"value2">>,<<"10">>,undefined]}).

test_hash(_Config)->
    PoolName = redis_pool,
    ?assertEqual(agdb_cached_adapter_redis:hset(PoolName,<<"hashkey1">>,<<"name">>,<<"xiaobai">>),                  {ok,added}),
    ?assertEqual(agdb_cached_adapter_redis:hset(PoolName,<<"hashkey1">>,<<"avg">>,<<"10">>),                        {ok,added}),
    ?assertEqual(agdb_cached_adapter_redis:hset(PoolName,<<"hashkey1">>,<<"avg">>,<<"20">>),                        {ok,saved}),

    ?assertEqual(agdb_cached_adapter_redis:hget(PoolName,<<"hashkey1">>,<<"name">>),                                {ok,<<"xiaobai">>}),
    ?assertEqual(agdb_cached_adapter_redis:hget(PoolName,<<"hashkey1">>,<<"sex">>),                                 {ok,undefined}),

    ?assertEqual(agdb_cached_adapter_redis:hdel(PoolName,<<"hashkey1">>,[<<"avg">>]),                               {ok,deleted}),

    ?assertEqual(agdb_cached_adapter_redis:hexists(PoolName,<<"hashkey1">>,[<<"avg">>]),                            {ok, not_exist}),
    ?assertEqual(agdb_cached_adapter_redis:hexists(PoolName,<<"hashkey1">>,[<<"name">>]),                           {ok, exist}),

    Insert = [<<"name">>,<<"xiaohong">>,<<"age">>,<<"22">>,<<"sex">>,<<"1">>],
    ?assertEqual(agdb_cached_adapter_redis:hmset(PoolName,<<"hashkey2">>, Insert),                                  {ok,saved}),
    KList = [<<"name">>,<<"age">>,<<"sex">>],
    VList = [<<"xiaohong">>,<<"22">>,<<"1">>],
    ?assertEqual(agdb_cached_adapter_redis:hmget(PoolName,<<"hashkey2">>, KList),                                   {ok, VList}),
    ?assertEqual(agdb_cached_adapter_redis:hgetall(PoolName,<<"hashkey2">>),                                        {ok,Insert}),
    ?assertEqual(agdb_cached_adapter_redis:hkeys(PoolName,<<"hashkey2">>),                                          {ok,KList}),
    ?assertEqual(agdb_cached_adapter_redis:hvals(PoolName,<<"hashkey2">>),                                          {ok,VList}),
    ?assertEqual(agdb_cached_adapter_redis:hincrby(PoolName,<<"hashkey2">>,<<"age">>,2),                            {ok,24}),
    ?assertEqual(agdb_cached_adapter_redis:hlen(PoolName,<<"hashkey2">>),                                           {ok,3}).

test_list(_Config)->
    PoolName = redis_pool,
    ?assertEqual(agdb_cached_adapter_redis:lpush(PoolName,<<"listkey1">>,[1,2,3]),                                   {ok,3}),
    ?assertEqual(agdb_cached_adapter_redis:lpush(PoolName,<<"listkey1">>,[4]),                                       {ok,4}),
    ?assertEqual(agdb_cached_adapter_redis:lpop(PoolName,<<"listkey1">>),                                            {ok,<<"4">>}),
    ?assertEqual(agdb_cached_adapter_redis:llen(PoolName,<<"listkey1">>),                                            {ok,3}),

    ?assertEqual(agdb_cached_adapter_redis:lindex(PoolName,<<"listkey1">>,2),                                        {ok,<<"1">>}),
    ?assertEqual(agdb_cached_adapter_redis:lrange(PoolName,<<"listkey1">>,0,-1),                                     {ok,[<<"3">>,<<"2">>,<<"1">>]}),
    ?assertEqual(agdb_cached_adapter_redis:lrem(PoolName,<<"listkey1">>,0,2),                                        {ok,1}),

    ?assertEqual(agdb_cached_adapter_redis:lset(PoolName,<<"listkey1">>,1,100),                                      {ok,saved}),
    ?assertEqual(agdb_cached_adapter_redis:lrange(PoolName,<<"listkey1">>,0,-1),                                     {ok,[<<"3">>,<<"100">>]}),

    ?assertMatch({error,_},         agdb_cached_adapter_redis:lset(PoolName,<<"listkey1">>,10,100)),

    ?assertEqual(agdb_cached_adapter_redis:rpush(PoolName,<<"listkey1">>,[11,12,13,14,15]),                          {ok,7}),
    ?assertEqual(agdb_cached_adapter_redis:rpop(PoolName,<<"listkey1">>),                                            {ok,<<"15">>}),
    ?assertEqual(agdb_cached_adapter_redis:ltrim(PoolName,<<"listkey1">>,2,-1),                                      {ok,deleted}),
    ?assertEqual(agdb_cached_adapter_redis:lrange(PoolName,<<"listkey1">>,0,-1),                                     {ok,[<<"11">>,<<"12">>,<<"13">>,<<"14">>]}).

test_set(_Config)->
    PoolName = redis_pool,
    ?assertEqual(agdb_cached_adapter_redis:sadd(PoolName,<<"setkey">>,[1,1,2,3,4,5]),                                {ok,5}),
    ?assertEqual(agdb_cached_adapter_redis:scard(PoolName,<<"setkey">>),                                             {ok,5}),
    ?assertEqual(agdb_cached_adapter_redis:srem(PoolName,<<"setkey">>,[1,2]),                                        {ok,2}),

    %% smembers函数要谨慎使用，在处理大量数据的set时会出现效率问题，应该用SSCAN迭代获取所有元素
    ?assertEqual(agdb_cached_adapter_redis:smembers(PoolName,<<"setkey">>),                                          {ok,[<<"3">>,<<"4">>,<<"5">>]}),
    ?assertEqual(agdb_cached_adapter_redis:sismember(PoolName,<<"setkey">>,3),                                       {ok,exist}),
    ?assertEqual(agdb_cached_adapter_redis:sismember(PoolName,<<"setkey">>,30),                                      {ok,not_exist}),

    {ok,Random} = agdb_cached_adapter_redis:srandmember(PoolName,<<"setkey">>,2),
    ct:pal("---------testset srandmember random ~p",[Random]),
    {ok,Random1} = agdb_cached_adapter_redis:srandmember(PoolName,<<"setkey">>,-2),
    ct:pal("---------testset srandmember random1 ~p",[Random1]),
    {ok,Random2} = agdb_cached_adapter_redis:spop(PoolName,<<"setkey">>),
    ct:pal("---------testset srandmember random2 ~p",[Random2]),
    ?assertEqual(agdb_cached_adapter_redis:scard(PoolName,<<"setkey">>),                                             {ok,2}),

    ?assertEqual(agdb_cached_adapter_redis:sadd(PoolName,<<"setkey1">>,[1,1,2,3,4,5]),                               {ok,5}),
    ?assertEqual(agdb_cached_adapter_redis:sadd(PoolName,<<"setkey2">>,[5,6,7]),                                     {ok,3}),
    ?assertEqual(agdb_cached_adapter_redis:sunionstore(PoolName,<<"setkey3">>,[<<"setkey1">>,<<"setkey2">>]),        {ok,<<"7">>}).

test_orderset(_Config)->
    PoolName = redis_pool,
    ?assertEqual(agdb_cached_adapter_redis:zadd(PoolName,<<"zadd">>,[1,a,2,b,3,c]),                                  {ok,added}),
    ?assertEqual(agdb_cached_adapter_redis:zcard(PoolName,<<"zadd">>),                                               {ok,3}),
    ?assertEqual(agdb_cached_adapter_redis:zcount(PoolName,<<"zadd">>,1,2),                                          {ok,2}),
    ?assertEqual(agdb_cached_adapter_redis:zincrby(PoolName,<<"zadd">>,<<"a">>,10),                                  {ok,11}),
    ?assertEqual(agdb_cached_adapter_redis:zrange(PoolName,<<"zadd">>,0,-1),                                         {ok,[<<"b">>,<<"c">>,<<"a">>]}),
    ?assertEqual(agdb_cached_adapter_redis:zrange_withscores(PoolName,<<"zadd">>,0,-1),                              {ok,[<<"b">>,<<"2">>,<<"c">>,<<"3">>,<<"a">>,<<"11">>]}),
    ?assertEqual(agdb_cached_adapter_redis:zrangebyscore(PoolName,<<"zadd">>,0,10),                                  {ok,[<<"b">>,<<"c">>]}),
    ?assertEqual(agdb_cached_adapter_redis:zrangebyscore_withscores(PoolName,<<"zadd">>,0,10),                       {ok,[<<"b">>,<<"2">>,<<"c">>,<<"3">>]}),
    ?assertEqual(agdb_cached_adapter_redis:zrangebyscore_limit(PoolName,<<"zadd">>,0,10,0,1),                        {ok,[<<"b">>]}),

    ?assertEqual(agdb_cached_adapter_redis:zrem(PoolName,<<"zadd">>,<<"a">>),                                        {ok,removed}),
    ?assertEqual(agdb_cached_adapter_redis:zrem(PoolName,<<"zadd">>,<<"notexistkey">>),                              {ok,not_exist}),
    ?assertEqual(agdb_cached_adapter_redis:zrank(PoolName,<<"zadd">>,<<"b">>),                                       {ok,1}),
    ?assertEqual(agdb_cached_adapter_redis:zrank(PoolName,<<"zadd">>,<<"notexistkey">>),                             {ok,0}),
    ?assertEqual(agdb_cached_adapter_redis:zscore(PoolName,<<"zadd">>,<<"b">>),                                      {ok,2}),
    ?assertEqual(agdb_cached_adapter_redis:zscore(PoolName,<<"zadd">>,<<"notexistkey">>),                            {ok,0}),
    ?assertEqual(agdb_cached_adapter_redis:zscore(PoolName,<<"zadd">>,<<"b">>),                                      {ok,2}),
    ?assertEqual(agdb_cached_adapter_redis:zismember(PoolName,<<"zadd">>,<<"notexistkey">>),                         {ok,not_exist}).

test_key(_Config)->
    PoolName = redis_pool,
    ?assertEqual(agdb_cached_adapter_redis:set(PoolName,<<"key1">>,<<"value1">>),            {ok,saved}),
    ?assertEqual(agdb_cached_adapter_redis:set(PoolName,<<"key2">>,<<"value2">>),            {ok,saved}),
    ?assertEqual(agdb_cached_adapter_redis:exists(PoolName,<<"key1">>),                      {ok,exist}),
    ?assertEqual(agdb_cached_adapter_redis:exists(PoolName,<<"notexistkey">>),               {ok,not_exist}),
    ?assertEqual(agdb_cached_adapter_redis:expire(PoolName,<<"key1">>,999),                  {ok,set}),
    ?assertEqual(agdb_cached_adapter_redis:expire(PoolName,<<"notexistkey">>,999),           {ok,not_key}),
    {M, S, _} = os:timestamp(),
    T = M * 1000000 + S+6000,
    ?assertEqual(agdb_cached_adapter_redis:expireat(PoolName,<<"key1">>,T),                  {ok,set}),
    ?assertEqual(agdb_cached_adapter_redis:expireat(PoolName,<<"notexistkey">>,T),           {ok,not_exist}),

    ?assertEqual(agdb_cached_adapter_redis:ttl(PoolName,<<"key2">>),                         {ok,no_expire_time}),
    ?assertEqual(agdb_cached_adapter_redis:ttl(PoolName,<<"notexistkey">>),                  {ok,not_key}),

    ?assertMatch({ok,_},        agdb_cached_adapter_redis:pttl(PoolName,<<"key1">>)),
    ?assertMatch({ok,_},        agdb_cached_adapter_redis:ttl(PoolName,<<"key1">>)),

    ?assertEqual(agdb_cached_adapter_redis:pttl(PoolName,<<"key2">>),                        {ok,no_expire_time}),
    ?assertEqual(agdb_cached_adapter_redis:pttl(PoolName,<<"notexistkey">>),                 {ok,not_key}),

    ?assertEqual(agdb_cached_adapter_redis:rename(PoolName,<<"key2">>,<<"key3">>),           {ok,renamed}),
    ?assertEqual(agdb_cached_adapter_redis:del(PoolName,<<"key3">>),                         {ok,1}).

test_other(_Config)->
    PoolName = redis_pool,
    agdb_cached_adapter_redis:flushdb(PoolName),
    ?assertEqual(agdb_cached_adapter_redis:set(PoolName,<<"key1">>,<<"value1">>),            {ok,saved}),
    ?assertEqual(agdb_cached_adapter_redis:set(PoolName,<<"key2">>,<<"value2">>),            {ok,saved}),
    ?assertMatch({ok,_},                                                                     agdb_cached_adapter_redis:keys(PoolName,<<"*">>) ),
    Find =  [["get",<<"key1">>],["get",<<"key2">>]],
    ?assertEqual(agdb_cached_adapter_redis:qp(PoolName,Find),                                [{ok,<<"value1">>},{ok,<<"value2">>}]),
    scan(0,"*",1,[]).

scan(<<"0">>,_,_,Result)->
    Result;
scan(Cursor,Pattern,Num,Result)->
    PoolName = redis_pool,
    {ok,[NewCursor,R]} = agdb_cached_adapter_redis:scan(PoolName,Cursor,Pattern,Num),
    scan(NewCursor,Pattern,Num,Result ++ R).
