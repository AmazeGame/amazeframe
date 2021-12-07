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

-module(agdb_cached_adapter_redis_cluster).

-behaviour(agdb_cached_adapter).

-include_lib("ag_base/include/agb_debuglogger.hrl").
-include_lib("eredis_cluster/include/eredis_cluster.hrl").

-export([connect/2]).
%% for key
-export([
    del/2,
    exists/2,
    expire/3,
    expireat/3,
    ttl/2,
    pttl/2,
    rename/3
]).
%% for string
-export([
    get/2,
    set/3,
    setex/4,
    incr/2,
    incrby/3,
    decr/2,
    decrby/3,
    append/3,
    mget/2
]).
%% for hash
-export([
    hdel/3,
    hexists/3,
    hget/3,
    hset/4,
    hmget/3,
    hmset/3,
    hgetall/2,
    hvals/2,
    hincrby/4,
    hkeys/2,
    hlen/2
]).
%% for list
-export([
    lindex/3,
    llen/2,
    lpop/2,
    lpush/3,
    lrange/4,
    lrem/4,
    lset/4,
    ltrim/4,
    rpop/2,
    rpush/3
]).
%% for set
-export([
    sadd/3,
    scard/2,
    srem/3,
    sismember/3,
    smembers/2,
    srandmember/3,
    spop/2,
    sunionstore/3
]).
%% for sorted set
-export([
    zadd/3,
    zcard/2,
    zcount/4,
    zincrby/4,
    zrange/4,
    zrevrange/4,
    zrange_withscores/4,
    zrevrange_withscores/4,
    zrangebyscore/4,
    zrangebyscore_withscores/4,
    zrangebyscore_limit/6,
    zrem/3,
    zrank/3,
    zrevrank/3,
    zscore/3,
    zismember/3
]).
-export([
    flushdb/1,
    keys/2,
    scan/4,
    q/2,
    qp/2
]).

-type adapter_q_result() :: {ok, atom() | integer()} | redis_result().

-spec connect(atom(), tuple()) ->
    ok | {error, term()}.
connect(PoolName, {PoolArgs, WorkerArgs}) when is_atom(PoolName) ->
    Opts = format_args(PoolArgs, WorkerArgs),
    ok = ensure_sup_start(),
    ok = eredis_cluster:connect(PoolName, Opts).

%%@private Format the params of 'poolboy' and 'eredis'
-spec format_args(PoolArgs, WorkerArgs) ->
    Options when
    PoolArgs :: [poolboy:pool()],
    WorkerArgs :: proplists:proplist(),
    Options :: eredis_cluster:cluster_options().
format_args(PoolArgs, WorkerArgs) ->
    PoolArgs ++ WorkerArgs.

%%@private Ensure 'eredis_cluster_sup.erl' started correctly.
-spec ensure_sup_start() ->
    ok.
ensure_sup_start() ->
    eredis_cluster_sup:start_link(),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%for key%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec keys(atom(), anystring()) ->
    redis_result().
keys(PoolName, KeyPattern) ->
    q(PoolName, ["KEYS", KeyPattern]).

-spec scan(atom(), integer(), binary(), integer()) ->
    redis_result().
scan(PoolName, Cursor, Pattern, Count) ->
    q(PoolName, ["SCAN", Cursor, "MATCH", Pattern, "COUNT", Count]).

%% Delete Key
-spec rename(atom(), binary(), binary()) ->
    adapter_q_result().
rename(PoolName, OldKey, NewKey) ->
    case q(PoolName, ["RENAME", OldKey, NewKey]) of
        {ok, <<"OK">>} ->
            {ok, renamed};
        {error, Reason} ->
            {error, Reason}
    end.

%% Delete Key
-spec del(atom(), binary()) ->
    adapter_q_result().
del(PoolName, Key) ->
    case q(PoolName, ["DEL", Key]) of
        {ok, Count} ->
            {ok, binary_to_integer(Count)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Exists Key
-spec exists(atom(), binary()) ->
    adapter_q_result().
exists(PoolName, Key) ->
    case q(PoolName, ["EXISTS", Key]) of
        {ok, <<"1">>} ->
            {ok, exist};
        {ok, <<"0">>} ->
            {ok, not_exist};
        {error, Reason} ->
            {error, Reason}
    end.

%% Set key Expire time (TIME_IN_SECONDS)
-spec expire(atom(), binary(), integer()) ->
    adapter_q_result().
expire(PoolName, Key, Seconds) ->
    case q(PoolName, ["EXPIRE", Key, Seconds]) of
        {ok, <<"1">>} ->
            {ok, set};
        {ok, <<"0">>} ->
            {ok, not_key};
        {error, Reason} ->
            {error, Reason}
    end.

%% Set key expire timestamp (TIME_IN_UNIX_TIMESTAMP)
-spec expireat(atom(), binary(), integer()) ->
    adapter_q_result().
expireat(PoolName, Key, TimeStamp) ->
    case q(PoolName, ["EXPIREAT", Key, TimeStamp]) of
        {ok, <<"1">>} ->
            {ok, set};
        {ok, <<"0">>} ->
            {ok, not_exist};
        {error, Reason} ->
            {error, Reason}
    end.

%% get key remain time(TIME_IN_SECONDS)
-spec ttl(atom(), binary()) ->
    adapter_q_result().
ttl(PoolName, Key) ->
    case q(PoolName, ["TTL", Key]) of
        {ok, <<"-2">>} ->
            {ok, not_key};
        {ok, <<"-1">>} ->
            {ok, no_expire_time};
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.

%% get key remain timestamp(TIME_IN_UNIX_TIMESTAMP)
-spec pttl(atom(), binary()) ->
    adapter_q_result().
pttl(PoolName, Key) ->
    case q(PoolName, ["PTTL", Key]) of
        {ok, <<"-2">>} ->
            {ok, not_key};
        {ok, <<"-1">>} ->
            {ok, no_expire_time};
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%for string%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Get Value
%% Get the value of a key
-spec get(atom(), binary()) ->
    adapter_q_result().
get(PoolName, Key) ->
    case q(PoolName, ["GET", Key]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

-spec mget(atom(), [binary()]) ->
    redis_result().
mget(PoolName, Keys) ->
    case q(PoolName, ["MGET" | Keys]) of
        {ok, Values} ->
            {ok, Values};
        {error, Reason} ->
            {error, Reason}
    end.

%% Set Value
%% Set the string value of a key
-spec set(atom(), binary(), binary()) ->
    adapter_q_result().
set(PoolName, Key, Value) ->
    case q(PoolName, ["SET", Key, Value]) of
        {ok, _} ->
            {ok, saved};
        {error, Reason} ->
            {error, Reason}
    end.

%% Set Value expire seconds
%% Set the string value of a key
-spec setex(atom(), binary(), binary(), integer()) ->
    adapter_q_result().
setex(PoolName, Key, Value, Seconds) ->
    case q(PoolName, ["SET", Key, Value, "EX", Seconds]) of
        {ok, _} ->
            {ok, saved};
        {error, Reason} ->
            {error, Reason}
    end.

%% Incr Key
%% Increment the integer value of a key by one
-spec incr(atom(), binary()) ->
    adapter_q_result().
incr(PoolName, Key) ->
    case q(PoolName, ["INCR", Key]) of
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Incrby Key
%% Increment the integer value of a key by Value
-spec incrby(atom(), binary(), integer()) ->
    adapter_q_result().
incrby(PoolName, Key, Value) ->
    case q(PoolName, ["INCRBY", Key, Value]) of
        {ok, Count} ->
            {ok, binary_to_integer(Count)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Decr Key
%% Decrement the integer value of a key by one
-spec decr(atom(), binary()) ->
    adapter_q_result().
decr(PoolName, Key) ->
    case q(PoolName, ["DECR", Key]) of
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Decrby Key
%% Decrement the integer value of a key by Value
-spec decrby(atom(), binary(), integer()) ->
    adapter_q_result().
decrby(PoolName, Key, Value) ->
    case q(PoolName, ["DECRBY", Key, Value]) of
        {ok, Count} ->
            {ok, binary_to_integer(Count)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Append Key value return value length
-spec append(atom(), binary(), binary()) ->
    adapter_q_result().
append(PoolName, Key, Value) ->
    case q(PoolName, ["APPEND", Key, Value]) of
        {ok, Length} ->
            {ok, binary_to_integer(Length)};
        {error, Reason} ->
            {error, Reason}
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%for hash%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hash Delete one or more hash fields
%% Removes the specified fields from the hash stored at key
-spec hdel(atom(), binary(), list() | [binary()]) ->
    adapter_q_result().
hdel(PoolName, Key, FieldList) ->
    case q(PoolName, ["HDEL", Key | FieldList]) of
        {ok, _} ->
            {ok, deleted};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Key Filed Exists
%% Get the exists of a hash field
-spec hexists(atom(), binary(), binary()) ->
    adapter_q_result().
hexists(PoolName, Key, Field) ->
    case q(PoolName, ["HEXISTS", Key, Field]) of
        {ok, <<"1">>} ->
            {ok, exist};
        {ok, <<"0">>} ->
            {ok, not_exist};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Key Get Filed
%% Get the value of a hash field
-spec hget(atom(), binary(), binary()) ->
    redis_result().
hget(PoolName, Key, Field) ->
    case q(PoolName, ["HGET", Key, Field]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Key Set Filed Value
%% Set the string value of a hash field
-spec hset(atom(), binary(), binary(), binary()) ->
    adapter_q_result().
hset(PoolName, Key, Field, Value) ->
    case q(PoolName, ["HSET", Key, Field, Value]) of
        {ok, <<"1">>} ->
            {ok, added}; %% add new value
        {ok, <<"0">>} ->
            {ok, saved}; %%cover
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Get Multi Key
%% Get the values of all the given hash fields
-spec hmget(atom(), binary(), list() | [binary()]) ->
    redis_result().
hmget(PoolName, Key, FieldList) ->
    case q(PoolName, ["HMGET", Key | FieldList]) of
        {ok, ValueList} ->
            {ok, ValueList};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Set Multi Key
%% Set multiple hash fields to multiple values
-spec hmset(atom(), binary(), list() | [binary()]) ->
    adapter_q_result().
hmset(PoolName, Key, FieldValueList) ->
    case q(PoolName, ["HMSET", Key | FieldValueList]) of
        {ok, _} ->
            {ok, saved};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Get All Field Value List
-spec hgetall(atom(), binary()) ->
    redis_result().
hgetall(PoolName, Key) ->
    case q(PoolName, ["HGETALL", Key]) of
        {ok, FieldValueList} ->
            {ok, FieldValueList};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Get All filed's Values
-spec hvals(atom(), binary()) ->
    redis_result().
hvals(PoolName, Key) ->
    case q(PoolName, ["HVALS", Key]) of
        {ok, Values} ->
            {ok, Values};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Key Incr filed value
%% Increment the integer value of a hash field by the given number
-spec hincrby(atom(), binary(), binary(), integer()) ->
    adapter_q_result().
hincrby(PoolName, Key, Field, Inc) ->
    case q(PoolName, ["HINCRBY", Key, Field, Inc]) of
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash hey Get All filed list
-spec hkeys(atom(), binary()) ->
    redis_result().
hkeys(PoolName, Key) ->
    case q(PoolName, ["HKEYS", Key]) of
        {ok, Fileds} ->
            {ok, Fileds};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash hey Get All filed list size
-spec hlen(atom(), binary()) ->
    adapter_q_result().
hlen(PoolName, Key) ->
    case q(PoolName, ["HKEYS", Key]) of
        {ok, Fileds} when is_list(Fileds) ->
            {ok, length(Fileds)};
        {ok, _} ->
            {ok, 0};
        {error, Reason} ->
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%for list%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns the element at index index in the list stored at key.
%% The index is zero-based, so 0 means the first element, 1 the second element and so on.
%% Get an element from a list by its index
-spec lindex(atom(), binary(), integer()) ->
    redis_result().
lindex(PoolName, Key, Index) ->
    case q(PoolName, ["LINDEX", Key, Index]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

%% List hey Get list size
-spec llen(atom(), binary()) ->
    adapter_q_result().
llen(PoolName, Key) ->
    case q(PoolName, ["llen", Key]) of
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Remove and get list first element
-spec lpop(atom(), binary()) ->
    redis_result().
lpop(PoolName, Key) ->
    case q(PoolName, ["LPOP", Key]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

%% List Push Head and return list's size
%% Prepend one or multiple values to a list
-spec lpush(atom(), binary(), list() | [binary()]) ->
    adapter_q_result().
lpush(PoolName, Key, ValueList) ->
    case q(PoolName, ["LPUSH", Key | ValueList]) of
        {ok, Count} ->
            {ok, binary_to_integer(Count)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get List Values
%% Get a range of elements from a list
-spec lrange(atom(), binary(), integer(), integer()) ->
    redis_result().
lrange(PoolName, Key, Start, Stop) ->
    case q(PoolName, ["LRANGE", Key, Start, Stop]) of
        {ok, Values} ->
            {ok, Values};
        {error, Reason} ->
            {error, Reason}
    end.

%% Remove elements from a list
%% Removes the first count occurrences of elements equal to value from the list stored at key.
%% The count argument influences the operation in the following ways:
% count > 0: Remove elements equal to value moving from head to tail.
% count < 0: Remove elements equal to value moving from tail to head.
% count = 0: Remove all elements equal to value.
-spec lrem(atom(), binary(), integer(), integer()) ->
    adapter_q_result().
lrem(PoolName, Key, Count, Value) ->
    case q(PoolName, ["LREM", Key, Count, Value]) of
        {ok, DelCount} ->
            {ok, binary_to_integer(DelCount)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Set value by index
-spec lset(atom(), binary(), integer(), binary()) ->
    adapter_q_result().
lset(PoolName, Key, Index, Value) ->
    case q(PoolName, ["LSET", Key, Index, Value]) of
        {ok, _} ->
            {ok, saved};
        {error, Reason} ->
            {error, Reason}
    end.

%% Trim List
%% Trim a list to the specified range
-spec ltrim(atom(), binary(), integer(), integer()) ->
    adapter_q_result().
ltrim(PoolName, Key, Start, Stop) ->
    case q(PoolName, ["LTRIM", Key, Start, Stop]) of
        {ok, <<"OK">>} ->
            {ok, deleted};
        {error, Reason} ->
            {error, Reason}
    end.

%% Remove and get list first element
-spec rpop(atom(), binary()) ->
    redis_result().
rpop(PoolName, Key) ->
    case q(PoolName, ["RPOP", Key]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

%% List Push Tail and return list's size
%% Prepend one or multiple values to a list
-spec rpush(atom(), binary(), list() | [binary()]) ->
    adapter_q_result().
rpush(PoolName, Key, ValueList) ->
    case q(PoolName, ["RPUSH", Key | ValueList]) of
        {ok, Count} ->
            {ok, binary_to_integer(Count)};
        {error, Reason} ->
            {error, Reason}
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%for set%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add Values to set
%% Add one or more members to a set
-spec sadd(atom(), binary(), list() | [binary()]) ->
    adapter_q_result().
sadd(PoolName, Key, Values) ->
    case q(PoolName, ["SADD", Key | Values]) of
        {ok, AddedCount} ->
            {ok, binary_to_integer(AddedCount)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get Size of a Set
-spec scard(atom(), binary()) ->
    adapter_q_result().
scard(PoolName, Key) ->
    case q(PoolName, ["SCARD", Key]) of
        {ok, Count} ->
            {ok, binary_to_integer(Count)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Remove Values from a set
-spec srem(atom(), binary(), list() | [binary()]) ->
    adapter_q_result().
srem(PoolName, Key, Values) ->
    case q(PoolName, ["SREM", Key | Values]) of
        {ok, RemovedCount} ->
            {ok, binary_to_integer(RemovedCount)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Is Member of a Set
-spec sismember(atom(), binary(), binary()) ->
    adapter_q_result().
sismember(PoolName, Key, Value) ->
    case q(PoolName, ["SISMEMBER", Key, Value]) of
        {ok, <<"1">>} ->
            {ok, exist};
        {ok, <<"0">>} ->
            {ok, not_exist};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get All Values From a Set
-spec smembers(atom(), binary()) ->
    redis_result().
smembers(PoolName, Key) ->
    case q(PoolName, ["SMEMBERS", Key]) of
        {ok, Values} ->
            {ok, Values};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get Random Values From a Set
-spec srandmember(atom(), binary(), integer()) ->
    redis_result().
srandmember(PoolName, Key, Count) ->
    case q(PoolName, ["SRANDMEMBER", Key, Count]) of
        % {ok, undefined} -> {ok, []};
        {ok, Values} ->
            {ok, Values};
        {error, Reason} ->
            {error, Reason}
    end.

%% remove and return rand removed element
-spec spop(atom(), binary()) ->
    redis_result().
spop(PoolName, Key) ->
    case q(PoolName, ["SPOP", Key]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

-spec sunionstore(atom(), binary(), binary()|list()) ->
    redis_result().
sunionstore(PoolName, Destination, Key) when is_binary(Key) ->
    sunionstore(PoolName, Destination, [Key]);
sunionstore(PoolName, Destination, KeyList) ->
    case q(PoolName, ["SUNIONSTORE", Destination | KeyList]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%for sorted set%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sort Set Add
%% Add one to a sorted set, or update its score if it already exists
-spec zadd(atom(), binary(), list()) ->
    adapter_q_result().
zadd(PoolName, Key, ScoreValues) ->
    case q(PoolName, ["ZADD", Key | ScoreValues]) of
        {ok, _} ->
            {ok, added};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get Size
%% Get the number of members in a sorted set
-spec zcard(atom(), binary()) ->
    adapter_q_result().
zcard(PoolName, Key) ->
    case q(PoolName, ["ZCARD", Key]) of
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get Size by special minScore ==> maxScore
%% Get the number of members in a sorted set
-spec zcount(atom(), binary(), integer(), integer()) ->
    adapter_q_result().
zcount(PoolName, Key, Min, Max) ->
    case q(PoolName, ["ZCOUNT", Key, Min, Max]) of
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Incr Score
%% Increment the score of a member in a sorted set
-spec zincrby(atom(), binary(), binary(), integer()) ->
    adapter_q_result().
zincrby(PoolName, Key, Value, Increment) ->
    case q(PoolName, ["ZINCRBY", Key, Increment, Value]) of
        {ok, Result} ->
            {ok, binary_to_integer(Result)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Return a range of members in a sorted set, by index  (0 is first member, -1 is last member)
-spec zrange(atom(), binary(), binary(), integer()) ->
    redis_result().
zrange(PoolName, Key, Start, Stop) ->
    case q(PoolName, ["ZRANGE", Key, Start, Stop]) of
        {ok, Members} ->
            {ok, Members};
        {error, Reason} ->
            {error, Reason}
    end.

%% Return a range of members in a sorted set, by index  (0 is first member, -1 is last member)
-spec zrevrange(atom(), binary(), binary(), integer()) ->
    redis_result().
zrevrange(PoolName, Key, Start, Stop) ->
    case q(PoolName, ["ZREVRANGE", Key, Start, Stop]) of
        {ok, Members} ->
            {ok, Members};
        {error, Reason} ->
            {error, Reason}
    end.

%% Return a range of member scores in a sorted set, by index  (0 is first member, -1 is last member)
-spec zrange_withscores(atom(), binary(), binary(), integer()) ->
    redis_result().
zrange_withscores(PoolName, Key, Start, Stop) ->
    case q(PoolName, ["ZRANGE", Key, Start, Stop, "WITHSCORES"]) of
        {ok, MemberScores} ->
            {ok, MemberScores};
        {error, Reason} ->
            {error, Reason}
    end.

%% Return a range of member scores in a sorted set, by index  (0 is first member, -1 is last member)
-spec zrevrange_withscores(atom(), binary(), binary(), integer()) ->
    redis_result().
zrevrange_withscores(PoolName, Key, Start, Stop) ->
    case q(PoolName, ["ZREVRANGE", Key, Start, Stop, "WITHSCORES"]) of
        {ok, MemberScores} ->
            {ok, MemberScores};
        {error, Reason} ->
            {error, Reason}
    end.

%% Return a range of members in a sorted set, by score(ScoreStart -inf, ScoreStop +info) for get all Members
-spec zrangebyscore(atom(), binary(), binary(), integer()) ->
    redis_result().
zrangebyscore(PoolName, Key, ScoreStart, ScoreStop) ->
    case q(PoolName, ["ZRANGEBYSCORE", Key, ScoreStart, ScoreStop]) of
        {ok, Members} ->
            {ok, Members};
        {error, Reason} ->
            {error, Reason}
    end.

%% Return a range of member scores in a sorted set, by score(ScoreStart -inf, ScoreStop +info) for get all Member Scores
-spec zrangebyscore_withscores(atom(), binary(), binary(), integer()) ->
    redis_result().
zrangebyscore_withscores(PoolName, Key, ScoreStart, ScoreStop) ->
    case q(PoolName, ["ZRANGEBYSCORE", Key, ScoreStart, ScoreStop, "WITHSCORES"]) of
        {ok, MemberScores} ->
            {ok, MemberScores};
        {error, Reason} ->
            {error, Reason}
    end.

-spec zrangebyscore_limit(atom(), binary(), binary(), integer(), integer(), integer()) ->
    redis_result().
zrangebyscore_limit(PoolName, Key, ScoreStart, ScoreStop, LimitStart, LimitEnd) ->
    case q(PoolName, ["ZRANGEBYSCORE", Key, ScoreStart, ScoreStop, "LIMIT", LimitStart, LimitEnd]) of
        {ok, MemberScores} ->
            {ok, MemberScores};
        {error, Reason} ->
            {error, Reason}
    end.

%% Sort Set Remove
%% Remove one from a sorted set
-spec zrem(atom(), binary(), binary()) ->
    adapter_q_result().
zrem(PoolName, Key, Value) ->
    case q(PoolName, ["ZREM", Key, Value]) of
        {ok, <<"1">>} ->
            {ok, removed};
        {ok, <<"0">>} ->
            {ok, not_exist};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get Rank
%% Determine the index of a member in a sorted set
-spec zrank(atom(), binary(), binary()) ->
    adapter_q_result().
zrank(PoolName, Key, Value) ->
    case q(PoolName, ["ZRANK", Key, Value]) of
        % {ok, undefined} -> {ok, 0};
        {ok, Rank} ->
            {ok, binary_to_integer(Rank) + 1};
        {error, Reason} ->
            {error, Reason}
    end.

-spec zrevrank(atom(), binary(), binary()) ->
    adapter_q_result().
zrevrank(PoolName, Key, Value) ->
    case q(PoolName, ["ZREVRANK", Key, Value]) of
        % {ok, undefined} -> {ok, 0};
        {ok, Rank} ->
            {ok, binary_to_integer(Rank) + 1};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get Score
%% Get the score associated with the given member in a sorted set
-spec zscore(atom(), binary(), binary()) ->
    adapter_q_result().
zscore(PoolName, Key, Member) ->
    case q(PoolName, ["ZSCORE", Key, Member]) of
        % {ok, undefined} -> {ok, 0};
        {ok, Score} ->
            {ok, binary_to_integer(Score)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Has Key
%% Has Key In Sort Set
-spec zismember(atom(), binary(), binary()) ->
    adapter_q_result().
zismember(PoolName, Key, Member) ->
    case zscore(PoolName, Key, Member) of
        {ok, 0} ->
            {ok, not_exist};
        {ok, _} ->
            {ok, exist};
        {error, Reason} ->
            {error, Reason}
    end.

%% Flush DB
-spec flushdb(atom()) ->
    redis_result().
flushdb(PoolName) ->
    {ok, <<"OK">>} = q(PoolName, [<<"FLUSHDB">>]).

% -spec q(term(), [list()|binary()|integer()]) -> any().
-spec q(atom(), redis_command()) ->
    redis_result().
q(PoolName, Command) ->
    case eredis_cluster:q(PoolName, Command) of
        {error, Reason} ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>>agdb_cached_adapter_redis q error:[~p],PoolName:[~p],Command:[~p]",
                [Reason, PoolName, Command]),
            {error, Reason};
        Result ->
            Result
    end.

% -spec qp(term(), [iolist()]) -> any().
-spec qp(atom(), redis_pipeline_command()) ->
    redis_pipeline_result().
qp(PoolName, Pipeline) ->
    Result = eredis_cluster:qp(PoolName, Pipeline),
    lists:foreach(fun
        ({error, Reason}) ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>>agdb_cached_adapter_redis q error:[~p],PoolName:[~p],Pipeline:[~p]",
                [Reason, PoolName, Pipeline]),
            {error, Reason};
        (_) ->
            ok
    end, Result),
    Result.
