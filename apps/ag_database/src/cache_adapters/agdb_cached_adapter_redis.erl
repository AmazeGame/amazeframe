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

-module(agdb_cached_adapter_redis).

-behaviour(agdb_cached_adapter).

-include_lib("ag_base/include/agb_debuglogger.hrl").

-export([connect/2]).
%% redis API
-export([
    q/2, q/3,
    qp/2, qp/3,
    transaction/2
]).
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
    zismember/3,
    zunionstore/4
]).
-export([
    flushdb/1,
    keys/2,
    scan/4,
    eval/2,
    lrem/3,
    script_load/2,
    evalsha/2
]).

%% Default timeout for calls to the client gen_server
%% Specified in http://www.erlang.org/doc/man/gen_server.html#call-3
-define(TIMEOUT, 5000).

%%@pricate copy from eredis.hrl
-type return_value() :: undefined | binary() | [binary() | list()].

-spec connect(atom(), tuple()) ->
    ok | {error, term()}.
connect(PoolName, {PoolArgs, WorkerArgs}) ->
    PoolArgs1 = [{strategy, fifo}, {name, {local, PoolName}}, {worker_module, eredis} | PoolArgs],
    case ag_database_sup:start_child(poolboy:child_spec(PoolName, PoolArgs1, WorkerArgs)) of
        {ok, _PID} ->
            ok;
        {error, {already_started, _PID}} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end;
connect(PoolName, {SentinelArgs, PoolArgs, WorkerArgs}) ->
    Sentinel =
        case proplists:get_value(host, WorkerArgs) of
            "sentinel:" ++ MasterStr ->
                list_to_atom(MasterStr);
            _ ->
                agb_error:error("not Sentinel")
        end,
    ag_database_sup:start_child({Sentinel,
        {eredis_sentinel, start_link, [{Sentinel,SentinelArgs}]},
        permanent, 5000, supervisor, [eredis_sentinel]}),
    PoolArgs1 = [{strategy, fifo}, {name, {local, PoolName}}, {worker_module, eredis} | PoolArgs],
    case ag_database_sup:start_child(poolboy:child_spec(PoolName, PoolArgs1, WorkerArgs)) of
        {ok, _PID} ->
            ok;
        {error, {already_started, _PID}} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%for key%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec keys(term(), binary()) ->
    {ok, [binary()]} | {error, Reason :: term()}.
keys(PoolName, KeyPattern) ->
    q(PoolName, ["KEYS", KeyPattern]).

-spec scan(term(), binary()|integer(), binary()|string(), integer()) ->
    {error, binary()}|{ok, list()}.
scan(PoolName, Cursor, Pattern, Count) ->
    q(PoolName, ["SCAN", Cursor, "MATCH", Pattern, "COUNT", Count]).

%% Delete Key
-spec rename(term(), binary(), binary()) ->
    {ok, atom()} | {error, term()}.
rename(PoolName, OldKey, NewKey) ->
    case q(PoolName, ["RENAME", OldKey, NewKey]) of
        {ok, <<"OK">>} ->
            {ok, renamed};
        {error, Reason} ->
            {error, Reason}
    end.

%% Delete Key
-spec del(term(), binary()) ->
    {ok, integer()} | {error, term()}.
del(PoolName, Key) ->
    case q(PoolName, ["DEL", Key]) of
        {ok, Count} ->
            {ok, binary_to_integer(Count)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Exists Key
-spec exists(term(), binary()) ->
    {ok, atom()} | {error, term()}.
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
-spec expire(term(), binary(), integer()) ->
    {ok, atom()} | {error, term()}.
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
-spec expireat(term(), binary(), integer()) ->
    {ok, atom()} | {error, term()}.
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
-spec ttl(term(), binary()) ->
    {ok, atom() | integer()} | {error, term()}.
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
-spec pttl(term(), binary()) ->
    {ok, term() | integer()} | {error, term()}.
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
-spec get(term(), binary()) ->
    {ok, undefined | binary()} | {error, term()}.
get(PoolName, Key) ->
    case q(PoolName, ["GET", Key]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

-spec mget(term(), [binary()]) ->
    {ok, [binary()]} | {error, term()}.
mget(PoolName, Keys) ->
    case q(PoolName, ["MGET" | Keys]) of
        {ok, Values} ->
            {ok, Values};
        {error, Reason} ->
            {error, Reason}
    end.

%% Set Value
%% Set the string value of a key
-spec set(term(), binary(), binary()) ->
    {ok, atom()} | {error, term()}.
set(PoolName, Key, Value) ->
    case q(PoolName, ["SET", Key, Value]) of
        {ok, _} ->
            {ok, saved};
        {error, Reason} ->
            {error, Reason}
    end.

%% Set Value expire seconds
%% Set the string value of a key
-spec setex(term(), binary(), binary(), integer()) ->
    {ok, atom()} | {error, term()}.
setex(PoolName, Key, Value, Seconds) ->
    case q(PoolName, ["SET", Key, Value, "EX", Seconds]) of
        {ok, _} ->
            {ok, saved};
        {error, Reason} ->
            {error, Reason}
    end.

%% Incr Key
%% Increment the integer value of a key by one
-spec incr(term(), binary()) ->
    {ok, integer()} | {error, term()}.
incr(PoolName, Key) ->
    case q(PoolName, ["INCR", Key]) of
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Incrby Key
%% Increment the integer value of a key by Value
-spec incrby(term(), binary(), integer()) ->
    {ok, integer()} | {error, term()}.
incrby(PoolName, Key, Value) ->
    case q(PoolName, ["INCRBY", Key, Value]) of
        {ok, Count} ->
            {ok, binary_to_integer(Count)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Decr Key
%% Decrement the integer value of a key by one
-spec decr(term(), binary()) ->
    {ok, integer()} | {error, term()}.
decr(PoolName, Key) ->
    case q(PoolName, ["DECR", Key]) of
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Decrby Key
%% Decrement the integer value of a key by Value
-spec decrby(term(), binary(), integer()) ->
    {ok, integer()} | {error, term()}.
decrby(PoolName, Key, Value) ->
    case q(PoolName, ["DECRBY", Key, Value]) of
        {ok, Count} ->
            {ok, binary_to_integer(Count)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Append Key value return value length
-spec append(term(), binary(), binary()) ->
    {ok, integer()} | {error, term()}.
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
-spec hdel(term(), binary(), list() | [binary()]) ->
    {ok, atom()} | {error, term()}.
hdel(PoolName, Key, FieldList) ->
    case q(PoolName, ["HDEL", Key | FieldList]) of
        {ok, _} ->
            {ok, deleted};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Key Filed Exists
%% Get the exists of a hash field
-spec hexists(term(), binary(), binary()) ->
    {ok, atom()} | {error, term()}.
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
-spec hget(term(), binary(), binary()) ->
    {ok, binary()} | {error, term()}.
hget(PoolName, Key, Field) ->
    case q(PoolName, ["HGET", Key, Field]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Key Set Filed Value
%% Set the string value of a hash field
-spec hset(term(), binary(), binary(), binary()) ->
    {ok, atom()} | {error, term()}.
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
-spec hmget(term(), binary(), list() | [binary()]) ->
    {ok, list() | [binary()]} | {error, term()}.
hmget(PoolName, Key, FieldList) ->
    case q(PoolName, ["HMGET", Key | FieldList]) of
        {ok, ValueList} ->
            {ok, ValueList};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Set Multi Key
%% Set multiple hash fields to multiple values
-spec hmset(term(), binary(), list() | [binary()]) ->
    {ok, atom()} | {error, term()}.
hmset(PoolName, Key, FieldValueList) ->
    case q(PoolName, ["HMSET", Key | FieldValueList]) of
        {ok, _} ->
            {ok, saved};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Get All Field Value List
-spec hgetall(term(), binary()) ->
    {ok, list() | [binary()]} | {error, term()}.
hgetall(PoolName, Key) ->
    case q(PoolName, ["HGETALL", Key]) of
        {ok, FieldValueList} ->
            {ok, FieldValueList};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Get All filed's Values
-spec hvals(term(), binary()) ->
    {ok, list() | [binary()]} | {error, term()}.
hvals(PoolName, Key) ->
    case q(PoolName, ["HVALS", Key]) of
        {ok, Values} ->
            {ok, Values};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash Key Incr filed value
%% Increment the integer value of a hash field by the given number
-spec hincrby(term(), binary(), binary(), integer()) ->
    {ok, integer()} | {error, term()}.
hincrby(PoolName, Key, Field, Inc) ->
    case q(PoolName, ["HINCRBY", Key, Field, Inc]) of
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash hey Get All filed list
-spec hkeys(term(), binary()) ->
    {ok, list() | [binary()]} | {error, term()}.
hkeys(PoolName, Key) ->
    case q(PoolName, ["HKEYS", Key]) of
        {ok, Fileds} ->
            {ok, Fileds};
        {error, Reason} ->
            {error, Reason}
    end.

%% Hash hey Get All filed list size
-spec hlen(term(), binary()) ->
    {ok, integer()} | {error, term()}.
hlen(PoolName, Key) ->
    case q(PoolName, ["HKEYS", Key]) of
        {ok, Fileds} ->
            {ok, length(Fileds)};
        {error, Reason} ->
            {error, Reason}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%for list%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns the element at index index in the list stored at key.
%% The index is zero-based, so 0 means the first element, 1 the second element and so on.
%% Get an element from a list by its index
-spec lindex(term(), binary(), integer()) ->
    {ok, binary()} | {error, term()}.
lindex(PoolName, Key, Index) ->
    case q(PoolName, ["LINDEX", Key, Index]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

%% List hey Get list size
-spec llen(term(), binary()) ->
    {ok, integer()} | {error, term()}.
llen(PoolName, Key) ->
    case q(PoolName, ["llen", Key]) of
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Remove and get list first element
-spec lpop(term(), binary()) ->
    {ok, binary()} | {error, term()}.
lpop(PoolName, Key) ->
    case q(PoolName, ["LPOP", Key]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

%% List Push Head and return list's size
%% Prepend one or multiple values to a list
-spec lpush(term(), binary(), list() | [binary()]) ->
    {ok, integer()} | {error, term()}.
lpush(PoolName, Key, ValueList) ->
    case q(PoolName, ["LPUSH", Key | ValueList]) of
        {ok, Count} ->
            {ok, binary_to_integer(Count)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get List Values
%% Get a range of elements from a list
-spec lrange(term(), binary(), integer(), integer()) ->
    {ok, list() | [binary()]} | {error, term()}.
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
-spec lrem(term(), binary(), integer(), integer()) ->
    {ok, integer()} | {error, term()}.
lrem(PoolName, Key, Count, Value) ->
    case q(PoolName, ["LREM", Key, Count, Value]) of
        {ok, DelCount} ->
            {ok, binary_to_integer(DelCount)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Set value by index
-spec lset(term(), binary(), integer(), binary()) ->
    {ok, atom()} | {error, term()}.
lset(PoolName, Key, Index, Value) ->
    case q(PoolName, ["LSET", Key, Index, Value]) of
        {ok, _} ->
            {ok, saved};
        {error, Reason} ->
            {error, Reason}
    end.

%% Trim List
%% Trim a list to the specified range
-spec ltrim(term(), binary(), integer(), integer()) ->
    {ok, atom()} | {error, term()}.
ltrim(PoolName, Key, Start, Stop) ->
    case q(PoolName, ["LTRIM", Key, Start, Stop]) of
        {ok, <<"OK">>} ->
            {ok, deleted};
        {error, Reason} ->
            {error, Reason}
    end.

%% Remove and get list first element
-spec rpop(term(), binary()) ->
    {ok, binary()} | {error, term()}.
rpop(PoolName, Key) ->
    case q(PoolName, ["RPOP", Key]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

%% List Push Tail and return list's size
%% Prepend one or multiple values to a list
-spec rpush(term(), binary(), list() | [binary()]) ->
    {ok, integer()} | {error, term()}.
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
-spec sadd(term(), binary(), list() | [binary()]) ->
    {ok, integer()} | {error, term()}.
sadd(PoolName, Key, Values) ->
    case q(PoolName, ["SADD", Key | Values]) of
        {ok, AddedCount} ->
            {ok, binary_to_integer(AddedCount)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get Size of a Set
-spec scard(term(), binary()) ->
    {ok, integer()} | {error, term()}.
scard(PoolName, Key) ->
    case q(PoolName, ["SCARD", Key]) of
        {ok, Count} ->
            {ok, binary_to_integer(Count)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Remove Values from a set
-spec srem(term(), binary(), list() | [binary()]) ->
    {ok, integer()} | {error, term()}.
srem(PoolName, Key, Values) ->
    case q(PoolName, ["SREM", Key | Values]) of
        {ok, RemovedCount} ->
            {ok, binary_to_integer(RemovedCount)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Is Member of a Set
-spec sismember(term(), binary(), binary()) ->
    {ok, atom()} | {error, term()}.
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
-spec smembers(term(), binary()) ->
    {ok, list() | [binary()]} | {error, term()}.
smembers(PoolName, Key) ->
    case q(PoolName, ["SMEMBERS", Key]) of
        {ok, Values} ->
            {ok, Values};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get Random Values From a Set
-spec srandmember(term(), binary(), integer()) ->
    {ok, list() | [binary()]} | {error, term()}.
srandmember(PoolName, Key, Count) ->
    case q(PoolName, ["SRANDMEMBER", Key, Count]) of
        {ok, undefined} ->
            {ok, []};
        {ok, Values} ->
            {ok, Values};
        {error, Reason} ->
            {error, Reason}
    end.

%% remove and return rand removed element
-spec spop(term(), binary()) ->
    {ok, binary()} | {error, term()}.
spop(PoolName, Key) ->
    case q(PoolName, ["SPOP", Key]) of
        {ok, Value} ->
            {ok, Value};
        {error, Reason} ->
            {error, Reason}
    end.

-spec sunionstore(term(), binary(), binary()|list()) ->
    {ok, binary()}|{error, term()}.
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
-spec zadd(term(), binary(), list()) ->
    {ok, atom()} | {error, term()}.
zadd(PoolName, Key, ScoreValues) ->
    case q(PoolName, ["ZADD", Key | ScoreValues]) of
        {ok, _} ->
            {ok, added};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get Size
%% Get the number of members in a sorted set
-spec zcard(term(), binary()) ->
    {ok, integer()} | {error, term()}.
zcard(PoolName, Key) ->
    case q(PoolName, ["ZCARD", Key]) of
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get Size by special minScore ==> maxScore
%% Get the number of members in a sorted set
-spec zcount(term(), binary(), integer(), integer()) ->
    {ok, integer()} | {error, term()}.
zcount(PoolName, Key, Min, Max) ->
    case q(PoolName, ["ZCOUNT", Key, Min, Max]) of
        {ok, Value} ->
            {ok, binary_to_integer(Value)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Incr Score
%% Increment the score of a member in a sorted set
-spec zincrby(term(), binary(), binary(), integer()) ->
    {ok, integer()} | {error, term()}.
zincrby(PoolName, Key, Value, Increment) ->
    case q(PoolName, ["ZINCRBY", Key, Increment, Value]) of
        {ok, Result} ->
            {ok, binary_to_integer(Result)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Return a range of members in a sorted set, by index  (0 is first member, -1 is last member)
-spec zrange(term(), binary(), integer(), integer()) ->
    {ok, list()} | {error, term()}.
zrange(PoolName, Key, Start, Stop) ->
    case q(PoolName, ["ZRANGE", Key, Start, Stop]) of
        {ok, Members} ->
            {ok, Members};
        {error, Reason} ->
            {error, Reason}
    end.

%% Return a range of members in a sorted set, by index  (0 is first member, -1 is last member)
-spec zrevrange(term(), binary(), integer(), integer()) ->
    {ok, list()} | {error, term()}.
zrevrange(PoolName, Key, Start, Stop) ->
    case q(PoolName, ["ZREVRANGE", Key, Start, Stop]) of
        {ok, Members} ->
            {ok, Members};
        {error, Reason} ->
            {error, Reason}
    end.

%% Return a range of member scores in a sorted set, by index  (0 is first member, -1 is last member)
-spec zrange_withscores(term(), binary(), binary(), integer()) ->
    {ok, list()} | {error, term()}.
zrange_withscores(PoolName, Key, Start, Stop) ->
    case q(PoolName, ["ZRANGE", Key, Start, Stop, "WITHSCORES"]) of
        {ok, MemberScores} ->
            {ok, MemberScores};
        {error, Reason} ->
            {error, Reason}
    end.

%% Return a range of member scores in a sorted set, by index  (0 is first member, -1 is last member)
-spec zrevrange_withscores(term(), binary(), binary(), integer()) ->
    {ok, list()} | {error, term()}.
zrevrange_withscores(PoolName, Key, Start, Stop) ->
    case q(PoolName, ["ZREVRANGE", Key, Start, Stop, "WITHSCORES"]) of
        {ok, MemberScores} ->
            {ok, MemberScores};
        {error, Reason} ->
            {error, Reason}
    end.

%% Return a range of members in a sorted set, by score(ScoreStart -inf, ScoreStop +info) for get all Members
-spec zrangebyscore(term(), binary(), binary(), integer()) ->
    {ok, list()} | {error, term()}.
zrangebyscore(PoolName, Key, ScoreStart, ScoreStop) ->
    case q(PoolName, ["ZRANGEBYSCORE", Key, ScoreStart, ScoreStop]) of
        {ok, Members} ->
            {ok, Members};
        {error, Reason} ->
            {error, Reason}
    end.

%% Return a range of member scores in a sorted set, by score(ScoreStart -inf, ScoreStop +info) for get all Member Scores
-spec zrangebyscore_withscores(term(), binary(), binary(), integer()) ->
    {ok, list()} | {error, term()}.
zrangebyscore_withscores(PoolName, Key, ScoreStart, ScoreStop) ->
    case q(PoolName, ["ZRANGEBYSCORE", Key, ScoreStart, ScoreStop, "WITHSCORES"]) of
        {ok, MemberScores} ->
            {ok, MemberScores};
        {error, Reason} ->
            {error, Reason}
    end.

-spec zrangebyscore_limit(term(), binary(), binary(), integer(), integer(), integer()) ->
    {ok, list()} | {error, term()}.
zrangebyscore_limit(PoolName, Key, ScoreStart, ScoreStop, LimitStart, LimitEnd) ->
    case q(PoolName, ["ZRANGEBYSCORE", Key, ScoreStart, ScoreStop, "LIMIT", LimitStart, LimitEnd]) of
        {ok, MemberScores} ->
            {ok, MemberScores};
        {error, Reason} ->
            {error, Reason}
    end.

%% Sort Set Remove
%% Remove one from a sorted set
-spec zrem(term(), binary(), binary()) ->
    {ok, atom()} | {error, term()}.
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
-spec zrank(term(), binary(), binary()) ->
    {ok, integer()} | {error, term()}.
zrank(PoolName, Key, Value) ->
    case q(PoolName, ["ZRANK", Key, Value]) of
        {ok, undefined} ->
            {ok, 0};
        {ok, Rank} ->
            {ok, binary_to_integer(Rank) + 1};
        {error, Reason} ->
            {error, Reason}
    end.

-spec zrevrank(term(), binary(), binary()) ->
    {ok, integer()} | {error, term()}.
zrevrank(PoolName, Key, Value) ->
    case q(PoolName, ["ZREVRANK", Key, Value]) of
        {ok, undefined} ->
            {ok, 0};
        {ok, Rank} ->
            {ok, binary_to_integer(Rank) + 1};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get Score
%% Get the score associated with the given member in a sorted set
-spec zscore(term(), binary(), binary()) ->
    {ok, integer()} | {error, term()}.
zscore(PoolName, Key, Member) ->
    case q(PoolName, ["ZSCORE", Key, Member]) of
        {ok, undefined} ->
            {ok, 0};
        {ok, Score} ->
            {ok, binary_to_integer(Score)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Has Key
%% Has Key In Sort Set
-spec zismember(term(), binary(), binary()) ->
    {ok, atom()} | {error, term()}.
zismember(PoolName, Key, Member) ->
    case zscore(PoolName, Key, Member) of
        {ok, 0} ->
            {ok, not_exist};
        {ok, _} ->
            {ok, exist};
        {error, Reason} ->
            {error, Reason}
    end.

-spec zunionstore(atom(), binary(), integer(), list()) ->
    {ok, term()}|{error, term()}.
zunionstore(PoolName, DestKey, NumKeys, Keys) ->
    Command = ["ZUNIONSTORE", DestKey, NumKeys] ++ Keys,
    case q(PoolName, Command) of
        {ok, Num} ->
            {ok, Num};
        {error, Reason} ->
            {error, Reason}
    end.

-spec eval(term(), list()) ->
    {ok, binary()} | {error, term()}.
eval(PoolName, Script) ->
    q(PoolName, ["EVAL" | Script]).

-spec evalsha(term(), list()) ->
    {ok, binary()} | {error, term()}.
evalsha(PoolName, Script) ->
    q(PoolName, ["EVALSHA" | Script]).

-spec lrem(term(), binary(), integer()) ->
    {ok, integer()} | {error, term()}.
lrem(PoolName, Key, Value) ->
    q(PoolName, ["LREM", Key, 0, Value]).

-spec script_load(term(), string()) ->
    {ok, binary()} | {error, term()}.
script_load(PoolName, LuaStr) ->
    q(PoolName, ["SCRIPT", "LOAD", LuaStr]).

%% Flush DB
-spec flushdb(term()) ->
    {ok, binary()}.
flushdb(PoolName) ->
    {ok, <<"OK">>} = q(PoolName, [<<"FLUSHDB">>]).

%%--------------------------------------------------------------------
%% @doc
%% Executes the given command in the specified connection. The
%% command must be a valid Redis command and may contain arbitrary
%% data which will be converted to binaries. The returned values will
%% always be binaries.
%% @end
%%--------------------------------------------------------------------
-spec q(PoolName :: atom(), Command :: [any()]) ->
    {ok, return_value()} | {error, Reason :: binary()}.
q(PoolName, Command) ->
    case q(PoolName, Command, ?TIMEOUT) of
        {error, Reason} ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>>agdb_cached_adapter_redis q error:[~p],PoolName:[~p],Command:[~p]",
                [Reason, PoolName, Command]),
            {error, Reason};
        Result ->
            Result
    end.

-spec q(PoolName :: atom(), Command :: [any()], Timeout :: integer()) ->
    {ok, return_value()} | {error, Reason :: binary()}.
q(PoolName, Command, Timeout) ->
    poolboy:transaction(
        PoolName,
        fun(Worker) ->
            eredis:q(Worker, Command, Timeout)
        end
    ).

-spec qp(PoolName :: atom(), Pipeline :: [iolist()]) ->
    [{ok, return_value()} | {error, Reason :: binary()}] | {error, no_connection}.
qp(PoolName, Pipeline) ->
    case qp(PoolName, Pipeline, ?TIMEOUT) of
        {error, Reason} ->
            ?LOG_ERROR(">>>>>>>>>>>>>>>>agdb_cached_adapter_redis qp error:[~p],PoolName:[~p],Pipeline:[~p]",
                [Reason, PoolName, Pipeline]),
            {error, Reason};
        Result ->
            Result
    end.

-spec qp(PoolName :: atom(), Pipeline :: [iolist()], Timeout :: integer()) ->
    [{ok, return_value()} | {error, Reason :: binary()}] | {error, no_connection}.
qp(PoolName, Pipeline, Timeout) ->
    poolboy:transaction(
        PoolName,
        fun(Worker) ->
            eredis:qp(Worker, Pipeline, Timeout)
        end
    ).

-spec transaction(PoolName :: atom(), Fun :: function()) ->
    {ok, binary() | [binary()]} | {error, Reason :: binary()}.
transaction(PoolName, Fun) when is_function(Fun) ->
    F =
        fun(C) ->
            try
                {ok, <<"OK">>} = eredis:q(C, ["MULTI"]),
                Fun(C),
                eredis:q(C, ["EXEC"])
            catch
                Class:Reason ->
                    {ok, <<"OK">>} = eredis:q(C, ["DISCARD"]),
                    ?LOG_ERROR("Error in redis transaction. ~p:~p", [Class, Reason]),
                    {Class, Reason}
            end
        end,
    poolboy:transaction(PoolName, F).
