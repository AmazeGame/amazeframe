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

-module(agdb_cached_adapter).

-callback connect(atom(), tuple()) ->
    ok | {error, Reason :: term()}.

-export([adapter/1]).

-export([init/3]).
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
    flush_db/1,
    keys/2,
    scan/4,
    qp/2,
    q/2
]).

-spec init(atom(), atom(), tuple()) ->
    {ok, module()}|{error, term()}.
init(Driver, DriverPoolName, Opts) ->
    case adapter(DriverPoolName) of
        undefined ->
            Adapter = list_to_atom(lists:concat(["agdb_cached_adapter_", Driver])),
            case lists:member(Adapter, agb_behaviour:get_behaviour_modules(?MODULE)) of
                true ->
                    ok = Adapter:connect(DriverPoolName, Opts),
                    ag_database_variable:put({?MODULE, DriverPoolName}, {Adapter, DriverPoolName}),
                    {ok, Adapter};
                false ->
                    {error, agb_string:sprintf("the db driver:~p is error!~n", [Driver])}
            end;
        {Adapter, _PoolName} ->
            {ok, Adapter}
    end.

-spec adapter(atom()) ->
    undefined | {module(), atom()}.
adapter(DriverPoolName) ->
    case ag_database_variable:geto({?MODULE, DriverPoolName}) of
        undefined ->
            undefined;
        {_, AdapterInfo} ->
            AdapterInfo
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%for key%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec keys(DriverPoolName :: atom(), KeyPattern :: binary()) ->
    {ok, [binary()]} | {error, Reason :: term()}.
keys(DriverPoolName, KeyPattern) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:keys(PoolName, KeyPattern).

-spec scan(DriverPoolName :: atom(), Cursor :: integer(), Pattern :: binary(), Count :: integer()) ->
    {ok, list()}.
scan(DriverPoolName, Cursor, Pattern, Count) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:scan(PoolName, Cursor, Pattern, Count).

%% Delete Key
-spec rename(DriverPoolName :: atom(), OldKey :: binary(), NewKey :: binary()) ->
    {ok, atom()} | {error, term()}.
rename(DriverPoolName, OldKey, NewKey) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:rename(PoolName, OldKey, NewKey).

%% Delete Key
-spec del(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, integer()} | {error, term()}.
del(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:del(PoolName, Key).

%% Exists Key
-spec exists(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, atom()} | {error, term()}.
exists(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:exists(PoolName, Key).

%% Set key Expire time (TIME_IN_SECONDS)
-spec expire(DriverPoolName :: atom(), Key :: binary(), Seconds :: integer()) ->
    {ok, atom()} | {error, term()}.
expire(DriverPoolName, Key, Seconds) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:expire(PoolName, Key, Seconds).

%% Set key expire timestamp (TIME_IN_UNIX_TIMESTAMP)
-spec expireat(atom(), binary(), integer()) ->
    {ok, atom()} | {error, term()}.
expireat(DriverPoolName, Key, TimeStamp) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:expire(PoolName, Key, TimeStamp).

%% get key remain time(TIME_IN_SECONDS)
-spec ttl(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, atom() | integer()} | {error, term()}.
ttl(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:ttl(PoolName, Key).

%% get key remain timestamp(TIME_IN_UNIX_TIMESTAMP)
-spec pttl(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, term() | integer()} | {error, term()}.
pttl(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:pttl(PoolName, Key).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%for string%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Get Value
%% Get the value of a key
-spec get(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, binary()} | {error, term()}.
get(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:get(PoolName, Key).

-spec mget(DriverPoolName :: atom(), Keys :: [binary()]) ->
    {ok, [binary()]} | {error, term()}.
mget(DriverPoolName, Keys) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:mget(PoolName, Keys).

%% Set Value
%% Set the string value of a key
-spec set(DriverPoolName :: atom(), Key :: binary(), Value :: binary()) ->
    {ok, atom()} | {error, term()}.
set(DriverPoolName, Key, Value) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:set(PoolName, Key, Value).

%% Set Value expire seconds
%% Set the string value of a key
-spec setex(DriverPoolName :: atom(), Key :: binary(), Value :: binary(), Seconds :: integer()) ->
    {ok, atom()} | {error, term()}.
setex(DriverPoolName, Key, Value, Seconds) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:setex(PoolName, Key, Value, Seconds).

%% Incr Key
%% Increment the integer value of a key by one
-spec incr(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, integer()} | {error, term()}.
incr(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:incr(PoolName, Key).

%% Incrby Key
%% Increment the integer value of a key by Value
-spec incrby(DriverPoolName :: atom(), Key :: binary(), Value :: integer()) ->
    {ok, integer()} | {error, term()}.
incrby(DriverPoolName, Key, Value) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:incrby(PoolName, Key, Value).

%% Decr Key
%% Decrement the integer value of a key by one
-spec decr(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, integer()} | {error, term()}.
decr(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:decr(PoolName, Key).

%% Decrby Key
%% Decrement the integer value of a key by Value
-spec decrby(DriverPoolName :: atom(), Key :: binary(), Value :: integer()) ->
    {ok, integer()} | {error, term()}.
decrby(DriverPoolName, Key, Value) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:decrby(PoolName, Key, Value).

%% Append Key value return value length
-spec append(DriverPoolName :: atom(), Key :: binary(), Value :: binary()) ->
    {ok, integer()} | {error, term()}.
append(DriverPoolName, Key, Value) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:append(PoolName, Key, Value).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%for hash%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hash Delete one or more hash fields
%% Removes the specified fields from the hash stored at key
-spec hdel(DriverPoolName :: atom(), Key :: binary(), FieldList :: list() | [binary()]) ->
    {ok, atom()} | {error, term()}.
hdel(DriverPoolName, Key, FieldList) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:hdel(PoolName, Key, FieldList).

%% Hash Key Filed Exists
%% Get the exists of a hash field
-spec hexists(DriverPoolName :: atom(), Key :: binary(), Field :: binary()) ->
    {ok, not_exist|exist} | {error, term()}.
hexists(DriverPoolName, Key, Field) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:hexists(PoolName, Key, Field).

%% Hash Key Get Filed
%% Get the value of a hash field
-spec hget(DriverPoolName :: atom(), Key :: binary(), Field :: binary()) ->
    {ok, binary()} | {error, term()}.
hget(DriverPoolName, Key, Field) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:hget(PoolName, Key, Field).

%% Hash Key Set Filed Value
%% Set the string value of a hash field
-spec hset(DriverPoolName :: atom(), Key :: binary(), Field :: binary(), Value :: binary()) ->
    {ok, atom()} | {error, term()}.
hset(DriverPoolName, Key, Field, Value) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:hset(PoolName, Key, Field, Value).

%% Hash Get Multi Key
%% Get the values of all the given hash fields
-spec hmget(DriverPoolName :: atom(), Key :: binary(), FieldList :: list() | [binary()]) ->
    {ok, list() | [binary()]} | {error, term()}.
hmget(DriverPoolName, Key, FieldList) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:hmget(PoolName, Key, FieldList).

%% Hash Set Multi Key
%% Set multiple hash fields to multiple values
-spec hmset(DriverPoolName :: atom(), Key :: binary(), FieldValueList :: list() | [binary()]) ->
    {ok, atom()} | {error, term()}.
hmset(DriverPoolName, Key, FieldValueList) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:hmset(PoolName, Key, FieldValueList).

%% Hash Get All Field Value List
-spec hgetall(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, list() | [binary()]} | {error, term()}.
hgetall(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:hgetall(PoolName, Key).

%% Hash Get All filed's Values
-spec hvals(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, list() | [binary()]} | {error, term()}.
hvals(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:hvals(PoolName, Key).

%% Hash Key Incr filed value
%% Increment the integer value of a hash field by the given number
-spec hincrby(DriverPoolName :: atom(), Key :: binary(), Field :: binary(), Inc :: integer()) ->
    {ok, integer()} | {error, term()}.
hincrby(DriverPoolName, Key, Field, Inc) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:hincrby(PoolName, Key, Field, Inc).

%% Hash hey Get All filed list
-spec hkeys(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, list() | [binary()]} | {error, term()}.
hkeys(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:hkeys(PoolName, Key).

%% Hash hey Get All filed list size
-spec hlen(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, integer()} | {error, term()}.
hlen(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:hlen(PoolName, Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%for list%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns the element at index index in the list stored at key.
%% The index is zero-based, so 0 means the first element, 1 the second element and so on.
%% Get an element from a list by its index
-spec lindex(DriverPoolName :: atom(), Key :: binary(), Index :: integer()) ->
    {ok, binary()} | {error, term()}.
lindex(DriverPoolName, Key, Index) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:lindex(PoolName, Key, Index).

%% List hey Get list size
-spec llen(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, integer()} | {error, term()}.
llen(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:llen(PoolName, Key).

%% Remove and get list first element
-spec lpop(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, binary()} | {error, term()}.
lpop(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:lpop(PoolName, Key).

%% List Push Head and return list's size
%% Prepend one or multiple values to a list
-spec lpush(DriverPoolName :: atom(), Key :: binary(), ValueList :: list() | [binary()]) ->
    {ok, integer()} | {error, term()}.
lpush(DriverPoolName, Key, ValueList) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:lpush(PoolName, Key, ValueList).

%% Get List Values
%% Get a range of elements from a list
-spec lrange(DriverPoolName :: atom(), Key :: binary(), Start :: integer(), Stop :: integer()) ->
    {ok, list() | [binary()]} | {error, term()}.
lrange(DriverPoolName, Key, Start, Stop) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:lrange(PoolName, Key, Start, Stop).

%% Remove elements from a list
%% Removes the first count occurrences of elements equal to value from the list stored at key.
%% The count argument influences the operation in the following ways:
% count > 0: Remove elements equal to value moving from head to tail.
% count < 0: Remove elements equal to value moving from tail to head.
% count = 0: Remove all elements equal to value.
-spec lrem(DriverPoolName :: atom(), Key :: binary(), Count :: integer(), Value :: integer()) ->
    {ok, integer()} | {error, term()}.
lrem(DriverPoolName, Key, Count, Value) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:lrem(PoolName, Key, Count, Value).

%% Set value by index
-spec lset(DriverPoolName :: atom(), Key :: binary(), Index :: integer(), Value :: binary()) ->
    {ok, atom()} | {error, term()}.
lset(DriverPoolName, Key, Index, Value) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:lset(PoolName, Key, Index, Value).

%% Trim List
%% Trim a list to the specified range
-spec ltrim(DriverPoolName :: atom(), Key :: binary(), Start :: integer(), Stop :: integer()) ->
    {ok, atom()} | {error, term()}.
ltrim(DriverPoolName, Key, Start, Stop) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:ltrim(PoolName, Key, Start, Stop).

%% Remove and get list first element
-spec rpop(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, binary()} | {error, term()}.
rpop(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:rpop(PoolName, Key).

%% List Push Tail and return list's size
%% Prepend one or multiple values to a list
-spec rpush(DriverPoolName :: atom(), Key :: binary(), ValueList :: list() | [binary()]) ->
    {ok, integer()} | {error, term()}.
rpush(DriverPoolName, Key, ValueList) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:rpush(PoolName, Key, ValueList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%for set%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add Values to set
%% Add one or more members to a set
-spec sadd(DriverPoolName :: atom(), Key :: binary(), Values :: list() | [binary()]) ->
    {ok, integer()} | {error, term()}.
sadd(DriverPoolName, Key, Values) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:sadd(PoolName, Key, Values).

%% Get Size of a Set
-spec scard(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, integer()} | {error, term()}.
scard(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:scard(PoolName, Key).

%% Remove Values from a set
-spec srem(DriverPoolName :: atom(), Key :: binary(), Values :: list() | [binary()]) ->
    {ok, integer()} | {error, term()}.
srem(DriverPoolName, Key, Values) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:srem(PoolName, Key, Values).

%% Is Member of a Set
-spec sismember(DriverPoolName :: atom(), Key :: binary(), Value :: binary()) ->
    {ok, atom()} | {error, term()}.
sismember(DriverPoolName, Key, Value) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:sismember(PoolName, Key, Value).

%% Get All Values From a Set
-spec smembers(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, list() | [binary()]} | {error, term()}.
smembers(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:smembers(PoolName, Key).

%% Get Random Values From a Set
-spec srandmember(DriverPoolName :: atom(), Key :: binary(), Count :: integer()) ->
    {ok, list() | [binary()]} | {error, term()}.
srandmember(DriverPoolName, Key, Count) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:srandmember(PoolName, Key, Count).

%% remove and return rand removed element
-spec spop(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, binary()} | {error, term()}.
spop(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:spop(PoolName, Key).

-spec sunionstore(DriverPoolName :: atom(), Destination :: binary(), Key :: binary()|list()) ->
    {ok, binary()}|{error, term()}.
sunionstore(DriverPoolName, Destination, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:sunionstore(PoolName, Destination, Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%for sorted set%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sort Set Add
%% Add one to a sorted set, or update its score if it already exists
-spec zadd(DriverPoolName :: atom(), Key :: binary(), ScoreValues :: list()) ->
    {ok, atom()} | {error, term()}.
zadd(DriverPoolName, Key, ScoreValues) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zadd(PoolName, Key, ScoreValues).

%% Get Size
%% Get the number of members in a sorted set
-spec zcard(DriverPoolName :: atom(), Key :: binary()) ->
    {ok, integer()} | {error, term()}.
zcard(DriverPoolName, Key) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zcard(PoolName, Key).

%% Get Size by special minScore ==> maxScore
%% Get the number of members in a sorted set
-spec zcount(DriverPoolName :: atom(), Key :: binary(), Min :: integer(), Max :: integer()) ->
    {ok, integer()} | {error, term()}.
zcount(DriverPoolName, Key, Min, Max) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zcount(PoolName, Key, Min, Max).

%% Incr Score
%% Increment the score of a member in a sorted set
-spec zincrby(DriverPoolName :: atom(), Key :: binary(), Value :: binary(), Increment :: integer()) ->
    {ok, integer()} | {error, term()}.
zincrby(DriverPoolName, Key, Value, Increment) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zincrby(PoolName, Key, Value, Increment).

%% Return a range of members in a sorted set, by index  (0 is first member, -1 is last member)
-spec zrange(atom(), binary(), integer(), integer()) ->
    {ok, list()} | {error, term()}.
zrange(DriverPoolName, Key, Start, Stop) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zrange(PoolName, Key, Start, Stop).

%% Return a range of members in a sorted set, by index  (0 is first member, -1 is last member)
-spec zrevrange(atom(), binary(), integer(), integer()) ->
    {ok, list()} | {error, term()}.
zrevrange(DriverPoolName, Key, Start, Stop) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zrevrange(PoolName, Key, Start, Stop).

%% Return a range of member scores in a sorted set, by index  (0 is first member, -1 is last member)
-spec zrange_withscores(DriverPoolName :: atom(), Key :: binary(), Start :: binary(), Stop :: integer()) ->
    {ok, list()} | {error, term()}.
zrange_withscores(DriverPoolName, Key, Start, Stop) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zrange_withscores(PoolName, Key, Start, Stop).

%% Return a range of member scores in a sorted set, by index  (0 is first member, -1 is last member)
-spec zrevrange_withscores(DriverPoolName :: atom(), Key :: binary(), Start :: binary(), Stop :: integer()) ->
    {ok, list()} | {error, term()}.
zrevrange_withscores(DriverPoolName, Key, Start, Stop) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zrevrange_withscores(PoolName, Key, Start, Stop).

%% Return a range of members in a sorted set, by score(ScoreStart -inf, ScoreStop +info) for get all Members
-spec zrangebyscore(DriverPoolName :: atom(), Key :: binary(), ScoreStart :: binary(), ScoreStop :: integer()) ->
    {ok, list()} | {error, term()}.
zrangebyscore(DriverPoolName, Key, ScoreStart, ScoreStop) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zrangebyscore(PoolName, Key, ScoreStart, ScoreStop).

%% Return a range of member scores in a sorted set, by score(ScoreStart -inf, ScoreStop +info) for get all Member Scores
-spec zrangebyscore_withscores(DriverPoolName :: atom(), Key :: binary(), ScoreStart :: binary(),
    ScoreStop :: integer()) ->
    {ok, list()} | {error, term()}.
zrangebyscore_withscores(DriverPoolName, Key, ScoreStart, ScoreStop) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zrangebyscore_withscores(PoolName, Key, ScoreStart, ScoreStop).

%% Return a range of member scores in a sorted set, by score(ScoreStart -inf, ScoreStop +info) for get all Member Scores
-spec zrangebyscore_limit(DriverPoolName :: atom(), Key :: binary(), ScoreStart :: binary(), ScoreStop :: integer(),
    LimitStart :: integer(), LimitEnd :: integer()) ->
    {ok, list()} | {error, term()}.
zrangebyscore_limit(DriverPoolName, Key, ScoreStart, ScoreStop, LimitStart, LimitEnd) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zrangebyscore_limit(PoolName, Key, ScoreStart, ScoreStop, LimitStart, LimitEnd).

%% Sort Set Remove
%% Remove one from a sorted set
-spec zrem(DriverPoolName :: atom(), Key :: binary(), Value :: binary()) ->
    {ok, atom()} | {error, term()}.
zrem(DriverPoolName, Key, Value) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zrem(PoolName, Key, Value).

%% Get Rank
%% Determine the index of a member in a sorted set
-spec zrank(DriverPoolName :: atom(), Key :: binary(), Value :: binary()) ->
    {ok, integer()} | {error, term()}.
zrank(DriverPoolName, Key, Value) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zrank(PoolName, Key, Value).

-spec zrevrank(DriverPoolName :: atom(), Key :: binary(), Value :: binary()) ->
    {ok, integer()} | {error, term()}.
zrevrank(DriverPoolName, Key, Value) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zrevrank(PoolName, Key, Value).

%% Get Score
%% Get the score associated with the given member in a sorted set
-spec zscore(DriverPoolName :: atom(), Key :: binary(), Member :: binary()) ->
    {ok, integer()} | {error, term()}.
zscore(DriverPoolName, Key, Member) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zscore(PoolName, Key, Member).

%% Has Key
%% Has Key In Sort Set
-spec zismember(DriverPoolName :: atom(), Key :: binary(), Member :: binary()) ->
    {ok, atom()} | {error, term()}.
zismember(DriverPoolName, Key, Member) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:zismember(PoolName, Key, Member).

%% Flush DB
-spec flush_db(DriverPoolName :: atom()) ->
    {ok, binary()}.
flush_db(DriverPoolName) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:flushdb(PoolName).

-spec qp(DriverPoolName :: atom(), Pipeline :: [iolist()]) ->
    any().
qp(DriverPoolName, Pipeline) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:qp(PoolName, Pipeline).

-spec q(DriverPoolName :: atom(), Command :: [any()]) ->
    any().
q(DriverPoolName, Command) ->
    {Adapter, PoolName} = adapter(DriverPoolName),
    Adapter:q(PoolName, Command).