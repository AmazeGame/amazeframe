---
--- Generated by EmmyLua(https://github.com/EmmyLua)
--- Created by Administrator.
--- DateTime: 2019/8/28 14:39
---


local function buildIndexKey(objectName, indexName, indexValue)
    return "{ag_cluster}" .. objectName .. ":" .. indexName .. ":" .. indexValue
end

local function buildKey(objectName, keyField, objKey)
    return "{ag_cluster}" .. objectName .. ":" .. keyField .. ":" .. objKey
end

local function toHashTable(table, startPos)
    local t = {}
    for i = startPos, #(table), 2 do
        --redis.log(redis.LOG_WARNING, "delIndex indexKey:", table[i], " indexHashKey:", table[i + 1])
        t[table[i]] = table[i + 1]
    end
    return t
end

local function printTable(T)
    for i, v in pairs(T) do
        redis.log(redis.LOG_WARNING, "key:", i, " value:", v)
    end
end

local function delIndex(objName, objKey, object, indexs)
    for _, index in ipairs(indexs) do
        local indexKey = buildIndexKey(objName, index, object[index])
        --redis.log(redis.LOG_WARNING, "delIndex value:", objKey, " delIndex:", indexKey)
        redis.call("srem", indexKey, objKey)
        --if tonumber(redis.call("EXISTS", indexKey)) == 0 then
        --    redis.call("srem", nodeKey, indexKey)
        --end
    end
end

local function writeIndex(objName, objKey, object, indexs, ttl)
    for _, index in ipairs(indexs) do
        local indexKey = buildIndexKey(objName, index, object[index])
        --redis.log(redis.LOG_WARNING, "writeIndex:", indexKey, " value:", objKey)
        redis.call("sadd", indexKey, objKey)
        --if nodeKey then
        --    redis.call("sadd", nodeKey, indexKey)
        --end
        if tonumber(ttl) ~= 0 then
            redis.call("Expire", indexKey, tonumber(ttl))
        end
    end
end

local function refreshIndexTTL(objName, object, indexs, ttl)
    --redis.log(redis.LOG_WARNING, "refreshIndexTTL", objName)
    for _, index in ipairs(indexs) do
        local indexKey = buildIndexKey(objName, index, object[index])
        --redis.log(redis.LOG_WARNING, "indexKey", indexKey)
        redis.call("Expire", indexKey, tonumber(ttl))
    end
end

local function writeIndexObject(args)
    local indexs = args.indexs
    local key = buildKey(args.objectName, args.keyField, KEYS[tonumber(args.keyPos)])
    local oldObj = redis.call('hgetall', key)
    if #(oldObj) ~= 0 then
        local oldHashObj = toHashTable(oldObj, 1)
        delIndex(args.objectName, key, oldHashObj, indexs)
    end
    --写主值
    redis.call("hmset", key, unpack(ARGV, 2))
    --redis.call("sadd", args.nodeKey, key)
    if tonumber(args.ttl) ~= 0 then
        redis.call("Expire", key, tonumber(args.ttl))
    end
    --写索引值
    local newHashObj = toHashTable(ARGV, 2)
    writeIndex(args.objectName, key, newHashObj, indexs, args.ttl)
    return "ok"
end

local function writeNotIndexObject(args)
    local key = buildKey(args.objectName, args.keyField, KEYS[tonumber(args.keyPos)])
    redis.call("hmset", key, unpack(ARGV, 2))
    --redis.call("sadd", args.nodeKey, key)
    if args.ttl ~= 0 then
        redis.call("Expire", key, args.ttl)
    end
    return "ok"
end

---- 写入键值
local function write(args)
    if #(args.indexs) > 0 then
        return writeIndexObject(args)
    else
        return writeNotIndexObject(args)
    end
end

-----------------------------------------------------------------------------------------------------------------------
---- 带写入检测的写入键值
local function verification_write(args)
    local checkKey = args.checkKey
    local checkValue = tonumber(args.checkValue)
    local operator = tostring(args.operator)
    local key = buildKey(args.objectName, args.keyField, KEYS[tonumber(args.keyPos)])
    local oldValue = redis.call('hget', key, checkKey)
    if oldValue then
        --redis.log(redis.LOG_WARNING, "oldValue",checkValue,operator,oldValue)
        if operator == "<=" then
            if tonumber(checkValue) <= tonumber(oldValue) then
                return "lockerror"
            end
        end
    end
    if #(args.indexs) > 0 then
        return writeIndexObject(args)
    else
        return writeNotIndexObject(args)
    end
end

---- 根据一个键值读取对象,
--- args.objectName 对象名 args.keyField 字段名 args.key 字段值 args.ttl 生命周期 args.indexs 索引列表
local function readByKey(args)
    local key = buildKey(args.objectName, args.keyField, KEYS[tonumber(args.keyPos)])
    --redis.log(redis.LOG_WARNING, "readByKey", key)
    local object = redis.call('hgetall', key)
    if args.ttl ~= 0 and #(object) > 0 then
        redis.call("Expire", key, args.ttl)
        refreshIndexTTL(args.objectName, toHashTable(object, 1), args.indexs, args.ttl)
    end
    --printTable(object)
    return object
end

---- 通过索引查找数据，
--- args.objectName 对象名 args.keyField 索引字段名 args.key 索引的值
--- args.beginCursor 读取开始的cursor args.count 读取个数，最大10000
--- return result[1] 结果集 result[2] 游标地址
local function readByIndex(args)
    local indexKey = buildIndexKey(args.objectName, args.keyField, KEYS[tonumber(args.keyPos)])
    local cursor = args.beginCursor
    local count = 0
    local result = { {}, 0 }
    --redis.log(redis.LOG_WARNING, "indexKey", indexKey)
    local len = redis.call('SCARD', indexKey)
    if len == 0 then
        return result
    elseif len == 1 then
        local objectKey = redis.call('SMEMBERS', indexKey)
        local object = redis.call('hgetall', objectKey[1])
        if #(object) == 0 then
            return result
        else
            if args.ttl ~= 0 then
                redis.call("Expire", objectKey[1], args.ttl)
                refreshIndexTTL(args.objectName, toHashTable(object, 1), args.indexs, args.ttl)
            end
        end
        result[1][1] = object
    else
        if args.count > 10000 then
            count = 10000
        else
            count = args.count
        end
        local r = redis.call('SSCAN', indexKey, cursor, "COUNT", count)
        for i = 1, #(r[2]) do
            local key = r[2][i]
            --redis.log(redis.LOG_WARNING, "key", key)
            result[1][i] = redis.call('hgetall', key)
        end
        result[2] = cursor
    end
    --printTable(result[1])
    return result
end

---- 更新带索引的对象
---  args.objectName 对象名 args.keyField 键值字段 args.key 键值 args.indexs 索引列表
---  ARGV[2] 更新的字段和值
local function updateByKey(args)
    local key = buildKey(args.objectName, args.keyField, KEYS[tonumber(args.keyPos)])
    local oldObject = redis.call('hgetall', key)
    --redis.log(redis.LOG_WARNING, "key", key,#(oldObject))
    if #(oldObject) == 0 then
        return "read table failed when update"
    end
    redis.call('hmset', key, unpack(ARGV, 2))
    local newObject = redis.call('hgetall', key)
    local oldHashObj = toHashTable(oldObject, 1)
    local newHashObj = toHashTable(newObject, 1)
    delIndex(args.objectName, key, oldHashObj, args.indexs)
    writeIndex(args.objectName, key, newHashObj, args.indexs, args.ttl)
    return "ok"
end

---- 删除逻辑
local function delete(objectName, key, indexs)
    local object = redis.call('hgetall', key)

    redis.call('del', key)
    --redis.call('srem', nodeKey, key)
    --redis.log(redis.LOG_WARNING, "delete key", key, "hashObj", #(hashObj))
    if #(object) > 0 then
        local hashObj = toHashTable(object, 1)
        delIndex(objectName, key, hashObj, indexs)
    end
end

local function deleteByKey(args)
    local key = buildKey(args.objectName, args.keyField, KEYS[tonumber(args.keyPos)])
    delete(args.objectName, key, args.indexs)
    return "ok"
end

--- args.objectName 对象名 args.indexKey 索引字段名 args.indexValue 索引的值
--- return ok
local function deleteByIndex(args)
    local indexKey = buildIndexKey(args.objectName, args.keyField, KEYS[tonumber(args.keyPos)])
    --redis.log(redis.LOG_WARNING, "deleteByIndex", indexKey)
    local keys = redis.call('Smembers', indexKey)
    --printTable(keys)
    for _, v in ipairs(keys) do
        delete(args.objectName, v, args.indexs)
    end
    return "ok"
end

local args = cjson.decode(ARGV[1])
--printTable(KEYS)
KEYS[2] = string.sub(KEYS[2], 13, string.len(KEYS[2]))
if tostring(KEYS[1]) == "{ag_cluster}verification_write" then
    return verification_write(args)
elseif tostring(KEYS[1]) == "{ag_cluster}write" then
    return write(args)
elseif tostring(KEYS[1]) == "{ag_cluster}index_read" then
    return readByIndex(args)
elseif tostring(KEYS[1]) == "{ag_cluster}read" then
    return readByKey(args)
elseif tostring(KEYS[1]) == "{ag_cluster}update" then
    return updateByKey(args)
elseif tostring(KEYS[1]) == "{ag_cluster}delete_by_key" then
    return deleteByKey(args)
elseif tostring(KEYS[1]) == "{ag_cluster}delete_by_index" then
    return deleteByIndex(args)
end


