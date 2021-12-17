%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_inner_check_packagenumber).
-include_lib("ag_base/include/agb_debuglogger.hrl").
-include("ag_engine_core_defines.hrl").
-include("ag_engine.hrl").

-define(MAX_PACKAGE_NUMBER, 16#FFFFFFFF). %% 包号最大值，范围 1~16#FFFFFFFF（unsigned int32），达到该值以后归零重新从1计算。
%% API
-export([init/0]).
-export([reset_package_number/1]).
-export([check_package_number/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec init() ->
    ok | ignore.
init() ->
    case is_open() of
        true ->
            init_database();
        _ ->
            ignore
    end.

%% @doc
%% 检测包号
%% @spec check_package_number(PeerInfo::map(), LocalSetting::map()) -> Bool:: boolean().
%% PeerInfo必须包含key：INTERNAL_DEFINE_PACKAGE_NUMBER LocalSetting必须包含key：INTERNAL_DEFINE_ID
%% @end

-spec check_package_number(PeerInfo, LocalSetting) -> Result when
    PeerInfo :: maps:map(),
    LocalSetting :: maps:map(),
    Result :: true | p_no.
check_package_number(PeerInfo, LocalSetting) ->
    case is_open() of
        true ->
            check_number(PeerInfo, LocalSetting);
        _ ->
            true
    end.

%% @doc 重置包号
-spec reset_package_number(Id) -> ok when
    Id :: binary().
reset_package_number(Id) ->
    case is_open() of
        true ->
            {Pool, _} = get_check_param(),
            Key = cache_key(?INTERNAL_DEFINE_PACKAGE_NUMBER, Id),
            agdb_cached_adapter:set(Pool, Key, 0);
        _ ->
            true
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
is_open() ->
    case application:get_env(ag_engine, check_packagenumber, close) of
        close ->
            false;
        _ ->
            true
    end.

init_database() ->
    {_, #{database :=DataBaseConfig}} = application:get_env(ag_engine, check_packagenumber),
    Pool = proplists:get_value(pools, DataBaseConfig),
    Opts = proplists:get_value(option, DataBaseConfig),
    agdb_manager:add_pool(Pool, redis, Opts).

check_number(#{?INTERNAL_DEFINE_PACKAGE_NUMBER := ClientPackageNumber}, #{?INTERNAL_DEFINE_ID := Id}) ->
    {Pool, MaxDiff} = get_check_param(),
    Key = cache_key(?INTERNAL_DEFINE_PACKAGE_NUMBER, Id),
    %% get PackageNumber
    case agdb_cached_adapter:get(Pool, Key) of
        {ok, undefined} ->
            p_no;
        {error, _Reason} ->
            p_no;
        {ok, NumStr} ->
            %% check PackageNumber
            Bool = do_check_package_number(ClientPackageNumber, binary_to_integer(NumStr), MaxDiff),
            %% write PackageNumber
            case Bool of
                true ->
                    agdb_cached_adapter:set(Pool, Key, ClientPackageNumber),
                    true;
                false ->
                    p_no
            end
    end;
check_number(_, _) ->
    p_no.

get_check_param() ->
    ?LOG_DEBUG("------get_check_param-----~p~n", [application:get_env(ag_engine, check_packagenumber)]),
    {_, #{database :=DataBaseConfig} = Config} = application:get_env(ag_engine, check_packagenumber),
    MaxDiff = maps:get(maxdiff, Config, 1),
    Pool = proplists:get_value(pools, DataBaseConfig),
    {Pool, MaxDiff}.

cache_key(Prefix, Key) ->
    binary:list_to_bin([Prefix, "_", Key]).

%% @private 用户自定义包号检查时，调用这个 API 实现底层检查
%% @doc
%% 校验逻辑:
%% 1. 客户端包号比服务端包号大
%%    两者差值：Diff
%%    1.1 Diff == 1 -> ✅
%%    1.2 Diff > 1 -> ❎
%% 2. 客户端包号比服务端包号小
%%    2.1 发生包号回滚 -> ✅
%%    2.2 没有回滚 -> ❎
%% @end
do_check_package_number(CPNo, SPNo, MaxDiff) when
    is_integer(CPNo) andalso is_integer(SPNo) andalso is_integer(MaxDiff) ->
    Diff = diff_package_number(CPNo, SPNo),
    Diff =< MaxDiff andalso Diff > 0.

%%@private 计算包号差值
diff_package_number(CPNo, SPNo) when CPNo >= SPNo ->
    CPNo - SPNo;
diff_package_number(CPNo, SPNo) ->
    (?MAX_PACKAGE_NUMBER - SPNo) + CPNo.