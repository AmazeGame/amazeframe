%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_authenticate).

-include_lib("ag_base/include/agb_debuglogger.hrl").

-include("ag_engine_core_defines.hrl").
-include("ag_engine.hrl").

-callback validate(PeerInfo :: map(), LocalSetting :: map()) ->
    true | false| time_out | p_no | require_version | {error, ErrCode :: integer()}.
-callback sign(Args :: list()) ->
    binary().
-callback check_peer_time(PeerTime :: integer() | string(), ValidateSeconds :: integer()) ->
    boolean().
-callback check_package_number(PeerInfo :: map(), LocalSetting :: map()) ->
    boolean().

-optional_callbacks([validate/2]).
-optional_callbacks([sign/1]).
-optional_callbacks([check_peer_time/2]).
-optional_callbacks([check_package_number/2]).


%% 由于从1计算，因此计算回滚时 16#FFFFFFFF 等价于 16#00000000

%% API
-export([
    validate/2,
    make_auth_strong/4,
    make_auth_weak/3,
    make_auth_temporary/2,
    check_peer_time/2,
    check_package_number/2,
    sign/1,
    init/0
]).


-spec init() ->
    any().
init() ->
    case application:get_env(auth_driver) of
        undefined ->
            ag_engine_variable:put(sign_driver,ag_engine_inner_authenticate),
            ag_engine_variable:put(peer_time_checker,ag_engine_inner_authenticate),
            ag_engine_variable:put(auth_driver,ag_engine_inner_authenticate),
            ag_engine_variable:put(package_number_dirver,ag_engine_inner_check_packagenumber);
        {ok, Driver} ->
            case agb_behaviour:check_behaviour(Driver, ?MODULE) of
                true ->
                    ValidateDriver = get_validate_driver(Driver),
                    SignDriver = get_sign_driver(Driver),
                    PeerTimeChecker = get_peer_timer_checker(Driver),
                    CheckPackageNumber = get_check_package_number(Driver),
                    ag_engine_variable:put(sign_driver,SignDriver),
                    ag_engine_variable:put(peer_time_checker,PeerTimeChecker),
                    ag_engine_variable:put(auth_driver,ValidateDriver),
                    ag_engine_variable:put(package_number_dirver,CheckPackageNumber);
                false ->
                    false
            end
    end.

get_validate_driver(Driver) ->
    case agb_behaviour:check_functions(Driver, [{validate, 2}]) of
        true -> Driver;
        false -> ag_engine_inner_authenticate
    end.

get_sign_driver(Driver) ->
    case agb_behaviour:check_functions(Driver, [{sign, 1}]) of
        true -> Driver;
        false -> ag_engine_inner_authenticate
    end.

get_peer_timer_checker(Driver) ->
    case agb_behaviour:check_functions(Driver, [{check_peer_time, 2}]) of
        true -> Driver;
        false -> ag_engine_inner_authenticate
    end.

get_check_package_number(Driver) ->
    case agb_behaviour:check_functions(Driver, [{check_package_number, 2}]) of
        true ->
            Driver;
        false ->
            ag_engine_inner_check_packagenumber:init(),
            ag_engine_inner_check_packagenumber
    end.

driver() ->
    ag_engine_variable:getv(auth_driver).

sign_driver() ->
    ag_engine_variable:getv(sign_driver).

peertime_checker() ->
    ag_engine_variable:getv(peer_time_checker).
    
package_number_dirver() ->
    ag_engine_variable:getv(check_package_number).

-spec validate(PeerInfo :: map(), LocalSetting :: map()) ->
    true|false|time_out|require_version|p_no|{error, ErrCode :: integer()}.
validate(PeerInfo, LocalSetting) ->
    Driver = driver(),
    Driver:validate(PeerInfo, LocalSetting).

-spec sign(Args :: list()) ->
    binary().
sign(Args) ->
    SignDriver = sign_driver(),
    SignDriver:sign(Args).

-spec check_peer_time(PeerTime :: integer()|string(), integer()) ->
    boolean().
check_peer_time(PeerTime, ValidateSeconds) ->
    PeerTimeDriver = peertime_checker(),
    PeerTimeDriver:check_peer_time(PeerTime, ValidateSeconds).

-spec check_package_number(PeerInfo, LocalSetting) -> Result when
    PeerInfo :: maps:map(),
    LocalSetting :: maps:map(),
    Result :: true | p_no.
check_package_number(PeerInfo, LocalSetting) ->
    Driver = package_number_dirver(),
    Driver:check_package_number(PeerInfo, LocalSetting).

-spec make_auth_weak(binary(), binary(), Map :: map()) ->
    map().
make_auth_weak(AppId, ClientKey, Map) ->
    Time = integer_to_binary(erlang:system_time(?AUTH_SYSTEM_TYPE)),
    Token = sign([AppId, Time, ClientKey]),
    Map#{?INTERNAL_DEFINE_TIME => Time, ?INTERNAL_DEFINE_SIGN => Token}.

-spec make_auth_strong(binary(), binary(), binary(), Map :: map()) ->
    map().
make_auth_strong(AppId, ClientKey, Security, Map) ->
    Time = integer_to_binary(erlang:system_time(?AUTH_SYSTEM_TYPE)),
    Token = sign([AppId, Time, ClientKey, Security]),
    Map#{?INTERNAL_DEFINE_TIME => Time, ?INTERNAL_DEFINE_SIGN => Token}.

-spec make_auth_temporary(TemporarySecurity :: binary(), Map :: map()) ->
    map().
make_auth_temporary(TemporarySecurity, Map) ->
    Time = integer_to_binary(erlang:system_time(?AUTH_SYSTEM_TYPE)),
    Map#{?INTERNAL_DEFINE_TIME => Time, ?INTERNAL_DEFINE_SIGN => TemporarySecurity}.

