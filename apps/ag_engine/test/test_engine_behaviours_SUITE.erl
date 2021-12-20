%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(test_engine_behaviours_SUITE).


%% API
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    test_archive/1,
    test_authenticate/1,
    test_authenticate_0/1,
    test_authenticate_1/1,
    test_platfrom/1,
    test_message_wrap_code/1,
    test_protocol_codec/1,
    test_Filters/1
]).

all() ->
    [
        test_archive,
        test_authenticate,
        test_authenticate_0,
        test_authenticate_1,
        test_platfrom,
        test_message_wrap_code,
        test_protocol_codec
    ].

init_per_suite(Config) ->
    logger:set_primary_config(level, all),
    test_utils:load_config(Config),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(test_archive, Config) ->
    start_applications(),
    Config;
init_per_testcase(test_authenticate, Config) ->
    application:set_env(ag_engine, check_packagenumber,
        #{
            database =>     %% 保存包号的db，现只支持redis
            [
                {pools, packagenumber_redis_pool},
                %{driver, redis_cluster},
                {driver, redis},
                {option,
                    {
                        [{size, 5}, {max_overflow, 10}],
                        [{host, "10.0.115.216"}, {port, 6379}, {database, 15}, {password, ""}, {reconnect_sleep, 100}]
                    }
                }
            ],
            maxdiff => 10   %% 比较包号时最大的间隔,默认为1
        }),
    start_applications(),
    Config;
init_per_testcase(test_authenticate_0, Config) ->
    application:set_env(ag_engine, auth_driver, test_authenticate),
    start_applications(),
    Config;
init_per_testcase(test_authenticate_1, Config) ->
    application:set_env(ag_engine, auth_driver, test_authenticate2),
    start_applications(),
    Config;
init_per_testcase(test_protocol_codec, Config) ->
    start_applications(),
    Config;
init_per_testcase(_Case, Config) ->
    start_applications(),
    Config.

start_applications() ->
    agb_application:check_started(ag_engine_cluster),
    agb_application:check_started(ag_idcreator),
    agb_application:check_started(ag_engine),
    ok.

end_per_testcase(test_archive, Config) ->
    stop_applications(),
    Config;
end_per_testcase(test_authenticate, _Config) ->
    agdb_cached_adapter_redis:flushdb(packagenumber_redis_pool),
    stop_applications(),
    ok;
end_per_testcase(test_authenticate_0, _Config) ->
    stop_applications(),
    ok;
end_per_testcase(test_authenticate_1, _Config) ->
    stop_applications(),
    ok;
end_per_testcase(test_protocol_codec, _Config) ->
    meck:unload(),
    stop_applications(),
    ok;
end_per_testcase(_Case, _Config) ->
    stop_applications(),
    ok.

stop_applications() ->
    agdb_cached_adapter_redis:flushdb(cluster_redis_pool),
    application:stop(ag_engine_cluster),
    application:stop(ag_idcreator),
    application:stop(ag_engine).

test_archive(_Config) ->
    ag_engine_archive:init(),
    ?assertEqual({"22345678", "12345678"}, ag_engine_archive:create_user(content)),
    ?assertEqual({"22345678", "12345678"}, ag_engine_archive:load_user(content)).

test_authenticate(_Config) ->
    agb_ets:put('ets_ts_game_authenticate', sign_driver, ag_engine_inner_authenticate),
    agb_ets:put('ets_ts_game_authenticate', peer_time_checker, ag_engine_inner_authenticate),
    agb_ets:put('ets_ts_game_authenticate', auth_driver, ag_engine_inner_authenticate),
    agb_ets:put('ets_ts_game_authenticate', package_number_dirver, ag_engine_inner_check_packagenumber),
    {_,CheckPackageNumberConfig} = application:get_env(ag_engine, check_packagenumber),
    application:set_env(ag_engine, check_packagenumber, close),
    AppId = <<"adfadfad">>,
    ClientKey = <<"123">>,
    Security = <<"1234">>,
    UserId = <<"abcde">>,
    ct:pal("auth_driver:~p", [agb_ets:get('ets_ts_game_authenticate', auth_driver)]),
    PeerInfo = #{?INTERNAL_DEFINE_APP_ID => AppId},
    LocalSetting = #{?INTERNAL_DEFINE_APP_ID => <<"wre">>, ?INTERNAL_DEFINE_MSG_VALIDATE_SECONDS => 60, ?INTERNAL_DEFINE_CLIENT_VER =><<"1.0.0">>},
    ?assertEqual(false, ag_engine_authenticate:validate(PeerInfo, LocalSetting)),

    PeerInfo1 = #{
        ?INTERNAL_DEFINE_APP_ID =>AppId,
        ?INTERNAL_DEFINE_TIME=>integer_to_binary(erlang:system_time(seconds) + 70)
    },
    LocalSetting1 = #{?INTERNAL_DEFINE_APP_ID => AppId, ?INTERNAL_DEFINE_MSG_VALIDATE_SECONDS => 60, ?INTERNAL_DEFINE_CLIENT_VER =><<"1.0.0">>},
    ?assertEqual(time_out, ag_engine_authenticate:validate(PeerInfo1, LocalSetting1)),

    PeerInfo2 = #{
        ?INTERNAL_DEFINE_APP_ID => AppId,
        ?INTERNAL_DEFINE_CLIENT_VER =><<"1.0.0">>,
        ?INTERNAL_DEFINE_TIME=>integer_to_binary(erlang:system_time(seconds))

    },
    LocalSetting2 = #{?INTERNAL_DEFINE_APP_ID => AppId, ?INTERNAL_DEFINE_CLIENT_VER => <<"1.2.0">>, ?INTERNAL_DEFINE_MSG_VALIDATE_SECONDS => 60},
    ?assertEqual(require_version, ag_engine_authenticate:validate(PeerInfo2, LocalSetting2)),

    PeerTime = integer_to_binary(erlang:system_time(seconds)),
    ErrorSign = ag_engine_authenticate:sign([<<"12345">>, PeerTime, ClientKey]),
    PeerInfo3 = #{
        ?INTERNAL_DEFINE_APP_ID => AppId,
        ?INTERNAL_DEFINE_CLIENT_VER =><<"1.0.0">>,
        ?INTERNAL_DEFINE_TIME=>PeerTime,
        ?INTERNAL_DEFINE_SIGN => ErrorSign
    },
    LocalSetting3 = #{?INTERNAL_DEFINE_APP_ID => AppId, ?INTERNAL_DEFINE_CLIENT_VER =><<"1.0.0">>, ?INTERNAL_DEFINE_CLIENT_KEY => ClientKey, ?INTERNAL_DEFINE_MSG_VALIDATE_SECONDS => 60},
    ?assertEqual(false, ag_engine_authenticate:validate(PeerInfo3, LocalSetting3)),


    Sign = ag_engine_authenticate:sign([AppId, PeerTime, ClientKey]),
    PeerInfo3_true = #{
        ?INTERNAL_DEFINE_APP_ID => AppId,
        ?INTERNAL_DEFINE_CLIENT_VER =><<"1.0.0">>,
        ?INTERNAL_DEFINE_TIME=>PeerTime,
        ?INTERNAL_DEFINE_SIGN => Sign
    },
    LocalSetting3_true =
        #{
            ?INTERNAL_DEFINE_APP_ID => AppId,
            ?INTERNAL_DEFINE_CLIENT_VER =><<"1.0.0">>,
            ?INTERNAL_DEFINE_CLIENT_KEY => ClientKey,
            ?INTERNAL_DEFINE_MSG_VALIDATE_SECONDS => 60
        },
    ?assertEqual(true, ag_engine_authenticate:validate(PeerInfo3_true, LocalSetting3_true)),

    ErrorSign1 = ag_engine_authenticate:sign([AppId, PeerTime, ClientKey, <<"123">>]),
    PeerInfo4 = #{
        ?INTERNAL_DEFINE_APP_ID => AppId,
        ?INTERNAL_DEFINE_CLIENT_VER =><<"1.0.0">>,
        ?INTERNAL_DEFINE_TIME=>PeerTime,
        ?INTERNAL_DEFINE_SIGN => ErrorSign1
    },
    LocalSetting4 = #{
        ?INTERNAL_DEFINE_APP_ID => AppId,
        ?INTERNAL_DEFINE_CLIENT_VER =><<"1.0.0">>,
        ?INTERNAL_DEFINE_CLIENT_KEY => ClientKey,
        ?INTERNAL_DEFINE_SECURITY => Security,
        ?INTERNAL_DEFINE_MSG_VALIDATE_SECONDS => 60},
    ?assertEqual(false, ag_engine_authenticate:validate(PeerInfo4, LocalSetting4)),
    application:set_env(ag_engine, check_packagenumber, CheckPackageNumberConfig),
    PeerInfo5 = #{
        ?INTERNAL_DEFINE_APP_ID => AppId,
        ?INTERNAL_DEFINE_CLIENT_VER =><<"1.0.0">>,
        ?INTERNAL_DEFINE_TIME=>PeerTime,
        ?INTERNAL_DEFINE_SIGN => ag_engine_authenticate:sign([AppId, PeerTime, ClientKey, Security]),
        ?INTERNAL_DEFINE_PACKAGE_NUMBER => <<"0">>
    },
    LocalSetting5 = #{
        ?INTERNAL_DEFINE_APP_ID => AppId,
        ?INTERNAL_DEFINE_CLIENT_VER =><<"1.0.0">>,
        ?INTERNAL_DEFINE_CLIENT_KEY => ClientKey,
        ?INTERNAL_DEFINE_SECURITY => Security,
        ?INTERNAL_DEFINE_ID => UserId,
        ?INTERNAL_DEFINE_MSG_VALIDATE_SECONDS => 60},
    ?assertEqual(p_no, ag_engine_authenticate:validate(PeerInfo5, LocalSetting5)),

    PeerInfo5_true = #{
        ?INTERNAL_DEFINE_APP_ID => AppId,
        ?INTERNAL_DEFINE_CLIENT_VER =><<"1.0.0">>,
        ?INTERNAL_DEFINE_TIME=>PeerTime,
        ?INTERNAL_DEFINE_SIGN => ag_engine_authenticate:sign([AppId, PeerTime, ClientKey, Security]),
        ?INTERNAL_DEFINE_PACKAGE_NUMBER => <<"1">>
    },
    LocalSetting5_true = #{
        ?INTERNAL_DEFINE_APP_ID => AppId,
        ?INTERNAL_DEFINE_CLIENT_VER =><<"1.0.0">>,
        ?INTERNAL_DEFINE_CLIENT_KEY => ClientKey,
        ?INTERNAL_DEFINE_SECURITY => Security,
        ?INTERNAL_DEFINE_ID => UserId,
        ?INTERNAL_DEFINE_MSG_VALIDATE_SECONDS => 60},

    ?assertEqual(p_no, ag_engine_authenticate:validate(PeerInfo5_true, LocalSetting5_true)),
    ok.

test_authenticate_0(_Config) ->
    ct:pal("auth_driver:~p", [agb_ets:get('ets_ts_game_authenticate', auth_driver)]),
    ?assertEqual(true, ag_engine_authenticate:validate(p, l)),
    ?assertEqual(111111, ag_engine_authenticate:sign([<<"10101">>, integer_to_binary(erlang:system_time(seconds)), <<"thisisclienkey">>])),
    ?assertEqual(true, ag_engine_authenticate:check_peer_time(integer_to_binary(erlang:system_time(seconds)), 60)).

test_authenticate_1(_Config) ->
    ct:pal("auth_driver:~p", [agb_ets:get('ets_ts_game_authenticate', auth_driver)]),
    ?assertEqual(true, ag_engine_authenticate:validate(p, l)),
    ?assertNotException(_, _, ag_engine_authenticate:sign([<<"10101">>, integer_to_binary(erlang:system_time(seconds)), <<"thisisclienkey">>])),
    ?assertNotException(_, _, ag_engine_authenticate:sign([<<"10101">>, integer_to_binary(erlang:system_time(seconds)), <<"thisisclienkey">>, <<"Security">>])),
    ?assertNotException(_, _, ag_engine_authenticate:make_auth_weak(<<"10101">>, <<"thisisclienkey">>, #{})),
    ag_engine_storage:set_security(<<"Security">>),
    ?assertNotException(_, _, ag_engine_authenticate:make_auth_strong(<<"10101">>, <<"thisisclienkey">>, <<"Security">>, #{})),
    ?assertNotException(_, _, ag_engine_authenticate:make_auth_temporary(<<"temporary">>, #{})),
    ?assertEqual(true, ag_engine_authenticate:check_peer_time(integer_to_binary(erlang:system_time(seconds)), 60)),
    ?assertEqual(false, ag_engine_authenticate:check_peer_time(integer_to_binary(erlang:system_time(seconds) + 70), 60)),
    ?assertEqual(false, ag_engine_authenticate:check_peer_time(integer_to_binary(erlang:system_time(seconds) - 70), 60)).

test_platfrom(_Config) ->
    ag_engine_platform:init(),
    ?assertEqual(ok, ag_engine_platform:validate(<<"fb">>, test)),
    ?assertEqual(<<"abcde@facebook">>, ag_engine_platform:transform_id(<<"fb">>, <<"abcde">>)),
    ?assertEqual(<<"abcde">>, ag_engine_platform:restore_id(<<"fb">>, <<"abcde@facebook">>)),
    ?assertEqual({error, {<<"email">>, no_auth_type}}, ag_engine_platform:validate(<<"email">>, test)).

test_message_wrap_code(_Config) ->
    ag_engine_message_wrap_code:init(),
    ?assertEqual(#{<<"code">>=>100, <<"info">>=><<"100">>}, ag_engine_message_wrap_code:pack_info(#{<<"code">>=>100})),
    ?assertEqual(#{<<"code">>=>200, <<"info">>=><<"200">>}, ag_engine_message_wrap_code:pack_info(#{<<"code">>=>200})).

test_protocol_codec(_Config) ->
    meck:new(ag_engine_gpb_codec, [unstick, passthrough]),
    meck:expect(ag_engine_gpb_codec, init, 0, fun meck_ag_engine_protocol_codec_init/0),
    ag_engine_protocol_codec:init(),
    MsgList = [#{<<"a">> => <<"abcde">>, <<"b">>=>12345}, #{<<"c">>=><<"efght">>, <<"d">>=>6789}],
    Msg = #{<<"a">> => <<"abcde">>, <<"b">>=>12345},
    Fun = fun(M) -> M end,
    MsgBinary1 = iolist_to_binary(ag_engine_protocol_codec:encode("jsx_json", MsgList, [Fun])),
    MsgBinary2 = iolist_to_binary(ag_engine_protocol_codec:encode(<<"json">>, MsgList, [{?MODULE, test_Filters}])),
    MsgBinary4 = iolist_to_binary(ag_engine_protocol_codec:encode("jsx_json", Msg, [])),
    ?assertEqual(MsgList, ag_engine_protocol_codec:decode("jsx_json", true, MsgBinary1, [])),
    ?assertEqual(MsgList, ag_engine_protocol_codec:decode(<<"json">>, true, MsgBinary2, [])),
    ?assertEqual(Msg, ag_engine_protocol_codec:decode("jsx_json", false, MsgBinary4, [])),
    ?assertEqual(Msg, ag_engine_protocol_codec:decode(MsgBinary4)).

test_Filters(Msg) ->
    Msg.

meck_ag_engine_protocol_codec_init() ->
    String = base_message(string),
    {ok, BaseMessagePb, CodeBinary} = gpb_compile:string(base_message_pb, String, [binary]),
    load_code(BaseMessagePb, CodeBinary),
    agb_ets:init('ag_engine_protocol_codec'),
    scan_pb_module().

load_code(Mod, Code) ->
    unload_code(Mod),
    {module, Mod} = code:load_binary(Mod, "<nofile>", Code),
    ok.

unload_code(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod),
    code:delete(Mod),
    ok.
base_message(_) ->
    agb_string:sprintf("message base_message {\n\tstring ~s = 1;\n}\n", [?MESSAGE_NAME_KEY]).

scan_pb_module() ->
    Modules = agb_behaviour:get_function_modules("*_pb.beam", [{encode_msg, 2}, {decode_msg, 2}, {get_msg_names, 0}, {get_msg_defs, 0}]),
    lists:foreach(
        fun(Module) ->
            Names = Module:get_msg_defs(),
            lists:foreach(
                fun
                    ({{msg, Name}, List}) ->
                        %%agb_ets:put(table(), Name, {Name, Module});
                        case agb_maplist:keyfind(name, name, List) of
                            false -> ignore;
                            TMap ->
                                Opts = maps:get(opts, TMap),
                                case lists:keyfind(default, 1, Opts) of
                                    false ->
                                        ignore;
                                    {_, Default} when is_list(Default) ->
                                        agb_ets:put('ag_engine_protocol_codec', list_to_atom(Default), {Name, Module});
                                    {_, Default} when is_integer(Default) ->
                                        agb_ets:put('ag_engine_protocol_codec', Default, {Name, Module})
                                end
                        end;
                    ({{enum, _}, _}) ->
                        ignore
                end, Names)
        end, Modules).