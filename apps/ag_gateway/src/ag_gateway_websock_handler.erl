%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_gateway_websock_handler).

-include_lib("ag_base/include/agb_debuglogger.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").
-include_lib("ag_engine/include/ag_engine_code_defines.hrl").

-define(WEBSOCKTIMEOUT, (5 * 60 * 1000)).
-define(CHECK_CARRIER_QUE_INTERVAL, 3000).
-define(CARRIER_CHECK, {'CARRIER_CHECK'}).
-export([init/3]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-spec init(Req :: map(), _Headers :: list(), Opts :: map()) ->
    {cowboy_websocket, Req :: map(), Opts :: list(), Opt :: map()} | {stop, Req :: map(), Opts :: map()}.
init(Req, _Headers, Opts) ->
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        undefined ->
            {cowboy_websocket, Req, Opts, #{idle_timeout => ?WEBSOCKTIMEOUT}};
        Subprotocols ->
            case lists:keymember(<<"mqtt">>, 1, Subprotocols) of
                true ->
                    Req2 = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>, <<"mqtt">>, Req),
                    {cowboy_websocket, Req2, Opts, #{idle_timeout => ?WEBSOCKTIMEOUT}};
                false ->
                    {stop, Req, Opts}
            end
    end.

-spec websocket_init(State :: term()) ->
    {ok, State :: term()}.
websocket_init(#{msgcarrier := MsgCarrier, clientip := ClientIP} = State) ->
    ?LOG_INFO("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p ip:~p init~n",
        [self(), ag_engine_storage:get_roleworkerpid(), ag_engine_storage:get_id(), ag_engine_storage:get_archive(), ClientIP]),
    case MsgCarrier of
        true ->
            erlang:send_after(?CHECK_CARRIER_QUE_INTERVAL, self(), ?CARRIER_CHECK);
        false ->
            ignore
    end,
    process_flag(trap_exit, true),
    ag_engine_cluster:increase_gate(node()),
    ag_engine_storage:set_client_ip(ClientIP),
    {ok, State}.

-spec websocket_handle(
    {ping, _Payload :: term()}|{text, Data :: binary()}|{binary, Data :: binary()}, State :: term()) ->
    {ok, State :: term()} |
    {reply, {text, binary()}, State :: term()}.
websocket_handle({ping, _Payload}, State) ->
    on_ping(),
    {ok, State};
websocket_handle(ping, State) ->
    on_ping(),
    {ok, State};
websocket_handle({Type, InBin}, #{proto := Proto, app_setting := AppSetting, msgcarrier := MsgCarrier} = State)
    when Type =:= text orelse Type =:= binary ->
    apply(Type, Proto, MsgCarrier, InBin, AppSetting, State);
websocket_handle(_Frame, State) ->
    {ok, State}.

-spec terminate(any(), cowboy_req:req(), any()) ->
    ok.
terminate(Reason, _Req, _State) ->
    ?LOG_INFO("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p terminate:~p~n",
        [self(), ag_engine_storage:get_roleworkerpid(), ag_engine_storage:get_id(), ag_engine_storage:get_archive(), Reason]),
    ag_engine_msg_executer:on_terminate(),
    ag_engine_cluster:gate_offline().

-spec websocket_info({timeout, _Ref :: _, Data :: binary()} | {on_persist_loaded} | 'CARRIER_CHECK', State :: _) ->
    {reply, {text, Data :: binary()}, State} |{ok, State}.
websocket_info({on_persist_loaded}, State) ->
    ?LOG_INFO("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p persist_loaded~n",
        [self(), ag_engine_storage:get_roleworkerpid(), ag_engine_storage:get_id(), ag_engine_storage:get_archive()]),
    {ok, State};
websocket_info(
    ?CARRIER_CHECK,
    #{
        proto := Proto,
        app_setting := AppSetting,
        prototype := Type,
        msgcarrier := MsgCarrier
    } = State) ->
    RoleWorkerPid = ag_engine_storage:get_roleworkerpid(),
    case catch ag_engine_msg_executer:pop_msg(RoleWorkerPid) of
        {ok, OutMsg} ->
            EncodeFilters = get_filter(encode_filters, AppSetting),
            {ok, OutBin} = do_encode_msg(EncodeFilters, MsgCarrier, OutMsg, Proto),
            {reply, {Type, OutBin}, State};
        {error, undefined} ->
            erlang:send_after(?CHECK_CARRIER_QUE_INTERVAL, self(), ?CARRIER_CHECK),
            {ok, State};
        {'EXIT', _} ->
            erlang:send_after(?CHECK_CARRIER_QUE_INTERVAL, self(), ?CARRIER_CHECK),
            {ok, State};
        [] ->
            erlang:send_after(?CHECK_CARRIER_QUE_INTERVAL, self(), ?CARRIER_CHECK),
            {ok, State}
    end;
websocket_info({'EXIT', Pid, _}, State) ->
    RWPid = ag_engine_storage:get_roleworkerpid(),
    case RWPid of
        Pid ->
            ?LOG_ERROR("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p EXIT Pid:~p RWPid:~p reason:no_match~n",
                [self(), ag_engine_storage:get_roleworkerpid(), ag_engine_storage:get_id(), ag_engine_storage:get_archive(), Pid, RWPid]),
            {stop, State};
        _ ->
            {ok, State}
    end;
websocket_info({stop, bad_request}, State) ->
    {stop, State};
websocket_info({get_core_value, KeyList, SendPid}, State) ->
    SendPid ! {core_value, ag_engine_storage:get_core_info_by_keys(KeyList)},
    {ok, State};
websocket_info(
    {async_handle_result, Reply, Echo},
    #{
        proto := Proto,
        app_setting := AppSetting,
        msgcarrier := MsgCarrier
    } = State) ->
    {_, OutMsg} = ag_engine_msg_executer:reply(Reply, Echo),
    EncodeFilters = get_filter(encode_filters, AppSetting),
    {ok, OutBin} = do_encode_msg(EncodeFilters, MsgCarrier, OutMsg, Proto),
    {reply, {binary, OutBin}, State};
websocket_info({message_to_client, OutBin}, State) when is_binary(OutBin) or is_list(OutBin) ->
    case iolist_size(OutBin) of
        0 ->
            {ok, State};
        _Size ->
            ?LOG_DEBUG("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p s=>c data:~p~n",
                [
                    self(),
                    ag_engine_storage:get_roleworkerpid(),
                    ag_engine_storage:get_id(),
                    ag_engine_storage:get_archive(),
                    iolist_to_binary(OutBin)
                ]),
            {reply, {binary, iolist_to_binary(OutBin)}, State}
    end;
websocket_info(
    {message_to_client, OutMsg},
    #{
        proto := Proto,
        app_setting := AppSetting,
        msgcarrier := MsgCarrier
    } = State) when is_map(OutMsg) ->
    EncodeFilters = get_filter(encode_filters, AppSetting),
    {ok, OutBin} = do_encode_msg(EncodeFilters, MsgCarrier, OutMsg, Proto),
    {reply, {binary, OutBin}, State};
websocket_info({text_message_to_client, OutBin}, State)
    when is_binary(OutBin) or is_list(OutBin) ->
    case iolist_size(OutBin) of
        0 ->
            {ok, State};
        _Size ->
            ?LOG_DEBUG("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p s=>c data:~p~n",
                [self(), ag_engine_storage:get_roleworkerpid(), ag_engine_storage:get_id(), ag_engine_storage:get_archive(), OutBin]),
            {reply, {text, iolist_to_binary(OutBin)}, State}
    end;
websocket_info(
    {text_message_to_client, OutMsg},
    #{
        proto := Proto,
        app_setting := AppSetting,
        msgcarrier := MsgCarrier
    } = State) when is_map(OutMsg) ->
    EncodeFilters = get_filter(encode_filters, AppSetting),
    {ok, OutBin} = do_encode_msg(EncodeFilters, MsgCarrier, OutMsg, Proto),
    {reply, {text, OutBin}, State};
websocket_info({kick_user, kick_user_other_login, Msg}, #{proto := Proto, app_setting := AppSetting} = State) ->
    self() ! kick_stop,
    EncodeFilters = maps:get(encode_filters, AppSetting, []),
    MsgBinary = ag_engine_protocol_codec:encode(Proto, Msg, EncodeFilters),
    {reply, {binary, MsgBinary}, State};
websocket_info(kick_user, State) ->
    self() ! kick_stop,
    {ok, State};
websocket_info(kick_stop, State) when is_map(State) ->
    {stop, State};
websocket_info({apply, M, F, A}, State) ->
    M:F(A),
    {ok, State};
websocket_info({apply_mfa, M, F, A}, State) ->
    erlang:apply(M, F, A),
    {ok, State};
websocket_info(_, State) ->
    {ok, State}.


apply(ReplyType, Proto, MsgCarrier, InBin, AppSetting, State) ->
    DecodeFilters = get_filter(decode_filters, AppSetting),
    EncodeFilters = get_filter(encode_filters, AppSetting),
    case catch do_decode_msg(DecodeFilters, MsgCarrier, InBin, Proto) of
        {ok, InMsg} ->
            do_msg_apply(ReplyType, Proto, MsgCarrier, InMsg, EncodeFilters, State);
        {'EXIT', Reason} ->
            ?LOG_ERROR("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p decode data:~p error reason:~p~n",
                [
                    self(),
                    ag_engine_storage:get_roleworkerpid(),
                    ag_engine_storage:get_id(),
                    ag_engine_storage:get_archive(), InBin, Reason
                ]),
            OutBin = ag_engine_protocol_codec:encode(Proto,
                ag_engine_message_helper:pack_code(?MSG_INTERNAL_ERROR, ?ERROR_CODE_INTERNAL_SERVER_ERROR), []),
            ?LOG_DEBUG("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p EXIT s=>c data:~p~n",
                [
                    self(),
                    ag_engine_storage:get_roleworkerpid(),
                    ag_engine_storage:get_id(),
                    ag_engine_storage:get_archive(),
                    OutBin
                ]),
            {reply, {ReplyType, OutBin}, State}
    end.

do_msg_apply(ReplyType, Proto, true, InMsg, EncodeFilters, State) ->
    MsgResult = ag_engine_msg_executer:msg_carrier_apply(InMsg),
    on_msg_result(MsgResult, ReplyType, Proto, true, EncodeFilters, State);
do_msg_apply(ReplyType, Proto, false, InMsg, EncodeFilters, State) ->
    MsgResult = ag_engine_msg_executer:msg_single_apply(InMsg, true),
    on_msg_result(MsgResult, ReplyType, Proto, false, EncodeFilters, State).

on_msg_result(Result, ReplyType, Proto, MsgCarrier, EncodeFilters, State) ->
    case Result of
        {true, OutMsg} when OutMsg == [];OutMsg == <<>> ->
            RWPid = ag_engine_storage:get_roleworkerpid(),
            ag_engine_worker_module:worker_touch_me(RWPid),
            {ok, State};
        {true, OutMsg} ->
            RWPid = ag_engine_storage:get_roleworkerpid(),
            ag_engine_worker_module:worker_touch_me(RWPid),
            {ok, OutBin} = do_encode_msg(EncodeFilters, MsgCarrier, OutMsg, Proto),
            {reply, {ReplyType, OutBin}, State#{prototype => ReplyType}};
        {false, OutMsg} when OutMsg == [];OutMsg == <<>> ->
            self() ! {stop, bad_request},
            {ok, State};
        {false, OutMsg} ->
            self() ! {stop, bad_request},
            {ok, OutBin} = do_encode_msg(EncodeFilters, MsgCarrier, OutMsg, Proto),
            {reply, {ReplyType, OutBin}, State}
    end.


do_decode_msg(DecodeFilters, MsgCarrier, InBin, Proto) when is_list(DecodeFilters) ->
    ?LOG_DEBUG("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p c=>s data:~p~n",
        [self(), ag_engine_storage:get_roleworkerpid(), ag_engine_storage:get_id(), ag_engine_storage:get_archive(), InBin]),
    InMsg = ag_engine_protocol_codec:decode(Proto, MsgCarrier, InBin, DecodeFilters),
    ?LOG_INFO("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p c=>s msg:~p~n",
        [self(), ag_engine_storage:get_roleworkerpid(), ag_engine_storage:get_id(), ag_engine_storage:get_archive(), InMsg]),
    {ok, InMsg}.

do_encode_msg(EncodeFilters, true, OutMsg, Proto) when is_map(OutMsg) ->
    do_encode_msg(EncodeFilters, true, [OutMsg], Proto);
do_encode_msg(EncodeFilters, true, OutMsg, Proto) when is_list(OutMsg) ->
    ?LOG_INFO("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p s=>c msg:~p~n",
        [self(), ag_engine_storage:get_roleworkerpid(), ag_engine_storage:get_id(), ag_engine_storage:get_archive(), OutMsg]),
    OutBin = ag_engine_protocol_codec:encode(Proto, OutMsg, EncodeFilters),
    ?LOG_DEBUG("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p s=>c data:~p~n",
        [self(), ag_engine_storage:get_roleworkerpid(), ag_engine_storage:get_id(), ag_engine_storage:get_archive(), OutBin]),
    {ok, OutBin};
do_encode_msg(EncodeFilters, false, OutMsg, Proto) ->
    ?LOG_INFO("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p s=>c msg:~p~n",
        [self(), ag_engine_storage:get_roleworkerpid(), ag_engine_storage:get_id(), ag_engine_storage:get_archive(), OutMsg]),
    OutBin = ag_engine_protocol_codec:encode(Proto, OutMsg, EncodeFilters),
    ?LOG_DEBUG("==>[websocket] gate:~p roleworker:~p uid:~p archive:~p s=>c data:~p~n",
        [self(), ag_engine_storage:get_roleworkerpid(), ag_engine_storage:get_id(), ag_engine_storage:get_archive(), OutBin]),
    {ok, OutBin}.

get_filter(Filters, AppSetting) ->
    maps:get(Filters, AppSetting, []).

on_ping() ->
    RWPid = ag_engine_storage:get_roleworkerpid(),
    ag_engine_worker_module:worker_touch_me(RWPid),
    ag_engine_msg_executer:on_ping().

