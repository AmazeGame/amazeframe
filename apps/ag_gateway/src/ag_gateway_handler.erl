%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
-module(ag_gateway_handler).
-include_lib("ag_base/include/agb_debuglogger.hrl").
-include_lib("ag_engine/include/ag_engine.hrl").
-include_lib("ag_engine/include/ag_engine_core_defines.hrl").
-include("ag_gateway_common.hrl").
-export([init/2]).
-export([
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).
%% API
-define(WEBSOCKET_FLAG, <<"web_socket_handler">>).

-type call_result(State) :: {ok, State}
| {ok, State, hibernate}
| {reply, cow_ws:frame() | [cow_ws:frame()], State}
| {reply, cow_ws:frame() | [cow_ws:frame()], State, hibernate}
| {stop, State}.

-type terminate_reason() :: normal | stop | timeout
| remote | {remote, cow_ws:close_code(), binary()}
| {error, badencoding | badframe | closed | atom()}
| {crash, error | exit | throw, any()}.

-spec init(Req :: map(), Opts :: map()) ->
    {ok, Req :: term(), Opts :: map()} |
    {cowboy_websocket, Req :: map(), Opts :: list(), Opt :: map()} |
    {stop, Req :: map(), Opts :: map()}.
init(Req, Opts) ->
    rand:seed(exs1024s, erlang:timestamp()),
    Headers1 = cowboy_req:headers(Req),
    QueryString = cowboy_req:parse_qs(Req),
    Headers = QueryString ++ maps:to_list(Headers1),
    ClientIp = get_real_ip(Req),
    Proto = get_proto(Headers),
    ?LOG_INFO("client is connected :proto=>~p~n", [Headers]),
    MsgCarrier = get_msg_carrier(Headers),
    Opts1 = Opts#{proto => Proto, msgcarrier => MsgCarrier, clientip => ClientIp},
    case cowboy_req:parse_header(<<"upgrade">>, Req) of
        undefined ->
            put(?WEBSOCKET_FLAG, false),
            ag_gateway_http_handler:init(Req, Headers, Opts1);
        _WebSocket ->
            ag_gateway_websock_handler:init(Req, Headers, Opts1)
    end.

get_msg_carrier(Headers) ->
    case lists:keyfind(?HEADER_MSGCARRIER_FIELD, 1, Headers) of
        false ->
            false;
        {_, <<"false">>} ->
            false;
        {_, <<"true">>} ->
            true;
        _ ->
            true
    end.

get_proto(Headers) ->
    case lists:keyfind(?HEADER_PROTO_FIELD, 1, Headers) of
        false ->
            case application:get_env(ag_gateway, default_proto) of
                {ok, DefaultProto} ->
                    DefaultProto;
                _ ->
                    <<"json">>
            end;
        {_, P} when is_list(P) ->
            list_to_binary(P);
        {_, P} when is_binary(P) ->
            P
    end.

get_real_ip(Req) ->
    Headers = cowboy_req:headers(Req),
    case maps:find(<<"x-forwarded-for">>, Headers) of
        {ok, ForwardedIp} ->
            [RealIp | _] = binary:split(ForwardedIp, <<",">>, [global]),
            agb_convertor:to_string(RealIp);
        error ->
            case maps:find(<<"x-real-ip">>, Headers) of
                {ok, RealIp} ->
                    agb_convertor:to_string(RealIp);
                error ->
                    {PeerIp, _} = cowboy_req:peer(Req),
                    inet_parse:ntoa(PeerIp)
            end
    end.

-spec websocket_init(State :: term()) ->
    {ok, State :: term()}.
websocket_init(State) ->
    put(?WEBSOCKET_FLAG, true),
    ag_gateway_websock_handler:websocket_init(State).

-spec websocket_handle({text, Data :: binary()}|{binary, Data :: binary()}, State :: term()) ->
    {ok, State :: term()} | {reply, {text, binary()}, State :: term()}.
websocket_handle(Data, State) ->
    ag_gateway_websock_handler:websocket_handle(Data, State).

-spec terminate(any(), cowboy_req:req(), terminate_reason()) ->
    no_return().
terminate(State, Req, Reason) ->
    case get(?WEBSOCKET_FLAG) of
        true ->
            ag_gateway_websock_handler:terminate(State, Req, Reason);
        _ ->
            ok
    end.

-spec websocket_info(any(), State) ->
    call_result(State) when State :: any().
websocket_info(Info, State) ->
    case get(?WEBSOCKET_FLAG) of
        true ->
            ag_gateway_websock_handler:websocket_info(Info, State);
        _ ->
            ?LOG_ERROR("it is incredible: http can not get info"),
            {ok, State}
    end.
