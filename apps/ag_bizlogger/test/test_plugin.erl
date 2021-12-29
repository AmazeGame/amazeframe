%%%-------------------------------------------------------------------
%%% @author BlackCat <anyongbo@fotoable.com>
%%% @copyright (C) 2020, Harbour Studios
%%% @doc
%%%
%%% @end
%%% Created : 08. 7月 2020 18:18
%%%-------------------------------------------------------------------
-module(test_plugin).
-author("BlackCat <anyongbo@fotoable.com>").

-behaviour(tsbl_plugin_behaviours).
%% API
-export([
    header/1,
    do_combine_header_opt/2
]).

-spec header(Args :: term()) ->
    map().
header({Name, AppId, DataFormat}) ->
    #{
        <<"appid">> => AppId,
        <<"hostname">> => Name,
        <<"date">> => agb_convertor:to_binary(agb_time:format_datetime(DataFormat, calendar:universal_time()))
    }.

-spec do_combine_header_opt(Header :: map(), Log :: map()) ->
    map()|string()|binary().
do_combine_header_opt(Header, Log) ->
    maps:merge(Header,Log).

%%do_combine_header_opt([], _, Log) ->
%%    Log;
%%do_combine_header_opt([appid | Keys], #{appid := AppId} = Opts, Log) ->
%%    case maps:is_key(appid, Log) of
%%        true ->
%%            do_combine_header_opt(Keys, Opts, Log);
%%        _ ->
%%            do_combine_header_opt(Keys, Opts, maps:merge(Log, #{appid => AppId}))
%%    end;
%%do_combine_header_opt([hostname | Keys], #{hostname := HostName} = Opts, Log) ->
%%    case maps:is_key(hostname, Log) of
%%        true ->
%%            do_combine_header_opt(Keys, Opts, Log);
%%        _ ->
%%            do_combine_header_opt(Keys, Opts, maps:merge(Log, #{hostname => HostName}))
%%    end;
%%do_combine_header_opt([date | Keys], #{date := Date} = Opts, Log) ->
%%    case maps:is_key(date, Log) of
%%        true ->
%%            do_combine_header_opt(Keys, Opts, Log);
%%        _ ->
%%            do_combine_header_opt(Keys, Opts, maps:merge(Log, #{date => Date}))
%%    end.