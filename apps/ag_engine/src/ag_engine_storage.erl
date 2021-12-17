%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-module(ag_engine_storage).

-include("ag_engine.hrl").
-include_lib("ag_base/include/agb_debuglogger.hrl").
%% API
-export([
    put/2,
    get/1
]).
-export([
    get_security/0,
    set_security/1
]).
-export([
    get_archive/0,
    set_archive/1
]).
-export([
    get_session/0,
    set_session/1
]).
-export([
    get_roleworkerpid/0,
    set_roleworkerpid/1
]).
-export([
    get_id/0,
    set_id/1
]).
-export([
    get_idtype/0,
    set_idtype/1
]).
-export([
    get_gatepid/0,
    set_gatepid/1
]).
-export([
    get_gatenode/0,
    set_gatenode/1
]).
-export([
    get_client_ip/0,
    set_client_ip/1
]).
-export([
    get_msg_exec_info/0,
    add_msg_exec_info/1,
    clean_msg_exec_info/0
]).
-export([
    update_msg_metrics_info/2,
    get_msg_metrics_info/1,
    remove_msg_metrics_info/1,
    get_all_msg_metrics_info/0
]).
-export([
    get_timer_ref/1,
    set_timer_ref/2
]).
-export([get_core_info_by_keys/1]).

-define(IMMDIATE_STORE, 'immdiate').

%%-type fin() :: fin | nofin.
-type core_key() :: security | archive | session | roleworkerpid | gatepid |
gatenode | msg_metrics_info | id | client_ip | idtype.

-export_type([core_key/0]).

-spec put(Key :: term(), Value :: term()) ->
    undefined | term().
put(Key, Value) ->
    erlang:put({?IMMDIATE_STORE, Key}, Value).

-spec get(Key :: term()) ->
    undefined | term().
get(Key) ->
    erlang:get({?IMMDIATE_STORE, Key}).

-spec set_security(Value :: ag_game_id()) ->
    undefined | ag_game_id().
set_security(Security) ->
    ?MODULE:put({core, security}, Security).

-spec get_security() ->
    undefined | ag_game_id().
get_security() ->
    ?MODULE:get({core, security}).

-spec set_archive(Value :: ag_game_id()) ->
    undefined | ag_game_id().
set_archive(Archive) ->
    ?MODULE:put({core, archive}, Archive).

-spec get_archive() ->
    undefined | ag_game_id().
get_archive() ->
    ?MODULE:get({core, archive}).

-spec set_session(Value :: ag_game_id()) ->
    undefined | ag_game_id().
set_session(Session) ->
    ?MODULE:put({core, session}, Session).

-spec get_session() ->
    undefined | ag_game_id().
get_session() ->
    ?MODULE:get({core, session}).

-spec set_roleworkerpid(Value :: pid()) ->
    undefined | pid().
set_roleworkerpid(Pid) ->
    ?MODULE:put({core, roleworkerpid}, Pid).

-spec get_roleworkerpid() ->
    undefined | pid().
get_roleworkerpid() ->
    ?MODULE:get({core, roleworkerpid}).

-spec set_gatepid(Value :: pid()|undefined) ->
    undefined | pid().
set_gatepid(undefined) ->
    ?LOG_WARNING("owner:~p set gatepid:~p~n", [self(), undefined]),
    ?MODULE:put({core, gatepid}, undefined);
set_gatepid(IdType) ->
    ?LOG_WARNING("owner:~p set gatepid:~p~n", [self(), IdType]),
    ?MODULE:put({core, gatepid}, IdType).

-spec get_gatepid() ->
    undefined | pid().
get_gatepid() ->
    ?MODULE:get({core, gatepid}).

-spec set_gatenode(Value :: node()) ->
    undefined | node().
set_gatenode(IdType) ->
    ?MODULE:put({core, gatenode}, IdType).

-spec get_gatenode() ->
    undefined | node().
get_gatenode() ->
    ?MODULE:get({core, gatenode}).

-spec set_id(Value :: ag_game_id()) ->
    undefined | ag_game_id().
set_id(Id) ->
    ?MODULE:put({core, id}, Id).

-spec get_id() ->
    undefined | ag_game_id().
get_id() ->
    ?MODULE:get({core, id}).

-spec set_client_ip(Ip :: string()) ->
    undefined | term().
set_client_ip(Ip) ->
    ?MODULE:put({core, client_ip}, Ip).

-spec get_client_ip() ->
    undefined | term().
get_client_ip() ->
    ?MODULE:get({core, client_ip}).

-spec set_idtype(Value :: term()) ->
    undefined | term().
set_idtype(IdType) ->
    ?MODULE:put({core, idtype}, IdType).

-spec get_idtype() ->
    undefined | term().
get_idtype() ->
    ?MODULE:get({core, idtype}).

-spec set_timer_ref(any(), reference() | undefined) ->
    undefined | term().
set_timer_ref(Name, Ref) ->
    ?MODULE:put({timeref, Name}, Ref).

-spec get_timer_ref(atom()) ->
    undefined | term().
get_timer_ref(Name) ->
    ?MODULE:get({timeref, Name}).

-spec clean_msg_exec_info() ->
    undefined | [].
clean_msg_exec_info() ->
    ?MODULE:put({core, msg_exec_info}, []).

-spec add_msg_exec_info(tuple()) ->
    undefined | [].
add_msg_exec_info(Info) ->
    ?MODULE:put({core, msg_exec_info}, get_msg_exec_info() ++ [Info]).

-spec get_msg_exec_info() ->
    undefined | [].
get_msg_exec_info() ->
    case ?MODULE:get({core, msg_exec_info}) of
        undefined ->
            [];
        V ->
            V
    end.

-spec update_msg_metrics_info(TrackingId :: term(), MetricsInfo :: map()) ->
    undefined | term().
update_msg_metrics_info(TrackingId, MetricsInfo) ->
    case ?MODULE:get({core, msg_metrics_info}) of
        undefined ->
            ?MODULE:put({core, msg_metrics_info}, #{TrackingId => MetricsInfo});
        Map ->
            ?MODULE:put({core, msg_metrics_info}, Map#{TrackingId => MetricsInfo})
    end.

-spec get_msg_metrics_info(TrackingId :: term()) ->
    map() | undefined.
get_msg_metrics_info(TrackingId) ->
    case ?MODULE:get({core, msg_metrics_info}) of
        undefined ->
            undefined;
        Map ->
            maps:get(TrackingId, Map, undefined)
    end.

-spec get_all_msg_metrics_info() ->
    map().
get_all_msg_metrics_info() ->
    case ?MODULE:get({core, msg_metrics_info}) of
        undefined ->
            #{};
        Map ->
            Map
    end.

-spec remove_msg_metrics_info(TrackingId :: term()) ->
    ok.
remove_msg_metrics_info(TrackingId) ->
    case ?MODULE:get({core, msg_metrics_info}) of
        undefined ->
            ok;
        Map ->
            NewMap = maps:remove(TrackingId, Map),
            if
                map_size(NewMap) == 0 ->
                    ?MODULE:put({core, msg_metrics_info}, undefined);
                true ->
                    ?MODULE:put({core, msg_metrics_info}, NewMap)
            end,
            ok
    end.

-spec get_core_info_by_keys([core_key()]) ->
    list().
get_core_info_by_keys(KeyList) ->
    lists:map(
        fun(Key) ->
            {Key, ?MODULE:get({core, Key})}
        end,
        KeyList
    ).