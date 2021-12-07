%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.28
%%%-------------------------------------------------------------------
%%%
-module(ag_config_adapter_network).


-include_lib("ag_base/include/agb_debuglogger.hrl").

-behaviour(ag_config_adapter).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    init_config/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([register_config_change_callback/1]).

-define(SERVER, ?MODULE).

-record(state, {
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([{ConfigServerUrl, AppId, Cluster}]) ->
    inets:start(),
    put_(configserverurl, ConfigServerUrl),
    put_(appid, AppId),
    put_(cluster, Cluster),
    download_config(),
    download_config(0).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({register_callback, {Name, M, F}}, State) ->
    register_config_change_callback(Name, M, F),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(long_poll, State) ->
    check_namespaceName_notificationId(),
    {noreply, State};
handle_info({updateconfig, Infos}, State) ->
    update_namespaceName_notificationId(Infos),
    get_namespaceNames_configurations(Infos),
    notification_change_info(Infos),
    erlang:send_after(5000, self(), long_poll),
    {noreply, State};
handle_info(notupdateconfig, State) ->
    erlang:send_after(5000, self(), long_poll),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
put_(Key, Value) ->
    erlang:put({?MODULE, Key}, Value).

get_(Key) ->
    erlang:get({?MODULE, Key}).

-spec init_namespaceName(NamespaceNames :: list()) -> list().
init_namespaceName(NamespaceNames) ->
    [put_(Name, #{version => -1, callback => {}}) || Name <- NamespaceNames].

-spec build_namespaceName_notificationId_url() -> string().
build_namespaceName_notificationId_url() ->
    ConfigServerUrl = get_(configserverurl),
    Appid = get_(appid),
    Cluster = get_(cluster),
    BaseUrl = ConfigServerUrl ++ "notifications/v2?",
    List = get_(appnames),
    ?LOG_INFO("build_namespaceName_notificationId_url List ~p~n", [List]),
    Notifications =
        lists:map(
            fun(AppName) ->
                NamespaceInfo = get_(AppName),
                NotificationId = maps:get(version, NamespaceInfo),
                #{
                    <<"namespaceName">>=> atom_to_binary(AppName, utf8),
                    <<"notificationId">> => NotificationId
                }
            end, List
        ),
    ?LOG_INFO("build_namespaceName_notificationId_url Notifications ~p~n", [Notifications]),

    InputParam = 
        [
            {"appId",Appid},
            {"cluster",Cluster},
            {"notifications",agb_json:encode(Notifications)}
        ],
    BaseUrl ++ uri_string:compose_query(InputParam).

-spec build_namespaceName_configurations_url(NamespaceName :: list()) -> string().
build_namespaceName_configurations_url(NamespaceName) ->
    ConfigServerUrl = get_(configserverurl),
    AppId = get_(appid),
    Cluster = get_(cluster),
    IP = os:getenv("MY_POD_IP", "127.0.0.1"),
    ConfigServerUrl ++ "configs/" ++ AppId ++ "/" ++ Cluster ++ "/" ++ NamespaceName ++ "?ip=" ++ IP.

first_get_namespaceName_notificationId() ->
    Url = build_namespaceName_notificationId_url(),
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, JsonInfo}} ->
            Infos = agb_json:decode2list(list_to_binary(JsonInfo)),
            update_namespaceName_notificationId(Infos),
            ?LOG_INFO("get config from  server Infos ~p~n", [Infos]),
            get_namespaceNames_configurations(Infos),
            true;
        _ ->
            ?LOG_INFO("get config from ~s server error~n", [Url]),
            erlang:throw(downloadfaile)
    end.

check_namespaceName_notificationId() ->
    Rui = build_namespaceName_notificationId_url(),
    Pid = self(),
    Fun =
        fun() ->
            try
                case httpc:request(get, {Rui, []}, [], []) of
                    {ok, {{_, 200, _}, _, JsonInfo}} ->
                        Infos = agb_json:decode2list(list_to_binary(JsonInfo)),
                        Pid ! {updateconfig, Infos};
                    Result ->
                        ?LOG_INFO("check_namespaceName_notificationId from server error ~p~n", [Result]),
                        Pid ! notupdateconfig
                end
            catch
                T:R:S ->
                    ?LOG_INFO("check_namespaceName_notificationId from server error ~p~n", [{T, R, S}]),
                    Pid ! notupdateconfig
            end
        end,
    spawn(Fun),
    ok.

-spec get_namespaceNames_configurations(Infos :: list()) -> ok.
get_namespaceNames_configurations(Infos) ->
    Fun =
        fun(Info) ->
            NamespaceName = proplists:get_value(<<"namespaceName">>, Info),
            Term = get_namespaceName_configurations(NamespaceName),
            update_namespaceName_configurations(NamespaceName, Term)
        end,
    [Fun(I) || I <- Infos],
    ok.

-spec get_namespaceName_configurations(NamespaceName :: binary()|list()) -> term().
get_namespaceName_configurations(NamespaceName) when is_binary(NamespaceName) ->
    get_namespaceName_configurations(binary_to_list(NamespaceName));
get_namespaceName_configurations(NamespaceName) ->
    Rui = build_namespaceName_configurations_url(NamespaceName),
    ?LOG_INFO("get configurations from server Rui ~p~n", [Rui]),
    case httpc:request(get, {Rui, []}, [], []) of
        {ok, {{_, 200, _}, _, JsonInfo}} ->
            %?LOG_INFO("get configurations from server JsonInfo ~p~n",[JsonInfo]),
            Json = agb_json:decode2list(list_to_binary(JsonInfo)),
            [{_, AppNames}] = proplists:get_value(<<"configurations">>, Json),
            {ok, Term} = agb_string:string_to_term(binary_to_list(AppNames)),
            Term;
        Info ->
            ?LOG_INFO("get configurations from [~s] server error info: ~p~n", [Rui, Info]),
            erlang:throw(downloadfaile)
    end.

-spec update_namespaceName_notificationId(Infos :: list()) ->
    ok.
update_namespaceName_notificationId(Infos) ->
    Fun =
        fun(Info) ->
            NamespaceName = binary_to_atom(proplists:get_value(<<"namespaceName">>, Info), utf8),
            NotificationId = proplists:get_value(<<"notificationId">>, Info),
            NamespaceInfo = get_(NamespaceName),
            put_(NamespaceName, NamespaceInfo#{version => NotificationId})
        end,
    [Fun(I) || I <- Infos],
    ok.

-spec update_namespaceName_configurations(NamespaceName :: binary(), Term :: [tuple()]) ->
    ok.
update_namespaceName_configurations(NamespaceName, Term) ->
    set_app_config(binary_to_atom(NamespaceName, utf8), Term),
    ok.

-spec register_config_change_callback(NamespaceName :: atom(), M :: atom(), F :: atom()) ->
    ok.
register_config_change_callback(NamespaceName, M, F) ->
    case get_(NamespaceName) of
        undefined ->
            false;
        Info ->
            put_(NamespaceName, Info#{callback => {M, F}}),
            %config_cached:put(Info#namespace_config{callback = {M,F}}),
            ok
    end.

-spec register_config_change_callback(Param :: {NamespaceName :: atom(), M :: atom(), F :: atom()}) ->
    ok.
register_config_change_callback(Param) ->
    gen_server:cast(?MODULE, {register_callback, Param}).

-spec notification_change_info(Infos :: list()) -> ok.
notification_change_info(Infos) ->
    Fun =
        fun(Info) ->
            NamespaceName = binary_to_atom(proplists:get_value(<<"namespaceName">>, Info), utf8),
            case get_(NamespaceName) of
                undefined -> nothing;
                NamespaceInfo ->
                    CallBack = maps:get(callback, NamespaceInfo),
                    case CallBack of
                        {} -> nothing;
                        {M, F} ->
                            apply(M, F, [])
                    end
            end
        end,
    [Fun(I) || I <- Infos],
    ok.

%%先从application空间拉取所有的空间名（所有需要配置文件的app的集合）
get_namespaceNames() ->
    get_namespaceName_configurations("application").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init_config(#{url:= string(), appid:= string(), cluster:= string()}) -> loading.
init_config(#{url:=Url, appid:=AppId, cluster:=Cluster}) ->
    ?LOG_INFO(" ag_adapter_localfile init ~p~n", [{Url, AppId, Cluster}]),
    ag_config_sup:start_child(ag_config_adapter_network, {Url, AppId, Cluster}),
    loading.

set_app_config(AppName, Config) ->
    agb_application:apply_application_env(AppName, Config).

download_config() ->
    AppNames = get_namespaceNames(),
    put_(appnames, AppNames),
    init_namespaceName(AppNames),
    first_get_namespaceName_notificationId(),
    erlang:send_after(5000, self(), long_poll).

download_config(5) ->
    ag_config_manager:set_app_launch_state(error),
    {stop, "download config error"};
download_config(Count) ->
    try
        download_config(),
        ag_config_manager:set_app_launch_state(running),
        {ok, #state{}}
    catch T:R:S ->
        ?LOG_INFO("download_config count:[~p] error ~p~n", [Count, {S, T, R}]),
        download_config(Count + 1)
    end.
