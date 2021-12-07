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
-module(ag_logger_file_process).


-include_lib("ag_logger.hrl").
-behaviour(gen_server).


%% API
-export([start_link/4, write/3,close/1,make_proc_biz_name/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).
-define(CHECK_INTERVAL_TIME,10000).
-record(state, {
	outfd::file:io_device()|undefined,
	outfile="" ::string() | binary(),
	tempfile="" ::string() | binary(),
	checkpoint::calendar:datetime(),
	rule::agb_time:checkpoint_rule(),
	nameformat::string(),
	outdir::string()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec write(Name::atom(),Mode::atom(),Bin::binary())->ok.
%%write(DateStr,AppId,?SERVER,Message)->
%%	?SERVER ! {write,Message#{date=>DateStr,appid=>AppId}},
%%	ok.
write(Biz,async,Bin)->
	?LOG_DEBUG("ag_logger_file_process async write bin ~p~n",[{Bin,make_proc_biz_name(Biz)}]),
	try
		gen_server:cast(make_proc_biz_name(Biz),{write,Bin}),
		ok
	catch T:R:S ->
		?LOG_ERROR("ag_logger_file_process:[~s] async write error ~p~n",[Biz,{T,R,S}]),
		{error, no_such_log}
	end;
write(Biz,sync,Bin)->
	?LOG_DEBUG("ag_logger_file_process sync write bin ~p~n",[{Bin,make_proc_biz_name(Biz)}]),
	try
		gen_server:call(make_proc_biz_name(Biz),{write,Bin}),
		ok
	catch T:R:S ->
		?LOG_ERROR("ag_logger_file_process:[~s] sync write error ~p~n",[Biz,{T,R,S}]),
		{error, no_such_log}
	end.

close(Biz) ->
	?LOG_DEBUG("ag_logger_file_process close ~p",[Biz]),
	gen_server:cast(make_proc_biz_name(Biz),{close}).


make_proc_biz_name(Biz)when is_binary(Biz)->
	make_proc_biz_name(binary_to_list(Biz));
make_proc_biz_name(Biz)when is_atom(Biz)->
	make_proc_biz_name(atom_to_list(Biz));
make_proc_biz_name(Biz)when is_list(Biz)->
	list_to_atom(Biz ++ atom_to_list(?MODULE)).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Biz::atom(),FileNameFormat::string(),SplitFileRule::agb_time:checkpoint_rule(),OutDir::string()) ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(?SERVER,FileNameFormat,SplitFileRule,OutDir) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [FileNameFormat,SplitFileRule,OutDir], []);
start_link(Biz,FileNameFormat,SplitFileRule,OutDir) ->
	Name = make_proc_biz_name(Biz),
	gen_server:start_link({local, Name}, ?MODULE, [FileNameFormat,SplitFileRule,OutDir], []).


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
-spec init(Args::term()) ->{ok, State :: #state{}}.
%%	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
%%	{stop, Reason :: term()} | ignore .
init([FileNameFormat,SplitFileRule,OutDir]) ->
	State = #state{checkpoint = agb_time:get_checkpoint(SplitFileRule),rule = SplitFileRule,nameformat = FileNameFormat,outdir = OutDir},
	erlang:send_after(?CHECK_INTERVAL_TIME,self(),{checkpoint}),
	NewState = gen_new_file(State),
	% 未处理临时文件
	undo_temp_file(NewState),
	{ok, NewState}.

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
handle_call({write,Bin}, _From, #state{outfd=Fd}=State) ->
	file:write(Fd,Bin),
	{reply, ok, State};
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
handle_cast({write,Bin}, #state{outfd=Fd}=State) ->
	file:write(Fd,Bin),
	{noreply,State};
handle_cast({close}, State) ->
	{stop, "close",State};
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
handle_info({checkpoint}, #state{checkpoint = LastCP,rule = Rule}=State) ->
	CP = agb_time:get_checkpoint(Rule),
	NewState =
		if LastCP =/= CP ->
			gen_new_file(State#state{checkpoint = CP});
		true->
			State
	end,
	erlang:send_after(?CHECK_INTERVAL_TIME,self(),{checkpoint}),
	{noreply, NewState};
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
terminate(_Reason,#state{outfd = Fd} =_State) ->
	file:close(Fd),
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

time_string(Pattern)->
	DateTime = erlang:universaltime(),
	agb_time:format_datetime(Pattern,DateTime).

gen_new_file(#state{outfd = undefined}=State)->
	do_gen_new_file(State);
gen_new_file(#state{outfd = Fd,tempfile = TmpFile,outfile = OutFile}=State)->
	file:close(Fd),
	file:rename(TmpFile,OutFile),
	NewState = do_gen_new_file(State#state{outfd=undefined}),
	NewState.

do_gen_new_file(#state{outdir = OutDir,nameformat = FileNameFormat}=State)->
	CatFileName= time_string(FileNameFormat),
	OutFile = filename:absname_join(OutDir,CatFileName),
	BaseFile = filename:basename(OutFile),
	DirName = filename:dirname(OutFile),
	TmpFile = filename:absname_join(DirName,[$.,BaseFile]),
	agb_file:ensure_dir(DirName),
	{ok, Fd} = file:open(TmpFile,[write]),
	State#state{outfd = Fd,tempfile = TmpFile,outfile = OutFile}.



undo_temp_file(State) ->
	Wildcard = filename:absname_join(State#state.outdir, "**"),
	FilenameList = filelib:wildcard(Wildcard),
	Fun = fun(Filename) ->
		% 筛掉路径
		case filelib:is_file(Filename) of
			false ->
				ok;
			true ->
				BaseFile = filename:basename(Filename),
				[F|T] = BaseFile,
				% 是否为临时文件
				case F =:= $. of
					false ->
						ok;
					true ->
						% 是否为当前使用中文件
						case Filename =:= State#state.tempfile of
							true ->
								ok;
							false ->
								Dir = filename:dirname(Filename),
								file:rename(Filename, filename:absname_join(Dir, T))
						end
				end
		end
				end,
	lists:foreach(Fun, FilenameList).
