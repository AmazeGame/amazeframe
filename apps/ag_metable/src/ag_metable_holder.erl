%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.11.02
%%%-------------------------------------------------------------------
%%%
-module(ag_metable_holder).


-behaviour(gen_server).
-include_lib("ag_logger/include/ag_logger.hrl").
-include("ag_metable.hrl").
%% API
-export([start_link/0,current_root/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-export([import_file_async/9]).

-define(SERVER, ?MODULE).
-define(CHECK_CONFIG_DATA,{check_config}).
-define(CHECK_INTERVAL,10000).
-define(ROOT_TABLE,'dynamic_meta_root_table').
-define(RUNNING_TABLES_TABLE,'metable_tables_ets').
-define(MSG_DELAY_CLEAR_ROOT,{'delay_clear_root'}).
-define(DELAY_CLEAR_TIME,60*1000).
-define(REVERS_FIELD_NAME_FOR_WITH_TABLE,"#$#?with_table#").


-record(state, {
	flush_status:: flushing|idle,
	table_transform::ag_metable_hook:trans_type(),
	record_with_table :: list(),
	flush_ref::ets:tid(),
	dir::string(),
	file_list_table::[string()],
	table_matches=undefined::[ag_metable_hook:hook_match()],
	field_matches=undefined::[ag_metable_hook:hook_match()],
	data_import= []::[ag_metable_hook:import_hook()],
	header_define::[ag_metable:header_define()]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec current_root()-> undefined | ets:tid().
current_root()->
	case ets:lookup(?ROOT_TABLE,root) of
		[]-> undefined;
		[{root,RootId}]-> RootId
	end.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
	ets:new(?ROOT_TABLE,[set,named_table,protected,{read_concurrency,true}]),
	{ok,Dir} = application:get_env(meta_dir),

	{ok,Hooks} = application:get_env(hooks),

	TableTrans = case lists:keyfind(table_transform,1,Hooks) of
						false-> undefined;
					 {_,Trans}->Trans
				 end,

	FieldMatches = case lists:keyfind(field_match,1,Hooks) of
						false-> undefined;
					 {_,FMatches}->FMatches
				 end,

	TableMatches = case lists:keyfind(table_match,1,Hooks) of
						false-> undefined;
					 {_,TMatches}->TMatches
				 end,

	ImportHooks = case lists:keyfind(data_import,1,Hooks) of
						false-> [];
					 {_,ImHooks}->ImHooks
				 end,

	{ok,HeaderDefs} = application:get_env(header_define),


	RecordWithTable = case application:get_env(record_with_table) of
						  {ok, RWT} -> RWT;
						  _ -> []
					  end,

	NewImportHooks = lists:map(
		fun({TabName,Info}) when is_list(TabName)->
			{list_to_atom(TabName),Info};
			({TabName,Info}) when is_binary(TabName)->
				{binary_to_atom(TabName,utf8),Info};
			({TabName,Info}) when is_atom(TabName)->
				{TabName,Info}
		end,ImportHooks),


	ansyc_flush_dir(Dir,[],TableTrans,TableMatches,FieldMatches,NewImportHooks,HeaderDefs,RecordWithTable),
	erlang:send_after(?CHECK_INTERVAL,self(),?CHECK_CONFIG_DATA),


	{ok, #state{
		flush_status =flushing,
		table_transform = TableTrans,
		record_with_table = RecordWithTable ,
		flush_ref = make_ref(),
		file_list_table = [],
		dir = Dir,
		data_import=NewImportHooks,
		table_matches=TableMatches,
		field_matches = FieldMatches,
		header_define = HeaderDefs}}.

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
handle_info({?MSG_DELAY_CLEAR_ROOT,NeedClearRoot},State)->
	clear_root(NeedClearRoot),
	{noreply, State};
handle_info({async_import,{ok,RootId,File}},State=#state{flush_status = flushing})->
	?LOG_INFO("[ok]:~ts imported~n",[File]),
	Time = ag_metable_util:get_fileunixtime(File),
	FileTimes = get({filetime,RootId}),
	FileCount = get({filecount,RootId}),
	NewFileTimes =
		case lists:keyfind(File,1,FileTimes) of
			false-> [{File,Time}|FileTimes];
			_-> lists:keystore(File,1,FileTimes,{File,Time})
		end,
	NewState =
		if
			length(NewFileTimes) >= FileCount ->
				case ets:lookup(?ROOT_TABLE, root) of
					[] ->
						ets:insert(?ROOT_TABLE, {root, RootId});
					[{root, OldRootId}] ->
						ets:insert(?ROOT_TABLE, {root, RootId}),
						erlang:send_after(?DELAY_CLEAR_TIME, self(), {?MSG_DELAY_CLEAR_ROOT, OldRootId})
				end,
				?LOG_INFO("[ok]import all files:~p~n",[length(NewFileTimes)]),
				ag_eventdispatcher_process:fire(ag_metable,#{name=><<"loaded">>}),
				erase({filetime,RootId}),
				erase({filecount,RootId}),
				State#state{file_list_table = NewFileTimes,flush_status = idle};
			true ->
				put({filetime, RootId}, NewFileTimes),
				State
		end,
	{noreply, NewState};
handle_info({async_import,{error,RootId,File}},State=#state{flush_status = flushing})->
	?LOG_ERROR("[error]async_import ~p:~p~n",[RootId,File]),
	clear_root(RootId),
	{noreply, State#state{flush_status = idle}};
handle_info({async_import,{error,RootId,File}},State=#state{flush_status = idle})->
	?LOG_ERROR("[error]async_import ~p:~p~n",[RootId,File]),
	{noreply, State};
handle_info({'same_table_conflicted',TabId0},State)->
	?LOG_ERROR("[error]same_table_conflicted :~p~n",[TabId0]),
	ets:delete(TabId0),
	{noreply, State};
handle_info({'ETS-TRANSFER',_Tab,_FromPid,{'PUSH TABLE OK',RootId,TableName,TabId,FieldList}},State)->
	ets:insert(RootId,{TableName,TabId,FieldList}),
	{noreply, State};
handle_info(?CHECK_CONFIG_DATA,
	State=#state{
		table_transform = TableTrans,flush_status = idle,
		dir=Dir,file_list_table = FilesTime,data_import = ImportHooks,
		table_matches = TableMatches,field_matches = FieldMatches,
		header_define = HeaderDefs,record_with_table = RecordWithTable})->
	Flushing = ansyc_flush_dir(Dir,FilesTime,TableTrans,TableMatches,FieldMatches,ImportHooks,HeaderDefs,RecordWithTable),
	NewState = if Flushing -> State#state{flush_status = flushing} ;
				  true-> State
				  end,
	erlang:send_after(?CHECK_INTERVAL,self(),?CHECK_CONFIG_DATA),
	{noreply,NewState};
handle_info(?CHECK_CONFIG_DATA,State=#state{flush_status = flushing})->
	erlang:send_after(?CHECK_INTERVAL,self(),?CHECK_CONFIG_DATA),
	{noreply,State};
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
ansyc_flush_dir(Dir,OldFilesTime,TableTrans,TableMatches,FieldMatches,ImportHooks,HeaderDefs,RecordWithTable)->
	case check_timeout(Dir,OldFilesTime) of
		false-> false;
		true->
			RootId = ets:new(?RUNNING_TABLES_TABLE, [set, protected,{read_concurrency,true}]),

			Wild = filename:absname_join(Dir,"*.{xlsx,xlsm}"),
			Files0 = filelib:wildcard(Wild),
			Files =lists:filter(
				fun(F)->
					case filename:basename(F) of
						"~$"++_-> false;
						_-> true
					end
				end,Files0),
			put({filetime,RootId},[]),
			put({filecount,RootId},length(Files)),

			lists:foreach(
				fun(File)->
					proc_lib:spawn_link(?MODULE,import_file_async,[self(),File, RootId,TableTrans,TableMatches,FieldMatches,ImportHooks,HeaderDefs,RecordWithTable]),
					?LOG_INFO("[ok]starting import file:~ts~n",[File])
				end,Files),
			true
	end.
-spec import_file_async(FromPid::pid(),File::string(), RootId::ets:tid() ,TableTrans ::ag_metable_hook:trans_type(), TableMatches::ag_metable_hook:hook_match(), FieldMatches::ag_metable_hook:hook_match(), ImportHooks::term(), HeaderDefs::[term()],RecordWithTable::[atom()]) -> ok.
import_file_async(FromPid,File, RootId,TableTrans, TableMatches, FieldMatches, ImportHooks, HeaderDefs,RecordWithTable) ->
	try
		import_file(FromPid,File, RootId, TableTrans, TableMatches  , FieldMatches, ImportHooks, HeaderDefs,RecordWithTable),
		FromPid ! {async_import, {ok, RootId,File}}
	catch E:R:S ->
			?LOG_ERROR("[error]import_file:~p:~p:~p~n", [E, R,S]),
			FromPid ! {async_import, {error,RootId, File}}
	  end.

check_timeout(Dir,OldFilesTime)->
	Wild = filename:absname_join(Dir,"*.{xlsx,xlsm}"),
	Files = filelib:wildcard(Wild),
	NewFileTime = lists:filtermap(
		fun(File)->
			case filename:basename(File) of
				"~$"++_->
					false;
				_Base ->
					FileTime = ag_metable_util:get_fileunixtime(File),
					{true,{File,FileTime}}
			end
		end,Files),
	Diff = (OldFilesTime--NewFileTime) ++ (NewFileTime -- OldFilesTime),
	if length(Diff) >0 -> true;
		true-> false
	end.

clear_root(RootId)->
	TabList = ets:tab2list(RootId),
	lists:foreach(fun({_TabName,TabId,_})->ets:delete(TabId)end,TabList),
	ets:delete(RootId).


is_record_table(Table,RecordWithTable) when is_binary(Table)->
	is_record_table(binary_to_atom(Table,utf8),RecordWithTable);
is_record_table(Table,RecordWithTable) when is_list(Table)->
	is_record_table(list_to_atom(Table),RecordWithTable);
is_record_table(Table,RecordWithTable) when is_atom(Table)->
	lists:any(
		fun(T) when is_atom(T)->
			T == Table;
			(T) when is_list(T)->
			list_to_atom(T) == Table;
			(T) when is_binary(T)->
			binary_to_atom(T,utf8) == Table
		end,RecordWithTable).

import_file(FromPid,File,RootId,TableTrans,TableMatches,FieldMatches,ImportHooks, HeaderDefs,RecordWithTable)->
	{descript_row,DescRow} = lists:keyfind(descript_row,1,HeaderDefs),
	{name_row,NameRow} = lists:keyfind(name_row,1,HeaderDefs),
	{type_row,TypeRow} = lists:keyfind(type_row,1,HeaderDefs),
	{date_row,DataRow} = lists:keyfind(date_row,1,HeaderDefs),

	NameImport = case lists:keyfind(name_import,1,HeaderDefs) of
		false-> true;
		{ name_import, Boolean}-> Boolean
	end,

	LineFun =
		fun
			(ThisTab, [RowIndex | Row], undefined)  when RowIndex < DataRow ->
				TabName = list_to_atom(ThisTab),
				OutputTable = ag_metable_hook:table_transform(File,TabName,TableTrans),
				case ag_metable_hook:table_match(File,OutputTable,TableMatches) of
					true ->
						WithTable = is_record_table(OutputTable,RecordWithTable),
						NewRow = if
									 WithTable ->
										 [?REVERS_FIELD_NAME_FOR_WITH_TABLE|Row];
									 true-> Row
						end,

						Fields =
							if DescRow==RowIndex->
									process_header_descript([],NewRow);
								NameRow==RowIndex->
									process_header_name([],NewRow,File,OutputTable,FieldMatches);
								TypeRow==RowIndex->
									process_header_type([],NewRow);
								true->
									[]
							end,
						EtsOptions = case WithTable of
										true->[set, protected, {read_concurrency, true}, {keypos, 2}];
										 false->[set, protected, {read_concurrency, true}]
									 end,
						NewTabId = ets:new(TabName, EtsOptions),
						NewContext = #table_header{table = TabName, fields = Fields, tabid = NewTabId,with_table = WithTable},
						{next_row, NewContext};
					false ->
						{next_sheet, undefined}
				end;
			(ThisTab, [RowIndex | Row], Context=#table_header{table = LastTable, tabid = TabId, fields = FieldList})  when RowIndex < DataRow ->
				ThisAtom = list_to_atom(ThisTab),
				NewFieldList =
					if ThisAtom =/= LastTable ->
						NewFieldList0 = if NameImport-> filter_noname(FieldList); true-> FieldList end,
						push_table(FromPid,File,RootId, LastTable,TableTrans, TabId, get_import_hook_fields(LastTable,NewFieldList0,ImportHooks)),
						[]; %%如果是第一行push上一个表
					true ->
						FieldList
				end,
				TabName = list_to_atom(ThisTab),
				OutputTable = ag_metable_hook:table_transform(File,TabName,TableTrans),
				WithTable = is_record_table(OutputTable,RecordWithTable),

				case ag_metable_hook:table_match(File,OutputTable,TableMatches) of
					true ->
						NewRow = if
									 WithTable ->
										 [?REVERS_FIELD_NAME_FOR_WITH_TABLE|Row];
									 true-> Row
								 end,
						Fields =
							if DescRow==RowIndex->
								process_header_descript(NewFieldList,NewRow);
								NameRow==RowIndex->
									process_header_name(NewFieldList,NewRow,File,OutputTable,FieldMatches);
								TypeRow==RowIndex->
									process_header_type(NewFieldList,NewRow);
								true->
									[]
							end,
						NewContext =
							if RowIndex == 1 ->		%%如果是第一行new一个新表
								EtsOptions = case WithTable of
												 true->[set, protected, {read_concurrency, true}, {keypos, 2}];
												 false->[set, protected, {read_concurrency, true}]
											 end,
									NewTabId = ets:new(TabName, EtsOptions),
									Context#table_header{table = TabName, fields = Fields, tabid = NewTabId,with_table = WithTable};
								true ->
									Context#table_header{fields = Fields}
							end,
						{next_row, NewContext};
					false ->
						?LOG_INFO("[ignore]:~p~n",[ThisTab]),
						{next_sheet, undefined}
				end;
			(ThisTab, [RowIndex | Row], Context = #table_header{fields = Fields, tabid = TabId,table = TabName,with_table = WithTable}) when RowIndex>=DataRow ->
				NewFieldList0 = if NameImport-> filter_noname(Fields); true-> Fields end,
				case lists:all(fun([])-> true;(_)-> false end,Row) of
					true->
						?LOG_WARNING("[warning]~s ,[~p]empty line is importing~n",[TabName,RowIndex]);
					false->
						if WithTable->
							OutputTable = ag_metable_hook:table_transform(File,ThisTab,TableTrans),
							process_data(TabName,TabId, NewFieldList0,get_table_import_hook(TabName,ImportHooks), RowIndex, [OutputTable|Row]);
						true->
							process_data(TabName,TabId, NewFieldList0,get_table_import_hook(TabName,ImportHooks), RowIndex, Row)
						end
				end,
				{next_row, Context};
			(_,[_RowIndex | _Row],C)->
				{next_row, C}
		end,
	case ag_excel_reader:read(File,undefined,LineFun) of
		#table_header{table = TableName1,tabid = TabId1,fields = FieldList} ->
			NewFieldList0 = if NameImport-> filter_noname(FieldList); true-> FieldList end,
			push_table(FromPid,File,RootId,TableName1,TableTrans,TabId1,get_import_hook_fields(TableName1,NewFieldList0,ImportHooks));
		undefined->
			nothing;
		{error,Error}->
			error(Error)
	end.

push_table(FromPid,File,RootId,TableName,TableTrans,TabId,FieldList)->
	NewTabName = ag_metable_hook:table_transform(File,TableName,TableTrans),
	case ets:lookup(RootId,NewTabName) of
		[]-> nothing;
		[{TableName0,TabId0,_}]->
			?LOG_ERROR("[error]same table!!!!==>>~ts<<==!!!!!!!!~n",[TableName0]),
			if
				FromPid =/= undefined-> FromPid ! {'same_table_conflicted',TabId0};
				true->
					ets:delete(TabId0)
			end
	end,

	if
		FromPid =/= undefined->
			ets:give_away(TabId,FromPid,{'PUSH TABLE OK',RootId,NewTabName,TabId,FieldList});
		true->
			ets:insert(RootId,{NewTabName,TabId,FieldList}),
			nothing
	end,
	?LOG_INFO("[ok]:~ts @ ~ts~n",[NewTabName,File]).

process_header_name(HeaderInfos,Row,File,OutputTable,FieldMatches)->
	NewRow = lists:zip(lists:seq(1,length(Row)),Row),
	HeaderInfos2 = lists:foldl(
		fun({_,[]},HInfos)->
			HInfos;
			({Indx,?REVERS_FIELD_NAME_FOR_WITH_TABLE},HInfos)->
				NewFieldInfo =#table_field{index =Indx,name = ?REVERS_FIELD_NAME_FOR_WITH_TABLE,enabled = true,type = atom},
				lists:keystore(Indx,#table_field.index,HInfos,NewFieldInfo);
			({Indx,Cell},HInfos)->
				CellEnabled= ag_metable_hook:field_match(File,OutputTable,Cell,FieldMatches),
				NewFieldInfo =
					case lists:keyfind(Indx,#table_field.index,HInfos) of
						false-> #table_field{index =Indx,name = Cell,enabled = CellEnabled};
						XlsxField-> XlsxField#table_field{name = Cell,enabled = CellEnabled}
					end,
				lists:keystore(Indx,#table_field.index,HInfos,NewFieldInfo)
		end,HeaderInfos,NewRow),

	{HeaderInfos4,_} = lists:foldl(
		fun(#table_field{enabled = true}=Field,{HeaderInfos3,Index})->
			{[Field#table_field{column = Index}|HeaderInfos3],Index+1};
			(_,{HeaderInfos3,Index})->{HeaderInfos3,Index}
		end,{[],1},HeaderInfos2),
	HeaderInfos4.

string_to_type([]) ->
	string;
string_to_type("integer") ->
	integer;
string_to_type("int") ->
	integer;
string_to_type("int_list") ->
	integer_list;
string_to_type("integer_list") ->
	integer_list;
string_to_type("float") ->
	float;
string_to_type("float_list") ->
	float_list;
string_to_type("tuple") ->
	tuple;
string_to_type("tuple_list") ->
	tuple_list;
string_to_type("string") ->
	string;
string_to_type("string_list") ->
	string_list;
string_to_type("atom") ->
	atom;
string_to_type("atom_list") ->
	atom_list;
string_to_type("binary") ->
	binary;
string_to_type("binary_list") ->
	binary_list;
string_to_type("list") ->
	list;
string_to_type("json") ->
	json;
string_to_type(_T) ->
	unknown.

process_header_type(HeaderInfos,Row)->
	NewRow = lists:zip(lists:seq(1,length(Row)),Row),
	lists:foldl(
		fun({_,[]},HInfos)->
			HInfos;
			({Indx,?REVERS_FIELD_NAME_FOR_WITH_TABLE},HInfos)->
				NewFieldInfo =#table_field{index =Indx,name = ?REVERS_FIELD_NAME_FOR_WITH_TABLE,enabled = true,type = atom},
				lists:keystore(Indx,#table_field.index,HInfos,NewFieldInfo);

			({Indx,Cell},HInfos)->
				NewFieldInfo =
					case lists:keyfind(Indx,#table_field.index,HInfos) of
						false-> #table_field{index =Indx,type = string_to_type(Cell)};
						XlsxField-> XlsxField#table_field{type = string_to_type(Cell)}
					end,
				lists:keystore(Indx,#table_field.index,HInfos,NewFieldInfo)
		end,HeaderInfos,NewRow).

process_header_descript(HeaderInfos,Row)->
	NewRow = lists:zip(lists:seq(1,length(Row)),Row),
	lists:foldl(
		fun({_,[]},HInfos)->
			HInfos;
			({Indx,?REVERS_FIELD_NAME_FOR_WITH_TABLE},HInfos)->
				NewFieldInfo =#table_field{index =Indx,name = ?REVERS_FIELD_NAME_FOR_WITH_TABLE,enabled = true,type = atom},
				lists:keystore(Indx,#table_field.index,HInfos,NewFieldInfo);

			({Indx,Cell},HInfos)->
				NewFieldInfo =
					case lists:keyfind(Indx,#table_field.index,HInfos) of
						false-> #table_field{index =Indx,descript = Cell};
						XlsxField-> XlsxField#table_field{descript = Cell}
					end,
				lists:keystore(Indx,#table_field.index,HInfos,NewFieldInfo)
		end,HeaderInfos,NewRow).


process_data(_,_Tab,[],_HookInfos,_,_)->
	ok;
process_data(TableName,Tab, FieldInfos,HookInfos,Line,Row)->
	NewRow = lists:zip(lists:seq(1, length(Row)), Row),
	EtsLine = lists:filtermap(
		fun({I, Cell})->
			case lists:keyfind(I, #table_field.index, FieldInfos) of
				false->
					false;
				#table_field{enabled = false}->
					false;
				#table_field{type = integer}->
					try
						Integer = list_to_integer(binary_to_list(unicode:characters_to_binary(Cell))),
						{true, Integer}
					catch _E:_R:_S ->
						agb_error:error("invalidate integer:~s(~p,~p)",[TableName,Line,I])
					end;
				#table_field{type = integer_list}->
					{ok,List} = ag_metable_util:string_to_integer_list(Cell),
					case ag_metable_util:check_list(List,integer) of
						true-> {true, List};
						false-> agb_error:error("invalidate integer list:~s(~p,~p)",[TableName,Line,I])
					end;

				#table_field{type = float}->
					case ag_metable_util:string_to_float(Cell) of
						{ok,Flt}->{true,Flt};
						{error,_}->agb_error:error("invalidate float list:~s(~p,~p)",[TableName,Line,I])
					end;
				#table_field{type = float_list}->
					{ok,List} = ag_metable_util:string_to_float_list(Cell),
					case ag_metable_util:check_list(List,float) of
						true-> {true, List};
						false-> agb_error:error("invalidate float list:~s(~p,~p)",[TableName,Line,I])
					end;

				#table_field{type = string}->
					{true, binary_to_list(unicode:characters_to_binary(Cell))};
				#table_field{type = string_list}->
					{ok,List} = agb_string:string_to_term(ag_metable_util:normalize_list_string(binary_to_list(unicode:characters_to_binary(Cell)))),
					case ag_metable_util:check_list(List,string) of
						true-> {true, List};
						false-> agb_error:error("invalidate string list:~s(~p,~p)",[TableName,Line,I])
					end;

				#table_field{type = atom}->
					{true, list_to_atom(Cell)};
				#table_field{type = atom_list}->
					{ok,List} = agb_string:string_to_term(ag_metable_util:normalize_list_string(Cell)),
					case ag_metable_util:check_list(List,atom) of
						true-> {true, List};
						false-> agb_error:error("invalidate atom list:~s(~p,~p)",[TableName,Line,I])
					end;
				#table_field{type = binary}->
					{true, unicode:characters_to_binary(Cell)};
				#table_field{type = binary_list}->
					C0 = binary_to_list(unicode:characters_to_binary(Cell)),
					{ok,List} = agb_string:string_to_term(C0),
					case ag_metable_util:check_list(List,binary) of
						true-> {true, List};
						false-> agb_error:error("invalidate binary list:~s(~p,~p)",[TableName,Line,I])
					end;
				#table_field{type = tuple}->
					{ok,Tuple} = agb_string:string_to_term(binary_to_list(unicode:characters_to_binary(Cell))),
					if is_tuple(Tuple)->
						{true, Tuple};
						true->
							agb_error:error("invalidate tuple :~s(~p,~p)",[TableName,Line,I])
					end;
				#table_field{type = tuple_list}->
					{ok,List} = agb_string:string_to_term(ag_metable_util:normalize_list_string(binary_to_list(unicode:characters_to_binary(Cell)))),
					case ag_metable_util:check_list(List,tuple) of
						true-> {true, List};
						false-> agb_error:error("invalidate tuple list:~s(~p,~p)",[TableName,Line,I])
					end;
				#table_field{type = json}->
					JsonTxt = unicode:characters_to_binary(Cell),
					try
						JsonObj = jsx:decode(JsonTxt, [return_maps]),
						{true,JsonObj}
					catch _E:_R:_S ->
							agb_error:error("invalidate json text:~s(~p,~p)",[TableName,Line,I])
					end;
				#table_field{type = list}->
					{ok,List} = agb_string:string_to_term(ag_metable_util:normalize_list_string(binary_to_list(unicode:characters_to_binary(Cell)))),
					case ag_metable_util:check_list(List,any) of
						true-> {true, List};
						false-> agb_error:error("invalidate any list:~s(~p,~p)",[TableName,Line,I])
					end;
				#table_field{type = unknown}->
					agb_error:error("can not process unknown type:~s(~p,~p)",[TableName,Line,I])
			end
		end, NewRow),
	case ag_metable_hook:process_import(EtsLine,HookInfos, FieldInfos) of
		[]-> not_import;
		NewEtsLine ->
			ets:insert(Tab, list_to_tuple(NewEtsLine))
	end.
%%
%%match_name([],_Name)->
%%	false;
%%match_name("*",_Name)->
%%	true;
%%match_name(["*"|_Rules],_Name)->
%%	true;
%%match_name([{prefix,Param}|Rules],Name)->
%%	Param == string:sub_string(Name, 1, string:length(Param)) orelse match_name(Rules,Name);
%%match_name([{suffix,Param}|Rules],Name)->
%%	Param == string:sub_string(Name, string:length(Name)-string:length(Param)+1, string:length(Name)) orelse match_name(Rules,Name);
%%match_name([{include,Param}|Rules],Name)->
%%	string:str(Name,Param)>0 orelse match_name(Rules,Name);
%%match_name([{equal,Param}|Rules],Name)->
%%	Name=:=Param orelse match_name(Rules,Name);
%%
%%match_name([{notprefix,Param}|_Rules],Name)-> %%
%%	Param /= string:sub_string(Name, 1, string:length(Param));
%%match_name([{notsuffix,Param}|_Rules],Name)->
%%	Param /= string:sub_string(Name, string:length(Name)-string:length(Param)+1, string:length(Name)) ;
%%match_name([{notinclude,Param}|_Rules],Name)->
%%	string:str(Name,Param)=<0;
%%match_name([{notequal,Param}|_Rules],Name)->
%%	Name=/=Param;
%%
%%match_name([Rule|_Rules],Name)->
%%	Exception = agb_string:sprintf("not support rule :~p @ name:~s",[Rule,Name]),
%%	error(Exception).


get_table_import_hook(TableName,ImportHooks)when is_atom(TableName)->
	case lists:keyfind(TableName,1,ImportHooks) of
		false-> [];
		{_T,TabHookInfo}->
			TabHookInfo
	end.

get_import_hook_fields(TableName,FieldInfos,ImportHooks) when is_atom(TableName)->
	case lists:keyfind(TableName,1,ImportHooks) of
		false-> FieldInfos;
		{_T,TabHookInfo}->
			case do_add_hook_fields([],length(FieldInfos)+1,count_enabled_fields(FieldInfos)+1,TabHookInfo) of
				[]->FieldInfos;
				HookFields->
					FieldInfos++ lists:reverse(HookFields)
			end
	end.


count_enabled_fields(FieldInfos)->
	length(lists:filter(fun(#table_field{enabled = true})-> true;(_)-> false end,FieldInfos)).

do_add_hook_fields(FieldInfos,_Index,_Column,[])->
	FieldInfos;
do_add_hook_fields(FieldInfos,Index,Column,[{FieldName,_}|HookInfos])->
	Field = #table_field{index = Index,column = Column,name = FieldName,type=hook,enabled = true},
	do_add_hook_fields([Field| FieldInfos],Index+1,Column+1,HookInfos).



filter_noname(FieldInfos)->
	lists:filter(fun(#table_field{name = []})-> false;(_)-> true  end, FieldInfos).
