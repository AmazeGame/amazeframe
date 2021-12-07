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
-module(ag_metable_util).
-include_lib("kernel/include/file.hrl").
-include("ag_metable.hrl").
%% API
-export([check_list/2]).
-export([string_to_float/1,string_to_integer/1,string_to_integer_list/1,string_to_float_list/1]).
-export([normalize_list_string/1]).
-export([get_fileunixtime/1]).
-export([column_from_field/2]).
-export([index_from_field/2]).
-export([field_from_column/2]).
-export([to_value/2]).

-spec check_list(List::list(),atom()) -> boolean().
check_list(List,integer)->
	lists:all(fun(I)-> is_integer(I) end,List);
check_list(List,atom)->
	lists:all(fun(I)-> is_atom(I) end,List);
check_list(List,string)->
	lists:all(fun(I)-> is_list(I) end,List);
check_list(List,float)->
	lists:all(fun(I)-> is_integer(I) or is_float(I) end,List);
check_list(List,tuple)->
	lists:all(fun(I)-> is_tuple(I)  end,List);
check_list(List,binary)->
	lists:all(fun(I)-> is_binary(I)  end,List);
check_list(List,any)->
	is_list(List).

-spec string_to_float(String::string())-> {ok, float()}|{error,term()}.
string_to_float(String)->
	{ok,Flt} = agb_string:string_to_term(String),
	if is_integer(Flt) -> {ok,Flt};
		is_float(Flt)-> {ok,Flt};
		true-> agb_error:error("string to float:~p",[String])
	end.

-spec string_to_integer(String::string())-> {ok, integer()}.
string_to_integer(String)->
	{ok,Integer} = agb_string:string_to_term(String),
	if is_integer(Integer) -> {ok,Integer};
		is_float(Integer)-> {ok,trunc(Integer+0.5)};
		true-> agb_error:error("string to integer:~p",[String])
	end.

-spec string_to_integer_list(String::string())-> {ok, list()}.
string_to_integer_list(String)->
	{ok,IntList} = agb_string:string_to_term(normalize_list_string(String)),
	Ints= lists:map(
		fun(Int) when is_integer(Int)-> Int;
		   (Int) when is_float(Int)-> trunc(Int+0.5);
		   (_Int) -> agb_error:error("string to integer list:~s",[String])
		end,IntList),
	{ok,Ints}.

-spec string_to_float_list(String::string())-> {ok, list()}.
string_to_float_list(String)->
	{ok,FltList} = agb_string:string_to_term(normalize_list_string(String)),
	Flts = lists:map(
		fun(Flt) when is_integer(Flt)-> Flt;
			(Flt) when is_float(Flt)-> Flt;
			(_Flt) -> agb_error:error("string to float list:~s",[String])
		end,FltList),
	{ok,Flts}.

-spec get_fileunixtime(File::file:name_all())-> integer()|{error,0} .
get_fileunixtime(File)->
	case file:read_file_info(File,[{time, universal}]) of
		{ok, FileInfo}->
			LastModify = FileInfo#file_info.mtime,
			calendar:datetime_to_gregorian_seconds(LastModify);
		{error,_Reason}-> {error,0}
	end.

normalize_list_string([$[|String])->
	right_normalize_list_string([$[|String]);
normalize_list_string(String)->
	right_normalize_list_string([$[|String]).

right_normalize_list_string(String)->
	case lists:last(String) of
		$]-> String;
		_->  String ++"]"
	end.

compare_nocase(String1,String2)->
	S1 = string:to_lower(String1),
	S2 = string:to_lower(String2),
	if S1 > S2-> 1 ;
		S1 == S2 -> 0;
		true-> -1
	end.

keyfind_nocase(Key, N, TupleList)->
	lists:foldl(
		fun(Tuple,false)->
			Elem = element(N,Tuple),
			case compare_nocase(Elem,Key) of
				0-> Tuple;
				_-> false
			end;
			(_,Tuple)-> Tuple
		end,false,TupleList).

column_from_field(Field,FieldInfos)->
	case keyfind_nocase(Field,#table_field.name,FieldInfos) of
		false->
			0;
		#table_field{column = Column}->
			Column
	end.

index_from_field(Field,FieldInfos)->
	case keyfind_nocase(Field,#table_field.name,FieldInfos) of
		false->
			0;
		#table_field{index = Index}->
			Index
	end.

field_from_column(Column,FieldInfos)->
	case keyfind_nocase(Column,#table_field.column,FieldInfos) of
		false->
			0;
		#table_field{name = Name}->
			Name
	end.

-spec to_value(String::string(),Type::string())->any().
to_value(String,Type) when is_binary(String)->
	to_value(binary_to_list(String),Type);
to_value(String,Type) when is_binary(Type)->
	to_value(String,binary_to_list(Type));
to_value(String,"integer") ->
	list_to_integer(String);
to_value(String,"int") ->
	list_to_integer(String);
to_value(String,"integer_list") ->
	{ok,List} = ag_metable_util:string_to_integer_list(String),
	case ag_metable_util:check_list(List,integer) of
		true->List;
		false-> agb_error:error("invalidate integer list:(~p)",[String])
	end;
to_value(String,"int_list") ->
	{ok,List} = ag_metable_util:string_to_integer_list(String),
	case ag_metable_util:check_list(List,integer) of
		true-> List;
		false-> agb_error:error("invalidate integer list:(~p)",[String])
	end;
to_value(String,"float") ->
	case ag_metable_util:string_to_float(String) of
		{ok,Flt}->Flt;
		{error,_}->agb_error:error("invalidate float list:(~p)",[String])
	end;
to_value(String,"float_list") ->
	{ok,List} = ag_metable_util:string_to_float_list(String),
	case ag_metable_util:check_list(List,float) of
		true-> List;
		false-> agb_error:error("invalidate float list:(~p)",[String])
	end;
to_value(String,"tuple") ->
	{ok,Tuple} = agb_string:string_to_term(binary_to_list(unicode:characters_to_binary(String))),
	if is_tuple(Tuple)->
		Tuple;
		true->
			agb_error:error("invalidate tuple :(~p)",[String])
	end;
to_value(String,"tuple_list") ->
	{ok,List} = agb_string:string_to_term(ag_metable_util:normalize_list_string(binary_to_list(unicode:characters_to_binary(String)))),
	case ag_metable_util:check_list(List,tuple) of
		true-> List;
		false-> agb_error:error("invalidate tuple list:(~p)",[String])
	end;
to_value(String,"string") ->
	if
		is_list(String) -> String;
		true ->  agb_error:error("invalidate string:(~p)",[String])
	end,
	String;
to_value(String,"string_list") ->
	{ok,List} = agb_string:string_to_term(ag_metable_util:normalize_list_string(binary_to_list(unicode:characters_to_binary(String)))),
	case ag_metable_util:check_list(List,string) of
		true-> List;
		false-> agb_error:error("invalidate string list:(~p)",[String])
	end;
to_value(String,"atom") ->
	list_to_atom(String);
to_value(String,"atom_list") ->
	{ok,List} = agb_string:string_to_term(ag_metable_util:normalize_list_string(String)),
	case ag_metable_util:check_list(List,atom) of
		true-> List;
		false-> agb_error:error("invalidate atom list:(~p)",[String])
	end;
to_value(String,"binary") ->
	unicode:characters_to_binary(String);
to_value(String,"binary_list") ->
	C0 = binary_to_list(unicode:characters_to_binary(String)),
	{ok,List} = agb_string:string_to_term(C0),
	case ag_metable_util:check_list(List,binary) of
		true-> List;
		false-> agb_error:error("invalidate binary list:(~p)",[String])
	end;
to_value(String,"list") ->
	{ok,List} = agb_string:string_to_term(ag_metable_util:normalize_list_string(binary_to_list(unicode:characters_to_binary(String)))),
	case ag_metable_util:check_list(List,any) of
		true-> List;
		false-> agb_error:error("invalidate any list:(~p)",[String])
	end;
to_value(String,"json") ->
	JsonTxt = unicode:characters_to_binary(String),
	try
		JsonObj = jsx:decode(JsonTxt, [return_maps]),
		JsonObj
	catch
		_E:_R:_S  ->agb_error:error("invalidate json text:(~p)",[String])
	end.