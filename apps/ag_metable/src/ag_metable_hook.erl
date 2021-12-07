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
-module(ag_metable_hook).
-author("Adrianx Lau <adrianx.lau@gmail.com>").


%% API
-export([table_transform/3,table_match/3,field_match/4,process_import/3]).
-type trans_type() :: undefined | trans_upper | trans_lower |
						{trans_split_head,string()} | {trans_split_tail,string()} |{trans_split_2nd,string()} |
						{trans_prefix, string()} | {trans_suffix,string()} |
						{trans_join_ft, string()} | {trans_join_tf, string()} |
						{module(),function()}.

-type hook_match() :: all_match |
						{prefix_match ,string()}|
						{suffix_match,string()} |
						{include_match,string()}|
						{exclude_match,string()}|
						{full_match,string() }  |

						{prefix_unmatch ,string()}|
						{suffix_unmatch,string()} |
						{include_unmatch,string()}|
						{exclude_unmatch,string()}|

						{module(),function()}.

-type import_info() :: {string(), {module(), function(), [string()],[string()]}|{to_value, string(), string()}|{script, string(), [string()]}}.
-type import_hook() :: {string(), [import_info()]}.

-export_type([trans_type/0,hook_match/0,import_hook/0,import_info/0]).
-spec table_transform(FileName::string(),Table::string()|atom(),TableTransform::trans_type())->string()|atom().
table_transform(_FileName,Table,undefined) ->
	Table;
table_transform(FileName,Table,TableTransform) when is_atom(Table)->
	list_to_atom(table_transform(FileName,atom_to_list(Table),TableTransform));
table_transform(FileName,Table,TableTransform) when is_binary(Table)->
	list_to_binary(table_transform(FileName,binary_to_list(Table),TableTransform));
table_transform(FileName,Table,TableTransform) when is_list(Table)->
	do_table_transform(FileName,Table,TableTransform).

do_table_transform(_FileName,Table,trans_upper)->
	trans_upper(Table);
do_table_transform(_FileName,Table,trans_lower)->
	trans_lower(Table);
do_table_transform(_FileName,Table,{trans_split_head,Token})->
	trans_split_head(Table,Token);
do_table_transform(_FileName,Table,{trans_split_tail,Token})->
	trans_split_tail(Table,Token);
do_table_transform(_FileName,Table,{trans_split_2nd,Token})->
	trans_split_2nd(Table,Token);

do_table_transform(_FileName,Table,{trans_prefix,Prefix})->
	trans_prefix(Table,Prefix);
do_table_transform(_FileName,Table,{trans_suffix,Suffix})->
	trans_suffix(Table,Suffix);

do_table_transform(FileName,Table,{trans_join_ft,Joinit})->
	trans_join(Joinit,FileName,Table);

do_table_transform(FileName,Table,{trans_join_tf,Joinit})->
	trans_join(Joinit,Table,FileName);

do_table_transform(FileName,Table,{Module,Function})->
	erlang:apply(Module,Function,[FileName,Table]).

trans_upper(Table)->
	string:to_upper(Table).

trans_lower(Table)->
	string:to_lower(Table).

trans_prefix(Table,Prefix)->
	string:concat(Prefix,Table).

trans_suffix(Table,Suffix)->
	string:concat(Table,Suffix).

trans_join(Joinit,String1,String2)->
	lists:flatten(lists:join(Joinit,[String1,String2])).

trans_split_head(Table,Token)->
	[Head|_Tail] = agb_string:token_str(Table,Token),
	Head.

trans_split_tail(Table,Token)->
	Tokens = agb_string:token_str(Table,Token),
	lists:last(Tokens).

trans_split_2nd(Table,Token)->
	[_Head,NHead|_Tail] = agb_string:token_str(Table,Token),
	NHead.

-spec table_match(File::string(),Table::string()|atom(),Matches::undefined|[hook_match()])->boolean().
table_match(_FileName,_Table,undefined)->
	true;
table_match(_FileName,_Table,[])->
	false;
table_match(FileName,Table,Matches) when is_atom(Table)->
	table_match(FileName,atom_to_list(Table),Matches);
table_match(FileName,Table,Matches) when is_binary(Table)->
	table_match(FileName,binary_to_list(Table),Matches);
table_match(FileName,Table,[Match|Matches]) when is_list(Table)->
	do_table_match(FileName,Table,Match) orelse table_match(FileName,Table,Matches).

do_table_match(_FileName,_Table,all_match)->
	true;
do_table_match(_FileName,Table,{prefix_match,Prefix})->
	Prefix == string:sub_string(Table, 1, string:length(Prefix));
do_table_match(_FileName,Table,{suffix_match,Suffix})->
	Suffix == string:sub_string(Table, string:length(Table)-string:length(Suffix)+1, string:length(Table));
do_table_match(_FileName,Table,{include_match,Include})->
	string:str(Table,Include)>0;
do_table_match(_FileName,Table,{exclude_match,Exclude})->
	string:str(Table,Exclude)==0;
do_table_match(_FileName,Table,{full_match,Full})->
	Table == Full;
do_table_match(_FileName,Table,{prefix_unmatch,Prefix})->
	Prefix =/= string:sub_string(Table, 1, string:length(Prefix));
do_table_match(_FileName,Table,{suffix_unmatch,Suffix})->
	Suffix =/= string:sub_string(Table, string:length(Table)-string:length(Suffix)+1, string:length(Table));
do_table_match(_FileName,Table,{include_unmatch,Include})->
	string:str(Table,Include)=<0;
do_table_match(_FileName,Table,{exclude_unmatch,Exclude})->
	string:str(Table,Exclude)=/=0;
do_table_match(_FileName,Table,{full_unmatch,Full})->
	Table =/= Full;
do_table_match(FileName,Table,{Module,Function})->
	erlang:apply(Module,Function,[FileName,Table]);
do_table_match(_FileName,_Table,Match)->
	agb_error:error("table can not be matched by this match:~p~n",[Match]).

-spec field_match(File::string(),Table::string()|atom(),Field::string(),Matches::undefined|[hook_match()])->boolean().
field_match(_FileName,_Table,_Field,undefined)->
	true;
field_match(_FileName,_Table,_Field,[])->
	false;
field_match(FileName,Table,Field,Matches) when is_atom(Table)->
	field_match(FileName,atom_to_list(Table),Field,Matches);
field_match(FileName,Table,Field,Matches) when is_binary(Table)->
	field_match(FileName,binary_to_list(Table),Field,Matches);
field_match(FileName,Table,Field,[Match|Matches]) when is_list(Table)->
	do_field_match(FileName,Table,Field,Match) orelse field_match(FileName,Table,Field,Matches).

do_field_match(_FileName,_Table,_Field,all_match)->
	true;
do_field_match(_FileName,_Table,Field,{prefix_match,Prefix})->
	Prefix == string:sub_string(Field, 1, string:length(Prefix));
do_field_match(_FileName,_Table,Field,{suffix_match,Suffix})->
	Suffix == string:sub_string(Field, string:length(Field)-string:length(Suffix)+1, string:length(Field));
do_field_match(_FileName,_Table,Field,{include_match,Include})->
	string:str(Field,Include)>0;
do_field_match(_FileName,_Table,Field,{exclude_match,Exclude})->
	string:str(Field,Exclude)==0;
do_field_match(_FileName,_Table,Field,{full_match,Full})->
	Field == Full;
do_field_match(_FileName,_Table,Field,{prefix_unmatch,Prefix})->
	Prefix =/= string:sub_string(Field, 1, string:length(Prefix));
do_field_match(_FileName,_Table,Field,{suffix_unmatch,Suffix})->
	Suffix =/= string:sub_string(Field, string:length(Field)-string:length(Suffix)+1, string:length(Field));
do_field_match(_FileName,_Table,Field,{include_unmatch,Include})->
	string:str(Field,Include)=<0;
do_field_match(_FileName,_Table,Field,{exclude_unmatch,Exclude})->
	string:str(Field,Exclude)=/=0;
do_field_match(_FileName,_Table,Field,{full_unmatch,Full})->
	Field =/= Full;
do_field_match(FileName,Table,Field,{Module,Function})->
	erlang:apply(Module,Function,[FileName,Table,Field]);
do_field_match(_FileName,_Table,_Field,Match)->
	agb_error:error("field can not be matched by this match:~p~n",[Match]).

-spec process_import([term()],[import_info()],[table_field:table_field()])->[term()].
process_import(EtsLine,[],_)->
	EtsLine;
process_import(EtsLine,[{_NewField,{to_value,Value,Type}}|TabHookInfo],HeaderInfos)->
	NewValueType = hook_args(EtsLine,HeaderInfos,[Value,Type]),
	NewValue = erlang:apply(ag_metable_util,to_value,NewValueType),
	NewEtsLine = EtsLine ++ [NewValue],
	process_import(NewEtsLine,TabHookInfo,HeaderInfos);
process_import(EtsLine,[{_FieldName,{script,Script,FieldArgs}}|TabHookInfo],HeaderInfos)->
	FieldBinds = hook_binds(EtsLine,HeaderInfos,FieldArgs),
	NewValue = agb_string:eval_string(Script,FieldBinds),
	NewEtsLine = EtsLine ++ [NewValue],
	process_import(NewEtsLine,TabHookInfo,HeaderInfos);
process_import(EtsLine,[{_FieldName,{Module,Function,FieldArgs,UserParam}}|TabHookInfo],HeaderInfos)->
	NewFieldArgs = hook_args(EtsLine,HeaderInfos,FieldArgs),
	NewValue = erlang:apply(Module,Function,NewFieldArgs ++ UserParam),
	NewEtsLine = EtsLine ++ [NewValue],
	process_import(NewEtsLine,TabHookInfo,HeaderInfos);
process_import(EtsLine,[{_FieldName,{Module,Function}}|TabHookInfo],HeaderInfos)->
	{_,FieldValues} = lists:foldl(
		fun(LineValue,{Column,NewValues})->
			Field = ag_metable_util:field_from_column(Column,HeaderInfos),
			if(Field/=[])->
				{Column+1,[{Field,LineValue}|NewValues]};
				true->
					{Column+1,NewValues}
			end
		end,{1,[]},EtsLine),
	NewValue = erlang:apply(Module,Function,[lists:reverse(FieldValues)]),
	NewEtsLine = EtsLine ++ [NewValue],
	process_import(NewEtsLine,TabHookInfo,HeaderInfos).

hook_binds(Row,HeaderInfos,Args)->
	lists:foldl(
		fun(Arg,Acc)->
			case ag_metable_util:column_from_field(Arg,HeaderInfos) of
				0->
					Acc;
				Column->
					Value = lists:nth(Column,Row),
					erl_eval:add_binding(list_to_atom(Arg),Value,Acc)
			end
		end,erl_eval:new_bindings(),Args).

hook_args([],_,_)->
	agb_error:error(all_fields_are_filtered);
hook_args(Row,HeaderInfos,Args)->
	lists:map(
		fun(Arg)->
			case ag_metable_util:column_from_field(Arg,HeaderInfos) of
				0-> "";
				Column->
					lists:nth(Column,Row)
			end
		end,Args).