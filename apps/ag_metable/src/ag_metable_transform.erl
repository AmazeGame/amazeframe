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
-module(ag_metable_transform).

%% API
-export([parse_transform/2]).

-spec parse_transform([erl_parse:abstract_form()],compile:option())->[erl_parse:abstract_form()].
parse_transform(Ast,_Option)->
	walk_ast([], Ast).

walk_ast(Acc, []) ->
	lists:reverse(Acc);
walk_ast(Acc, [{function, Line, Name, Arity, Clauses}|T]) ->
	walk_ast([{function, Line, Name, Arity, add_try([],Clauses)}|Acc], T);
walk_ast(Acc, [H|L]) ->
	walk_ast([H|Acc], L).

add_try(Acc,[])->
	lists:reverse(Acc);
add_try(Acc, [{clause, Line, Arguments, [], Body} | NextClause]) ->
	NewBody = case lists:keyfind('Table',3 , Arguments) of
				  false -> Body;
				  {var, _VarLine, _Table} ->
					  add_try_block(Arguments,Body);
				  _ -> Body
			  end,

	add_try([{clause, Line, Arguments, [], NewBody} | Acc], NextClause);
add_try(Acc, [H|L]) ->
	add_try([H|Acc], L).


add_try_block(Arguments,Body0)->
	BLine = 1,
	EndTry = [{clause,BLine,
		[{tuple,BLine,[{var,BLine,'E'},{var,BLine,'R'},{var,BLine,'S'}]}],
		[],
		[
%%			{match,BLine,
%%			{var,BLine,'S'},
%%			{call,BLine,
%%				{remote,BLine,
%%					{atom,BLine,erlang},
%%					{atom,BLine,get_stacktrace}},
%%				[]}},

			{call,BLine,
				{remote,BLine,{atom,BLine,io},{atom,BLine,format}},
				[{string,BLine,"args:~p~nstacktrace:~p~n"},
					{cons,BLine,
						{tuple,BLine,Arguments},
						{cons,BLine,
							{tuple,BLine,[{var,BLine,'E'},{var,BLine,'R'},{var,BLine,'S'}]},
							{nil,BLine}}}]},
			{call,BLine,
				{remote,BLine,{atom,BLine,erlang},{atom,BLine,raise}},
				[{var,BLine,'E'},{var,BLine,'R'},{var,BLine,'S'}]}
			]}],

	[{'try',BLine,Body0,[],EndTry,[]}].
