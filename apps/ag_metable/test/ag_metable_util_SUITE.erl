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
-module(ag_metable_util_SUITE).

-import(ct_helper, [config/2]).
-include("ag_metable.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
%% API
-export([all/0,groups/0,init_per_group/2,end_per_group/2]).
-export([test_util/1,test_column_from_field/1,test_field_from_column/1,test_index_from_field/1,test_to_value/1]).

groups() ->
    [{basic, [shuffle], [
        test_column_from_field,
        test_index_from_field,
        test_field_from_column
    ]}].

all() ->
    [{group, basic},test_util,test_to_value].

init_per_group(_Group, Config) ->
    ct:pal("init_per_group ~p",[_Group]),
    Field = #table_field{column = 1,index = 10,name = "id",type = string},
    Field1 = #table_field{column = 2,index = 20,name = "name",type = string},
    Field2 = #table_field{column = 3,index = 30,name = "level",type = string},
    Fields=[Field,Field1,Field2],
    [{fields,Fields}|Config].

end_per_group(_Group, _Config) ->
    ok.

test_util(Config) ->
    List1 = [1,2,3,4,5,6],
    List2 = [a,b,c,d,e,f],
    List3 = ["a","b","c","d","e","f"],
    List4 = [1.0,2.0,3.0,4.0],
    List5 = [{a},{b},{c},{d},{e}],
    List6 = [<<"a">>,<<"b">>,<<"c">>,<<"d">>,<<"e">>],
    List7 = [1,n,<<"c">>,<<"d">>,<<"e">>],
    ?assertEqual(true,ag_metable_util:check_list(List1,integer)),
    ?assertEqual(true,ag_metable_util:check_list(List2,atom)),
    ?assertEqual(true,ag_metable_util:check_list(List3,string)),
    ?assertEqual(true,ag_metable_util:check_list(List4,float)),
    ?assertEqual(true,ag_metable_util:check_list(List5,tuple)),
    ?assertEqual(true,ag_metable_util:check_list(List6,binary)),
    ?assertEqual(true,ag_metable_util:check_list(List7,any)),

    ?assertEqual(false,ag_metable_util:check_list(List7,integer)),
    ?assertEqual(false,ag_metable_util:check_list(List7,atom)),
    ?assertEqual(false,ag_metable_util:check_list(List7,string)),
    ?assertEqual(false,ag_metable_util:check_list(List7,float)),
    ?assertEqual(false,ag_metable_util:check_list(List7,tuple)),
    ?assertEqual(false,ag_metable_util:check_list(List7,binary)),

    ?assertEqual({ok,3.1415},ag_metable_util:string_to_float("3.1415")),
    ?assertEqual({ok,31415},ag_metable_util:string_to_float("31415")),
    ?assertException(error,_,ag_metable_util:string_to_float("grt")),


    ?assertEqual({ok,3},ag_metable_util:string_to_integer("3.1415")),
    ?assertEqual({ok,4},ag_metable_util:string_to_integer("3.5415")),
    ?assertEqual({ok,31415},ag_metable_util:string_to_integer("31415")),
    ?assertException(error,_,ag_metable_util:string_to_integer("grt")),

    ?assertEqual("[1,2,3,4,5]",ag_metable_util:normalize_list_string("1,2,3,4,5")),
    ?assertEqual("[1,2,3,4,5]",ag_metable_util:normalize_list_string("[1,2,3,4,5")),
    ?assertEqual("[1,2,3,4,5]",ag_metable_util:normalize_list_string("1,2,3,4,5]")),
    ?assertEqual("[1,2,3,4,5]",ag_metable_util:normalize_list_string("[1,2,3,4,5]")),

    ?assertEqual({ok,[1,2,4,4,5]},ag_metable_util:string_to_integer_list("1,2.1,3.5,4,5")),
    ?assertException(error,_,ag_metable_util:string_to_integer_list("grt")),
    ?assertEqual({ok,[1,2.1,3.5,4,5]},ag_metable_util:string_to_float_list("1,2.1,3.5,4,5")),
    ?assertException(error,_,ag_metable_util:string_to_float_list("grt")),

    Dir = config(data_dir, Config) ++ "test.txt",
    ct:pal("DirDirDirDirDirDirDirDir~p",[Dir]),
    ?assertNotEqual( {error,0},ag_metable_util:get_fileunixtime(Dir)),
    ?assertEqual( {error,0},ag_metable_util:get_fileunixtime("test.txt")).



test_column_from_field(Config)->
    Fields = config(fields,Config),
    ?assertEqual(1, ag_metable_util:column_from_field("id",Fields)),
    ?assertEqual(2, ag_metable_util:column_from_field("Name",Fields)),
    ?assertEqual(3, ag_metable_util:column_from_field("level",Fields)).


test_index_from_field(Config)->
    Fields = config(fields,Config),
    ?assertEqual(10, ag_metable_util:index_from_field("id",Fields)),
    ?assertEqual(20, ag_metable_util:index_from_field("Name",Fields)),
    ?assertEqual(30, ag_metable_util:index_from_field("level",Fields)).

test_field_from_column(Config)->
    Fields = config(fields,Config),
    ?assertEqual("id", ag_metable_util:field_from_column(1,Fields)),
    ?assertEqual("name", ag_metable_util:field_from_column(2,Fields)),
    ?assertEqual("level", ag_metable_util:field_from_column(3,Fields)).

test_to_value(_Config)->
    ?assertEqual(100,ag_metable_util:to_value(<<"100">>,<<"integer">>)),
    ?assertEqual(100,ag_metable_util:to_value(<<"100">>,"integer")),
    ?assertEqual(100,ag_metable_util:to_value("100","integer")),
    ?assertEqual(100,ag_metable_util:to_value("100","int")),
    ?assertEqual([1,2,3,4],ag_metable_util:to_value("1,2,3,4","integer_list")),
    ?assertEqual([1,2,3,4],ag_metable_util:to_value("1,2,3,4","int_list")),

    ?assertException(error,_,ag_metable_util:to_value(<<"abc">>,<<"integer">>)),
    ?assertException(error,_,ag_metable_util:to_value("abc",<<"int">>)),
    ?assertException(error,_,ag_metable_util:to_value("aerfd",<<"integer_list">>)),
    ?assertException(error,_,ag_metable_util:to_value("abc",<<"int_list">>)),

    ?assertEqual(55.5,ag_metable_util:to_value("55.5","float")),
    ?assertEqual([55.5,60.2,78],ag_metable_util:to_value("55.5,60.2,78","float_list")),
    ?assertException(error,_,ag_metable_util:to_value(<<"abc">>,<<"float">>)),
    ?assertException(error,_,ag_metable_util:to_value("a,b,c",<<"float_list">>)),

    ?assertEqual({a,b,c},ag_metable_util:to_value("{a,b,c}","tuple")),
    ?assertEqual({a,#{a=>1},c},ag_metable_util:to_value("{a,#{a=>1},c}","tuple")),
    ?assertEqual([{a,b,c}],ag_metable_util:to_value("{a,b,c}","tuple_list")),
    ?assertEqual([{a,#{a=>1},c}],ag_metable_util:to_value("{a,#{a=>1},c}","tuple_list")),
    ?assertException(error,_,ag_metable_util:to_value("a","tuple")),
    ?assertException(error,_,ag_metable_util:to_value("a,b,c","tuple_list")),

    ?assertEqual("abc",ag_metable_util:to_value(<<"abc">>,<<"string">>)),
    ?assertEqual(["abc","ere"],ag_metable_util:to_value(<<"\"abc\",\"ere\"">>,<<"string_list">>)),
    ?assertException(error,_,ag_metable_util:to_value(abc,<<"string">>)),
    ?assertException(error,_,ag_metable_util:to_value("abc",<<"string_list">>)),

    ?assertEqual(abc,ag_metable_util:to_value(<<"abc">>,<<"atom">>)),
    ?assertEqual([abc,ere],ag_metable_util:to_value(<<"abc,ere">>,<<"atom_list">>)),
    ?assertException(error,_,ag_metable_util:to_value(<<"\"abc\",\"ere\"">>,<<"atom_list">>)),

    ?assertEqual(<<"abc">>,ag_metable_util:to_value(<<"abc">>,<<"binary">>)),
    ?assertEqual(<<"abc">>,ag_metable_util:to_value("abc",<<"binary">>)),
    ?assertEqual([<<"abc">>,<<"er">>],ag_metable_util:to_value("[<<\"abc\">>,<<\"er\">>]",<<"binary_list">>)),
    ?assertException(error,_,ag_metable_util:to_value("[\"abc\",\"er\"]",<<"binary_list">>)),

    ?assertEqual([a,b,c],ag_metable_util:to_value(<<"a,b,c">>,<<"list">>)),
    ?assertEqual([a,b,c],ag_metable_util:to_value("a,b,c",<<"list">>)),

    ?assertEqual(#{<<"a">>=>1},ag_metable_util:to_value(<<"{\"a\":1}">>,<<"json">>)),
    ?assertException(error,_,ag_metable_util:to_value(<<"\"a\":1}">>,<<"json">>)).