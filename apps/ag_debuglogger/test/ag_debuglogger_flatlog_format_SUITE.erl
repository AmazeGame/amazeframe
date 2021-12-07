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
-module(ag_debuglogger_flatlog_format_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [to_string, format_log, map_depth, term_depth, unstructured].

term_depth() ->
    [{docs, "Once a term is too deep, it gets continued with `...'"}].
term_depth(_) ->
    ?assertEqual(
        "\"[\"01234567890123456789\",abc,[d,e|...]]\"",
        lists:flatten(ag_debuglogger_flatlog_format:to_string(
            ["01234567890123456789", abc, [d, e, [f, g, h]]]
            , #{term_depth => 6}
        ))
    ),
    ok.


to_string(_) ->
    ?assertEqual(
        "test",
        lists:flatten(ag_debuglogger_flatlog_format:to_string(test, #{term_depth => 6}))
    ),
    ?assertEqual(
        "12345",
        lists:flatten(ag_debuglogger_flatlog_format:to_string(12345, #{term_depth => 6}))
    ),
    ?assertEqual(
        "<0.239.0>",
        lists:flatten(ag_debuglogger_flatlog_format:to_string(list_to_pid("<0.239.0>"), #{term_depth => 6}))
    ),
%%    ?assertEqual(
%%        "#Ref<0.4192537678.4073193475.71181>",
%%        lists:flatten(ag_debuglogger_flatlog_format:to_string(erlang:list_to_ref("#Ref<0.4192537678.4073193475.71181>"), #{term_depth => 6}))
%%    ),
    ?assertEqual(
        "test",
        lists:flatten(ag_debuglogger_flatlog_format:to_string(<<"test">>, #{term_depth => 6}))
    ),
    ?assertEqual(
        "test",
        lists:flatten(ag_debuglogger_flatlog_format:to_string("test", #{term_depth => 6}))
    ),
    ?assertEqual(
        "[27979,35797]",
        lists:flatten(ag_debuglogger_flatlog_format:to_string("测试", #{term_depth => 6}))
    ),
    ?assertEqual(
        "{a,b,c}",
        lists:flatten(ag_debuglogger_flatlog_format:to_string({a, b, c}, #{term_depth => 6}))
    ),
    ?assertEqual(
        "{a,b,c}",
        lists:flatten(ag_debuglogger_flatlog_format:to_string({a, b, c}, #{term_depth => undefined}))
    ),
    ok.


format_log(_) ->
    Template = [time, level, {pid, [pid], ""}, mfa, line, msg],
    Config = #{term_depth => undefined, map_depth => -1, time_offset=>0, time_designator=>$T},
    Msg = #{log =>#{<<"name">>=><<"abcdefg">>}},
    Meta0 = #{time=> erlang:system_time(millisecond), mfa=>{?MODULE, ?FUNCTION_NAME, 1}, line=>76, pid=>list_to_pid("<0.239.0>")},
    FL0 = ag_debuglogger_flatlog_format:format_log(Template, Config, Msg, Meta0),
    ct:pal("format_log ~p~n", [FL0]),
    ?assertMatch(
        [_,
            ["<0.239.0>"],
            ["ag_debuglogger_flatlog_format_SUITE", 58, "format_log"],
            "76",
            [["log_abcdefg", 32]]],
        FL0),
    Meta1 = #{time=> erlang:system_time(millisecond), mfa=>{?MODULE, ?FUNCTION_NAME, [a, b, c]}, level=>"debug"},
    FL1 = ag_debuglogger_flatlog_format:format_log(Template, Config, Msg, Meta1),
    ct:pal("format_log ~p~n", [FL1]),
    ?assertMatch(
        [_, "debug", [],
            ["ag_debuglogger_flatlog_format_SUITE", 58, "format_log"],
            [["log_abcdefg", 32]]], FL1),
    Meta2 = #{time=> erlang:system_time(millisecond), mfa=>"test", level=>"debug"},
    FL3 = ag_debuglogger_flatlog_format:format_log(Template, Config, Msg, Meta2),
    ?assertMatch(
        [_, "debug", [], "test", [["log_abcdefg", 32]]],
        FL3),
    ok.

map_depth() ->
    [{docs, "A max number of nesting in maps can be provided"}].

map_depth(_) ->
    %% Use custom templates to drop metadata/templates
    Template = [msg],
    Map = #{a => #{b => #{c => #{d => x}},
        f => g},
        1 => #{2 => #{3 => x}}},
    ?assertEqual(
        [[["a_g", 32, ["a_b_c=... "]], [["1_2_x", 32]]]],
        ag_debuglogger_flatlog_format:format(#{level => info, msg => {report, Map}, meta => #{}},
            #{template => Template,
                map_depth => 3})

    ),
    ?assertEqual(
        [["a=... ", "1=... "]],
        ag_debuglogger_flatlog_format:format(#{level => info, msg => {report, Map}, meta => #{}},
            #{template => Template,
                map_depth => 1})

    ),
    ok.

unstructured() ->
    [{docs, "logs that aren't structured get passed through with a re-frame"}].
unstructured(_) ->
    ?assertEqual(
        [["abc", 32]],
        ag_debuglogger_flatlog_format:format(#{level => info, msg => {string, "abc"}, meta => #{}},
            #{template => [msg]})

    ),
    ?assertEqual(
        [["abc", 32]],
        ag_debuglogger_flatlog_format:format(#{level => info, msg => {string, [<<"abc">>]}, meta => #{}},
            #{template => [msg]})

    ),
    ?assertEqual(
        [[[34,"hello world",34],32]],
        ag_debuglogger_flatlog_format:format(#{level => info, msg => {"hello ~s", ["world"]}, meta => #{}},
            #{template => [msg]})

    ),
    ok.


