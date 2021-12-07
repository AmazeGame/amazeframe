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
-module(ag_debuglogger_json_format_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [to_string, format_log, unstructured].

term_depth() ->
    [{docs, "Once a term is too deep, it gets continued with `...'"}].
term_depth(_) ->
    ?assertEqual(
        "[\"01234567890123456789\",abc,[d,e|...]]",
        lists:flatten(ag_debuglogger_json_format:to_string(
            ["01234567890123456789", abc, [d, e, [f, g, h]]]
            , #{term_depth => 6}
        ))
    ),
    ok.


to_string(_) ->
    ?assertEqual(
        <<"test">>,
        ag_debuglogger_json_format:to_string(test, #{term_depth => 6})
    ),
    ?assertEqual(
        12345,
        ag_debuglogger_json_format:to_string(12345, #{term_depth => 6})
    ),
    ?assertEqual(
        "<0.239.0>",
        ag_debuglogger_json_format:to_string(list_to_pid("<0.239.0>"), #{term_depth => 6})
    ),
%%    ?assertEqual(
%%        "#Ref<0.4192537678.4073193475.71181>",
%%        lists:flatten(ag_debuglogger_json_format:to_string(list_to_ref("#Ref<0.4192537678.4073193475.71181>"), #{term_depth => 6}))
%%    ),
    ?assertEqual(
        <<"test">>,
        ag_debuglogger_json_format:to_string(<<"test">>, #{term_depth => 6})
    ),
    ?assertEqual(
        <<"test">>,
        ag_debuglogger_json_format:to_string("test", #{term_depth => 6})
    ),
    ?assertEqual(
        <<"[27979,35797]">>,
        ag_debuglogger_json_format:to_string("测试", #{term_depth => 6})
    ),
    ?assertEqual(
        <<"{a,b,c}">>,
        ag_debuglogger_json_format:to_string({a, b, c}, #{term_depth => 6})
    ),
    ?assertEqual(
        <<"{a,b,c}">>,
        ag_debuglogger_json_format:to_string({a, b, c}, #{term_depth => undefined})
    ),
    ok.


format_log(_) ->
    Template = [time, level, {pid, [pid], ""}, mfa, line, msg],
    Config = #{term_depth => undefined, map_depth => -1, time_offset=>0, time_designator=>$T},
    Msg = #{log =>#{<<"name">>=><<"abcdefg">>}},
    Meta0 = #{time=> erlang:system_time(millisecond), mfa=>{?MODULE, ?FUNCTION_NAME, 1}, line=>76, pid=>list_to_pid("<0.239.0>")},
    FL0 = ag_debuglogger_json_format:format_log(Template, Config, Msg, Meta0),
    ct:pal("format_log ~p~n", [FL0]),
%%    ?assertMatch(
%%        #{line := <<"76">>,
%%        log := #{<<"name">> := <<"abcdefg">>},
%%        mfa := <<"ag_debuglogger_json_format_SUITE:format_log">>,
%%        pid := <<"<0.239.0>">>,
%%        time := <<"1970-01-20T04:34:18.838+08:00">>}, FL0),
    Meta1 = #{time=> erlang:system_time(millisecond), mfa=>{?MODULE, ?FUNCTION_NAME, [a, b, c]}, level=>"debug"},
    FL1 = ag_debuglogger_json_format:format_log(Template, Config, Msg, Meta1),
    ct:pal("format_log ~p~n", [FL1]),
%%    ?assertMatch(#{level := <<"debug">>, log := <<"log_abcdefg ">>,
%%        mfa :=<<"ag_debuglogger_json_format_SUITE:format_log">>, pid := [],
%%        time := _}, FL1),
    Meta2 = #{time=> erlang:system_time(millisecond), mfa=>"test", level=>"debug"},
    FL3 = ag_debuglogger_json_format:format_log(Template, Config, Msg, Meta2),
    ct:pal("format_log ~p~n", [FL3]),
%%    ?assertMatch(#{level := <<"debug">>, log := <<"log_abcdefg ">>,
%%        mfa := <<"test">>, pid := [],
%%        time := _}, FL3),
    ok.

map_depth() ->
    [{docs, "A max number of nesting in maps can be provided"}].
map_depth(_) ->
    %% Use custom templates to drop metadata/templates
    Template = [msg],
    Map = #{a => #{b => #{c => #{d => x}},
        f => g},
        "1" => #{"2" => #{"3" => x}}},
    ?assertEqual(
        <<"{\"log\":\"a_g a_b_c=... 1_2_x \"}\n">>,
        ag_debuglogger_json_format:format(#{level => info, msg => {report, Map}, meta => #{}},
            #{template => Template,
                map_depth => 3})

    ),
    ?assertEqual(
        <<"{\"log\":\"a=... 1=... \"}\n">>,
        ag_debuglogger_json_format:format(#{level => info, msg => {report, Map}, meta => #{}},
            #{template => Template,
                map_depth => 1})

    ),
    ok.

unstructured() ->
    [{docs, "logs that aren't structured get passed through with a re-frame"}].
unstructured(_) ->
    ?assertEqual(
        <<"{\"log\":\"abc\"}\n">>,
        ag_debuglogger_json_format:format(#{level => info, msg => {string, "abc"}, meta => #{}},
            #{template => [msg]})

    ),
    ?assertEqual(
        <<"{\"log\":\"abc\"}\n">>,
        ag_debuglogger_json_format:format(#{level => info, msg => {string, [<<"abc">>]}, meta => #{}},
            #{template => [msg]})

    ),
    ?assertEqual(
        <<"{\"log\":{\"hello \":\"world\"}}\n">>,
        ag_debuglogger_json_format:format(#{level => info, msg => {"hello ~s", ["world"]}, meta => #{}},
            #{template => [msg]})

    ),
    ok.


