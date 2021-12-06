%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
%%%
-module(ag_bizlogger_write_process_SUITE).


-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [test_write_process_api, test_make_write_process_name].


init_per_suite(Config) ->
    application:start(ag_bizlogger),
    Config.

end_per_suite(_Config) ->

    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.


test_write_process_api(_) ->
    Args = #{biz=>op, index=>1, file_name_format => "out/YYYY-MM-DD/YY-DD-MM hh-mm-ss_~p.txt", split_file_rule => every_min, out_dir=>"./optest/"},
    {_, Pid} = gen_server:start(ag_bizlogger_write_process, [Args], []),
    ag_bizlogger_write_process:write(op_1, <<"test">>),
    ct:pal("ag_bizlogger_write_process pid:~p~n", [Pid]),
    Pid ! {checkpoint},
    timer:sleep(1000),
    gen_server:cast(Pid, {close}),
    timer:sleep(1000),
    ?assertEqual(false, is_process_alive(Pid)),
    ok.

test_make_write_process_name(_) ->
    ?assertEqual(
        ag_bizlogger_write_process_test_1,
        ag_bizlogger_write_process:make_process_name(<<"test">>, 1)
    ),
    ?assertEqual(
        ag_bizlogger_write_process_test_1,
        ag_bizlogger_write_process:make_process_name(test, 1)
    ),
    ?assertEqual(
        ag_bizlogger_write_process_test_1,
        ag_bizlogger_write_process:make_process_name("test", 1)
    ),
    ok.
