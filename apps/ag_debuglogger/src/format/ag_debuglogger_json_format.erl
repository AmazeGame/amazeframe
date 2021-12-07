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
-module(ag_debuglogger_json_format).

%% API exports
-export([
    format/2,
    format_token/1,
    format_msg/2
]).

-ifdef(TEST).
-export([
    format_log/5,
    to_string/2
]).
-endif.

-type template() :: [metakey() | {metakey(), template(), template()} | string()].
-type metakey() :: atom() | [atom()].

%%====================================================================
%% API functions
%%====================================================================
-spec format(LogEvent, Config) -> unicode:chardata() when
    LogEvent :: logger:log_event(),
    Config :: logger:formatter_config().
format(#{level := Level, msg := {report, Msg}, meta := Meta}, UsrConfig) when is_map(Msg) ->
    Config = apply_defaults(UsrConfig),
    NewMeta = maps:put(level, Level, Meta),
    LogMap = format_log(maps:get(template, Config), Config, Msg, NewMeta),
    Log = agb_json:encode(#{<<"erl">> => LogMap}),
    <<Log/binary, <<"\n">>/binary>>;
format(Map = #{msg := {report, KeyVal}}, UsrConfig) when is_list(KeyVal) ->
    format(Map#{
        msg := {report, maps:from_list(KeyVal)}},
        UsrConfig
    );
format(Map = #{msg := {string, String}}, UsrConfig) ->
    format(Map#{
        msg := {report, #{log => unicode:characters_to_binary(String)}}},
        UsrConfig
    );
format(Map = #{msg := {Format, Terms}}, UsrConfig) ->
    RightWord = string:right(Format, 2),
    NewFormat =
        if
            RightWord == "~n" ->
                string:sub_string(Format, 1, string:len(Format) - 2);
            true ->
                Format
        end,

    FormatStrs = format_token(NewFormat),
    FormatBins = [list_to_binary(Str) || Str <- FormatStrs],
    LogList =
        case erlang:length(FormatBins) == erlang:length(Terms) of
            true ->
                lists:zip(FormatBins, Terms);
            _ ->
                lists:zip(FormatBins, Terms ++ [<<"end">>])
        end,
    LogMap = maps:from_list(LogList),
    format(Map#{
        msg := {report, #{log => LogMap}}},
        UsrConfig
    ).

%%====================================================================
%% Internal functions
%%====================================================================
apply_defaults(Map) ->
    maps:merge(
        #{term_depth => undefined,
            map_depth => -1,
            time_offset => 0,
            time_designator => $T,
            template => [time, level, {pid, [pid], ""}, mfa, line, msg]
        },
        Map
    ).

-spec format_log(template(), Config, Msg, Meta) -> map() when
    Config :: logger:formatter_config(),
    Msg :: Data,
    Meta :: Data,
    Data :: #{string() | binary() | atom() => term()}.
format_log(Tpl, Config, Msg, Meta) ->
    format_log(Tpl, Config, Msg, Meta, #{}).

-spec format_log(template(), Config, Msg, Meta, Acc) -> map() when
    Config :: logger:formatter_config(),
    Msg :: Data,
    Meta :: Data,
    Acc :: maps:map(),
    Data :: #{string() | binary() | atom() => term()}.
format_log([], _Config, _Msg, _Meta, Acc) ->
    Acc;
format_log([msg | Rest], Config, Msg, Meta, Acc) ->
    %[format_msg(Msg, Config) | Acc]
    format_log(Rest, Config, Msg, Meta, maps:merge(format_msg(Msg, Config), Acc));
format_log([Key | Rest], Config, Msg, Meta, Acc) when is_atom(Key) orelse is_atom(hd(Key)) -> % from OTP
    case maps:find(Key, Meta) of
        error ->
            format_log(Rest, Config, Msg, Meta, Acc);
        {ok, Val} ->
            format_log(Rest, Config, Msg, Meta,
                maps:merge(format_val(Key, Val, Config), Acc))
    end;
format_log([{Key, IfExists, Else} | Rest], Config, Msg, Meta, Acc) ->
    case maps:find(Key, Meta) of
        error ->
            format_log(Rest, Config, Msg, Meta,
                maps:merge(#{Key => Else}, Acc));
        {ok, Val} ->
            format_log(Rest, Config, Msg, Meta,
                maps:merge(format_log(IfExists, Config, Msg, #{Key => Val}, #{}), Acc))
    end.

-spec format_msg(Data, Config) -> Msg when
    Data :: maps:map(),
    Config :: maps:map(),
    Msg :: maps:map() | string().
format_msg(Data, Config) ->
    format_msg("", Data, Config).


format_msg(Parents, Data, Config) when is_map(Data) ->
    maps:fold(
        fun
            (report, V, Acc) when is_map(V) ->
                Acc#{report => format_msg(Parents, V, Config)};
            (report, V, Acc) when is_list(V) ->
                Acc#{report => format_msg(Parents, maps:from_list(V), Config)};
            (format, V, Acc) ->
                Acc#{log => agb_convertor:to_binary(lists:flatten(io_lib:format(V, maps:get(args, Data))))};
            (args, _V, Acc) ->
                Acc;
            (K, V, Acc) when is_map(V) ->
                Acc#{K => format_msg(Parents, V, Config)};
            (K, V, Acc) when is_list(V) ->
                case io_lib:printable_list(V) of
                    true ->
                        Acc#{K => to_string(V, Config)};
                    _ ->
                        Acc#{K => format_msg(Parents, V, Config)}
                end;
            (K, V, Acc) ->
                Acc#{K => to_string(V, Config)}
        end,
        #{},
        Data
    );
format_msg(Parents, Data, Config) when is_list(Data) ->
    lists:map(
        fun
            (V) when is_map(V) ->
                format_msg(Parents, V, Config);
            (V) when is_list(V) ->
                format_msg(Parents, V, Config);
            (V) ->
                to_string(V, Config)
        end,
        Data
    ).

format_val(time, Time, Config) ->
    #{time => agb_convertor:to_binary(format_time(Time, Config))};
format_val(mfa, MFA, Config) ->
    #{mfa => agb_convertor:to_binary(escape(format_mfa(MFA, Config)))};
format_val(Key, Val, Config) ->
    #{Key => agb_convertor:to_binary(to_string(Val, Config))}.

format_time(N, #{time_offset := _O, time_designator := _D}) when is_integer(N) ->
    calendar:system_time_to_rfc3339(N div 1000, [{unit, millisecond}]).

format_mfa({M, F, A}, _) when is_atom(M), is_atom(F), is_integer(A) ->
    [atom_to_list(M), $:, atom_to_list(F)];
format_mfa({M, F, A}, Config) when is_atom(M), is_atom(F), is_list(A) ->
    %% arguments are passed as a literal list ({mod, fun, [a, b, c]})
    format_mfa({M, F, length(A)}, Config);
format_mfa(MFAStr, Config) -> % passing in a pre-formatted string value
    to_string(MFAStr, Config).

-spec to_string(Data, Config) -> String when
    Data :: term(),
    Config :: maps:map(),
    String :: string().
to_string(X, _) when is_atom(X) ->
    escape(atom_to_list(X));
to_string(X, _) when is_integer(X) ->
    X;
to_string(X, _) when is_pid(X) ->
    pid_to_list(X);
to_string(X, _) when is_reference(X) ->
    ref_to_list(X);
to_string(X, _C) when is_binary(X) ->
    X;
to_string(X, C) when is_list(X) ->
    case io_lib:printable_list(X) of
        true ->
            escape(X);
        _ ->
            escape(format_str(C, X))
    end;
to_string(X, C) ->
    escape(format_str(C, X)).

format_str(#{term_depth := undefined}, T) ->
    lists:flatten(io_lib:format("~0tp", [T]));
format_str(#{term_depth := D}, T) ->
    lists:flatten(io_lib:format("~0tP", [T, D])).

escape(Str) ->
    agb_convertor:to_binary(Str).

format_token(FormatStr) ->
    format_token(FormatStr, [], []).
format_token([], [], KeysAcc) ->
    lists:reverse(KeysAcc);
format_token([], KeyAcc, KeysAcc) ->
    lists:reverse([lists:reverse(KeyAcc) | KeysAcc]);
format_token([$~ | T], [], KeysAcc) ->
    case format_control_end(T) of
        {no_split, T1} ->
            format_token(T1, [], KeysAcc);
        {split, T1} ->
            format_token(T1, [], ["begin" | KeysAcc])
    end;
format_token([$~ | T], KeyAcc, KeysAcc) ->
    case format_control_end(T) of
        {no_split, T1} ->
            format_token(T1, [], KeysAcc);
        {split, T1} ->
            format_token(T1, [], [lists:reverse(KeyAcc) | KeysAcc])
    end;
format_token([H | T], KeyAcc, KeysAcc) ->
    format_token(T, [H | KeyAcc], KeysAcc).

format_control_end([]) ->
    {no_split, []};
format_control_end([H | T]) when
    H == $c orelse H == $f orelse H == $e orelse H == $g orelse H == $s orelse H == $w orelse
        H == $p orelse H == $W orelse H == $P orelse H == $B orelse H == $X orelse H == $# orelse
        H == $b orelse H == $x orelse H == $+ ->
    {split, T};
format_control_end([$n | T]) ->
    {no_split, T};
format_control_end([_ | T]) ->
    format_control_end(T).
