%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------

-module(agb_time).


%% API
-export([
    wday_to_str/1,
    month_to_str/1,
    str_to_month/1,
    str_to_day/1,
    str_to_min_sec/1
]).
-export([
    universal_seconds_to_local_time/1,
    universal_seconds_to_utc_time/1
]).
-export([
    utc_to_local/2,
    local_to_utc/2
]).
-export([
    format_datetime/2,
    get_checkpoint/1, get_checkpoint/2
]).
-export([
    get_diff_date/2,
    diff_date_days/2,
    diff_daystime_seconds/2
]).
-export([
    get_utc_time_seconds/0
]).
-export([
    same_date/2
]).

-type second_char() :: 48..57. %% 0~9
-type month() :: 1..12.
-type day() :: 1..31.
-type minute() :: 0..59.
-type second() :: 0..59.
-type daynum() :: 1..7.

-export_type([checkpoint_rule/0, second_char/0]).
-type checkpoint_rule() ::
weekly | {weekly, daynum()} | {weekly, daynum(), calendar:time()} |
monthly | {monthly, day()} | {monthly, day(), calendar:time()} |
daily | {daily, calendar:time()} |
hourly | {hourly, minute(), second()} | '2hourly' | '3hourly' | '4hourly' | '6hourly' | '8hourly' | '12hourly' |
every_min | every_2min | every_3min | every_4min | every_5min | every_6min | every_10min |
every_12min | every_15min | every_20min | every_30min.

-define(SECONDS_1970, 62167219200).

-spec universal_seconds_to_utc_time(Seconds :: non_neg_integer()) ->
    calendar:datetime().
universal_seconds_to_utc_time(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds + ?SECONDS_1970).

-spec universal_seconds_to_local_time(Seconds :: non_neg_integer()) ->
    calendar:datetime().
universal_seconds_to_local_time(Seconds) ->
    DT = calendar:gregorian_seconds_to_datetime(Seconds + ?SECONDS_1970),
    calendar:universal_time_to_local_time(DT).

-spec wday_to_str(daynum()) ->
    string().
wday_to_str(1) ->
    "Mon";
wday_to_str(2) ->
    "Tue";
wday_to_str(3) ->
    "Wed";
wday_to_str(4) ->
    "Thu";
wday_to_str(5) ->
    "Fri";
wday_to_str(6) ->
    "Sat";
wday_to_str(7) ->
    "Sun".
-spec month_to_str(month()) ->
    string().
month_to_str(1) ->
    "Jan";
month_to_str(2) ->
    "Feb";
month_to_str(3) ->
    "Mar";
month_to_str(4) ->
    "Apr";
month_to_str(5) ->
    "May";
month_to_str(6) ->
    "Jun";
month_to_str(7) ->
    "Jul";
month_to_str(8) ->
    "Aug";
month_to_str(9) ->
    "Sep";
month_to_str(10) ->
    "Oct";
month_to_str(11) ->
    "Nov";
month_to_str(12) ->
    "Dec".

-spec str_to_month([second_char()] |string()) ->
    month().
str_to_month("Jan") ->
    1;
str_to_month("Feb") ->
    2;
str_to_month("Mar") ->
    3;
str_to_month("Apr") ->
    4;
str_to_month("May") ->
    5;
str_to_month("Jun") ->
    6;
str_to_month("Jul") ->
    7;
str_to_month("Aug") ->
    8;
str_to_month("Sep") ->
    9;
str_to_month("Oct") ->
    10;
str_to_month("Nov") ->
    11;
str_to_month("Dec") ->
    12;
str_to_month("00") ->
    0;
str_to_month("01") ->
    1;
str_to_month("02") ->
    2;
str_to_month("03") ->
    3;
str_to_month("04") ->
    4;
str_to_month("05") ->
    5;
str_to_month("06") ->
    6;
str_to_month("07") ->
    7;
str_to_month("08") ->
    8;
str_to_month("09") ->
    9;
str_to_month("10") ->
    10;
str_to_month("11") ->
    11;
str_to_month("12") ->
    12.

-spec str_to_day([second_char()]) ->
    day().
str_to_day("00") ->
    0;
str_to_day("01") ->
    1;
str_to_day("02") ->
    2;
str_to_day("03") ->
    3;
str_to_day("04") ->
    4;
str_to_day("05") ->
    5;
str_to_day("06") ->
    6;
str_to_day("07") ->
    7;
str_to_day("08") ->
    8;
str_to_day("09") ->
    9;
str_to_day("10") ->
    10;
str_to_day("11") ->
    11;
str_to_day("12") ->
    12;
str_to_day("13") ->
    13;
str_to_day("14") ->
    14;
str_to_day("15") ->
    15;
str_to_day("16") ->
    16;
str_to_day("17") ->
    17;
str_to_day("18") ->
    18;
str_to_day("19") ->
    19;
str_to_day("20") ->
    20;
str_to_day("21") ->
    21;
str_to_day("22") ->
    22;
str_to_day("23") ->
    23;
str_to_day("24") ->
    24;
str_to_day("25") ->
    25;
str_to_day("26") ->
    26;
str_to_day("27") ->
    27;
str_to_day("28") ->
    28;
str_to_day("29") ->
    29;
str_to_day("30") ->
    30;
str_to_day("31") ->
    31.

-spec str_to_min_sec([second_char()]) ->
    second().
str_to_min_sec("00") ->
    0;
str_to_min_sec("01") ->
    1;
str_to_min_sec("02") ->
    2;
str_to_min_sec("03") ->
    3;
str_to_min_sec("04") ->
    4;
str_to_min_sec("05") ->
    5;
str_to_min_sec("06") ->
    6;
str_to_min_sec("07") ->
    7;
str_to_min_sec("08") ->
    8;
str_to_min_sec("09") ->
    9;
str_to_min_sec("10") ->
    10;
str_to_min_sec("11") ->
    11;
str_to_min_sec("12") ->
    12;
str_to_min_sec("13") ->
    13;
str_to_min_sec("14") ->
    14;
str_to_min_sec("15") ->
    15;
str_to_min_sec("16") ->
    16;
str_to_min_sec("17") ->
    17;
str_to_min_sec("18") ->
    18;
str_to_min_sec("19") ->
    19;
str_to_min_sec("20") ->
    20;
str_to_min_sec("21") ->
    21;
str_to_min_sec("22") ->
    22;
str_to_min_sec("23") ->
    23;
str_to_min_sec("24") ->
    24;
str_to_min_sec("25") ->
    25;
str_to_min_sec("26") ->
    26;
str_to_min_sec("27") ->
    27;
str_to_min_sec("28") ->
    28;
str_to_min_sec("29") ->
    29;
str_to_min_sec("30") ->
    30;
str_to_min_sec("31") ->
    31;
str_to_min_sec("32") ->
    32;
str_to_min_sec("33") ->
    33;
str_to_min_sec("34") ->
    34;
str_to_min_sec("35") ->
    35;
str_to_min_sec("36") ->
    36;
str_to_min_sec("37") ->
    37;
str_to_min_sec("38") ->
    38;
str_to_min_sec("39") ->
    39;
str_to_min_sec("40") ->
    40;
str_to_min_sec("41") ->
    41;
str_to_min_sec("42") ->
    42;
str_to_min_sec("43") ->
    43;
str_to_min_sec("44") ->
    44;
str_to_min_sec("45") ->
    45;
str_to_min_sec("46") ->
    46;
str_to_min_sec("47") ->
    47;
str_to_min_sec("48") ->
    48;
str_to_min_sec("49") ->
    49;
str_to_min_sec("50") ->
    50;
str_to_min_sec("51") ->
    51;
str_to_min_sec("52") ->
    52;
str_to_min_sec("53") ->
    53;
str_to_min_sec("54") ->
    54;
str_to_min_sec("55") ->
    55;
str_to_min_sec("56") ->
    56;
str_to_min_sec("57") ->
    57;
str_to_min_sec("58") ->
    58;
str_to_min_sec("59") ->
    59.

-spec format_datetime(Pattern :: string() | binary(), calendar:datetime()) ->
    string() | binary().
format_datetime(Pattern, DateTime) when is_list(Pattern) ->
    lists:flatten(do_format_datetime(Pattern, DateTime));
format_datetime(Pattern, DateTime) when is_binary(Pattern) ->
    iolist_to_binary(do_format_datetime(binary_to_list(Pattern), DateTime)).

do_format_datetime([], _) ->
    "";
do_format_datetime([$Y, $Y, $Y, $Y | FormatTail], {{Year, _Month, _Day}, _Time} = DateTime) ->
    [integer_to_list(Year), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$Y, $Y | FormatTail], {{Year, _Month, _Day}, _Time} = DateTime) ->
    [Y1, Y2 | _Y0] = lists:reverse(integer_to_list(Year)),
    [[Y2, Y1], do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$Y | FormatTail], DateTime) ->
    ["Y", do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$M, $M | FormatTail], {{_Year, Month, _Day}, _Time} = DateTime) when Month < 10 ->
    [$0, integer_to_list(Month), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$M, $M | FormatTail], {{_Year, Month, _Day}, _Time} = DateTime) ->
    [integer_to_list(Month), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$M, $o, $n | FormatTail], {{_Year, Month, _Day}, _Time} = DateTime) ->
    [month_to_str(Month), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$D, $D | FormatTail], {{_Year, _Month, Day}, _Time} = DateTime) when Day < 10 ->
    [$0, integer_to_list(Day), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$D, $D | FormatTail], {{_Year, _Month, Day}, _Time} = DateTime) ->
    [integer_to_list(Day), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$D | FormatTail], {{_Year, _Month, Day}, _Time} = DateTime) ->
    [integer_to_list(Day), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$h, $h | FormatTail], {_Date, {Hour, _Minutes, _Seconds}} = DateTime) when Hour < 10 ->
    [$0, integer_to_list(Hour), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$h, $h | FormatTail], {_Date, {Hour, _Minutes, _Seconds}} = DateTime) ->
    [integer_to_list(Hour), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$h | FormatTail], {_Date, {Hour, _Minutes, _Seconds}} = DateTime) ->
    [integer_to_list(Hour), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$m, $m | FormatTail], {_Date, {_Hour, Minutes, _Seconds}} = DateTime) when Minutes < 10 ->
    [$0, integer_to_list(Minutes), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$m, $m | FormatTail], {_Date, {_Hour, Minutes, _Seconds}} = DateTime) ->
    [integer_to_list(Minutes), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$m | FormatTail], {_Date, {_Hour, Minutes, _Seconds}} = DateTime) ->
    [integer_to_list(Minutes), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$s, $s | FormatTail], {_Date, {_Hour, _Minutes, Seconds}} = DateTime) when Seconds < 10 ->
    [$0, integer_to_list(Seconds), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$s, $s | FormatTail], {_Date, {_Hour, _Minutes, Seconds}} = DateTime) ->
    [integer_to_list(Seconds), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([$s | FormatTail], {_Date, {_Hour, _Minutes, Seconds}} = DateTime) ->
    [integer_to_list(Seconds), do_format_datetime(FormatTail, DateTime)];
do_format_datetime([C | FormatTail], {_Date, _Time} = DateTime) ->
    [C, do_format_datetime(FormatTail, DateTime)].

-spec get_checkpoint(Rule :: checkpoint_rule()) ->
    calendar:datetime().
get_checkpoint(Rule) ->
    DateTime = erlang:universaltime(),
    get_checkpoint(Rule, DateTime).

-spec get_checkpoint(Rule :: checkpoint_rule(), DateTime :: calendar:datetime()) ->
    calendar:datetime().
get_checkpoint(weekly, DateTime) ->
    get_checkpoint({weekly, 1, {0, 0, 0}}, DateTime);
get_checkpoint({weekly, WDay}, DateTime) ->
    get_checkpoint({weekly, WDay, {0, 0, 0}}, DateTime);
get_checkpoint({weekly, WDay, CheckTime}, {NowDate, NowTime}) ->
    WeekDay = calendar:day_of_the_week(NowDate),
    Days = calendar:date_to_gregorian_days(NowDate),
    CPDays0 = Days - WeekDay + WDay,
    CPDays =
        if
            WDay > WeekDay orelse (WDay =:= WeekDay andalso CheckTime > NowTime) ->
                CPDays0 - 7;
            true ->
                CPDays0

        end,
    {calendar:gregorian_days_to_date(CPDays), CheckTime};
get_checkpoint(monthly, DateTime) ->
    get_checkpoint({monthly, 1, {0, 0, 0}}, DateTime);
get_checkpoint({monthly, CheckDay}, DateTime) ->
    get_checkpoint({monthly, CheckDay, {0, 0, 0}}, DateTime);
get_checkpoint({monthly, CheckDay, CheckTime}, DateTime = {{Y, M, _D}, _NowTime}) ->
    {Y1, M1, D1} =
        if
            {{Y, M, CheckDay}, CheckTime} > DateTime ->
                if
                    M > 1 ->
                        {Y, M - 1, CheckDay}; %%大于1月份推迟一个月
                    true ->
                        {Y - 1, 12, CheckDay}  %%小于1月推到上一年12月
                end;
            true ->
                {Y, M, CheckDay}
        end,

    LastDay = calendar:last_day_of_the_month(Y1, M1),
    if
        D1 >= LastDay ->
            {{Y1, M1, LastDay}, CheckTime};
        true ->
            {{Y1, M1, D1}, CheckTime}
    end;
get_checkpoint(daily, DateTime) ->
    get_checkpoint({daily, {0, 0, 0}}, DateTime);
get_checkpoint({daily, CheckTime}, {NowDate, NowTime}) ->
    if
        CheckTime > NowTime -> %%还未到时间,前推一天
            Days = calendar:date_to_gregorian_days(NowDate),
            {calendar:gregorian_days_to_date(Days - 1), CheckTime};
        true ->
            {NowDate, CheckTime}
    end;
get_checkpoint(hourly, {{Y, M, D}, {Hour, _, _}}) ->
    {{Y, M, D}, {Hour, 0, 0}};
get_checkpoint({hourly, CheckMinute, CheckSecond}, {NowDate, {Hour, Minute, Second}}) ->
    if
        {CheckMinute, CheckSecond} > {Minute, Second} ->
            Seconds = calendar:datetime_to_gregorian_seconds({NowDate, {Hour, Minute, Second}}) - 3600,
            {LastDate, {LastHour, _, _}} = calendar:gregorian_seconds_to_datetime(Seconds),
            {LastDate, {LastHour, CheckMinute, CheckSecond}};
        true ->
            {NowDate, {Hour, CheckMinute, CheckSecond}}
    end;
get_checkpoint('2hourly', {{Y, M, D}, {Hour, _, _}}) ->
    SpHour = Hour div 2,
    {{Y, M, D}, {SpHour * 2, 0, 0}};
get_checkpoint('3hourly', {{Y, M, D}, {Hour, _, _}}) ->
    SpHour = Hour div 3,
    {{Y, M, D}, {SpHour * 3, 0, 0}};
get_checkpoint('4hourly', {{Y, M, D}, {Hour, _, _}}) ->
    SpHour = Hour div 4,
    {{Y, M, D}, {SpHour * 4, 0, 0}};
get_checkpoint('6hourly', {{Y, M, D}, {Hour, _, _}}) ->
    SpHour = Hour div 6,
    {{Y, M, D}, {SpHour * 6, 0, 0}};
get_checkpoint('8hourly', {{Y, M, D}, {Hour, _, _}}) ->
    SpHour = Hour div 8,
    {{Y, M, D}, {SpHour * 8, 0, 0}};
get_checkpoint('12hourly', {{Y, M, D}, {Hour, _, _}}) ->
    SpHour = Hour div 12,
    {{Y, M, D}, {SpHour * 12, 0, 0}};
get_checkpoint(every_min, {{Y, M, D}, {Hour, Minute, _}}) ->
    {{Y, M, D}, {Hour, Minute, 0}};
get_checkpoint(every_2min, {{Y, M, D}, {Hour, Minute, _}}) ->
    SpMin = Minute div 2,
    {{Y, M, D}, {Hour, SpMin * 2, 0}};
get_checkpoint(every_3min, {{Y, M, D}, {Hour, Minute, _}}) ->
    SpMin = Minute div 3,
    {{Y, M, D}, {Hour, SpMin * 3, 0}};
get_checkpoint(every_4min, {{Y, M, D}, {Hour, Minute, _}}) ->
    SpMin = Minute div 4,
    {{Y, M, D}, {Hour, SpMin * 4, 0}};
get_checkpoint(every_5min, {{Y, M, D}, {Hour, Minute, _}}) ->
    SpMin = Minute div 5,
    {{Y, M, D}, {Hour, SpMin * 5, 0}};
get_checkpoint(every_6min, {{Y, M, D}, {Hour, Minute, _}}) ->
    SpMin = Minute div 6,
    {{Y, M, D}, {Hour, SpMin * 6, 0}};
get_checkpoint(every_10min, {{Y, M, D}, {Hour, Minute, _}}) ->
    SpMin = Minute div 10,
    {{Y, M, D}, {Hour, SpMin * 10, 0}};
get_checkpoint(every_15min, {{Y, M, D}, {Hour, Minute, _}}) ->
    SpMin = Minute div 15,
    {{Y, M, D}, {Hour, SpMin * 15, 0}};
get_checkpoint(every_20min, {{Y, M, D}, {Hour, Minute, _}}) ->
    SpMin = Minute div 20,
    {{Y, M, D}, {Hour, SpMin * 20, 0}};
get_checkpoint(every_30min, {{Y, M, D}, {Hour, Minute, _}}) ->
    SpMin = Minute div 30,
    {{Y, M, D}, {Hour, SpMin * 30, 0}}.

-spec get_diff_date(Date :: calendar:date(), Days :: integer()) ->
    calendar:date().
get_diff_date(Date, Days) ->
    Days0 = calendar:date_to_gregorian_days(Date),
    calendar:gregorian_days_to_date(Days0 + Days).

-spec diff_date_days(Date1 :: calendar:date(), Date2 :: calendar:date()) ->
    integer().
diff_date_days(Date1, Date2) ->
    Days1 = calendar:date_to_gregorian_days(Date1),
    Days2 = calendar:date_to_gregorian_days(Date2),
    Days2 - Days1.

-spec same_date(UnixTime1 :: non_neg_integer(), UnixTime2 :: non_neg_integer()) ->
    boolean().
same_date(UnixTime1, UnixTime2) ->
    {Date2, _} = calendar:gregorian_seconds_to_datetime(UnixTime2),
    {Date1, _} = calendar:gregorian_seconds_to_datetime(UnixTime1),
    Date1 =:= Date2.

-spec diff_daystime_seconds(DateTime1 :: calendar:datetime(), DateTime2 :: calendar:datetime()) ->
    integer().
diff_daystime_seconds(DateTime1, DateTime2) ->
    {Days, {H, M, S}} = calendar:time_difference(DateTime1, DateTime2),
    86400 * Days + H * 3600 + M * 60 + S.

-spec utc_to_local(UtcDateTime :: calendar:datetime(), TimeZoneName :: string()) ->
    calendar:datetime().
utc_to_local(UtcDateTime, TZName) ->
    agb_localtime:utc_to_local(UtcDateTime, TZName).

-spec local_to_utc(LocalDateTime :: calendar:datetime(), TimeZoneName :: string()) ->
    calendar:datetime().
local_to_utc(LocalDateTime, TZName) ->
    agb_localtime:local_to_utc(LocalDateTime, TZName).

-spec get_utc_time_seconds() ->
    integer().
get_utc_time_seconds() ->
    {Mega, Sec, _Micro} = os:timestamp(),
    Mega * 1000000 + Sec.
