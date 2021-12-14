%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------

-module(agb_localtime_dst).
-include("agb_timezone_db.hrl").
-include("agb_timezone_index.hrl").

-export([check/2]).
-comple([export_all]).

% check(DateTime, TimeZone) -> is_in_dst | is_not_in_dst | ambiguous_time | time_not_exists
%  DateTime = DateTime()
%  TimeZone = tuple()
check(DateTime, Timezone) when is_list(Timezone) ->
    case lists:keyfind(agb_localtime:get_timezone(Timezone), 1, ?AGB_TIMEZONE_DB) of
        false ->
            {error, unknown_tz};
        TZ ->
            check(DateTime, TZ)
    end;
check({Date = {Year, _, _}, Time}, {_, _, _, _Shift, DstShift, DstStartRule, DstStartTime, DstEndRule, DstEndTime}) ->
    DstStartDay = get_dst_day_of_year(DstStartRule, Year),
    DstEndDay = get_dst_day_of_year(DstEndRule, Year),
    CurrDay = get_day_of_year(Date),
    case is_dst_date(DstStartDay, DstEndDay, CurrDay) of
        equal_to_start ->
            is_dst_start_time(time_to_minutes(Time), time_to_minutes(DstStartTime), DstShift);
        equal_to_end ->
            is_dst_end_time(time_to_minutes(Time), time_to_minutes(DstEndTime), DstShift);
        Res ->
            Res
    end.

is_dst_start_time(CurrTime, DstStartTime, _DstShift) when CurrTime < DstStartTime ->
    is_not_in_dst;
is_dst_start_time(CurrTime, DstStartTime, DstShift) when CurrTime >= (DstStartTime + DstShift) ->
    is_in_dst;
is_dst_start_time(_CurrTime, _DstStartTime, _DstShift) ->
    time_not_exists.

is_dst_end_time(CurrTime, DstEndTime, DstShift) when CurrTime < (DstEndTime - DstShift) ->
    is_in_dst;
is_dst_end_time(CurrTime, DstEndTime, _DstShift) when CurrTime >= DstEndTime ->
    is_not_in_dst;
is_dst_end_time(_CurrTime, _DstStartTime, _DstShift) ->
    ambiguous_time.

is_dst_date(DstStartDay, _DstEndDay, CurrDay) when (CurrDay == DstStartDay) ->
    equal_to_start;
is_dst_date(_DstStartDay, DstEndDay, CurrDay) when (CurrDay == DstEndDay) ->
    equal_to_end;
is_dst_date(DstStartDay, DstEndDay, CurrDay)
    when (DstStartDay < DstEndDay) andalso ((CurrDay > DstStartDay) and (CurrDay < DstEndDay)) ->
    is_in_dst;
is_dst_date(DstStartDay, DstEndDay, CurrDay)
    when (DstStartDay < DstEndDay) andalso ((CurrDay < DstStartDay) or (CurrDay > DstEndDay)) ->
    is_not_in_dst;
is_dst_date(DstStartDay, DstEndDay, CurrDay)
    when (DstStartDay > DstEndDay) andalso ((CurrDay < DstStartDay) and (CurrDay > DstEndDay)) ->
    is_not_in_dst;
is_dst_date(DstStartDay, DstEndDay, CurrDay)
    when (DstStartDay > DstEndDay) andalso ((CurrDay > DstStartDay) or (CurrDay < DstEndDay)) ->
    is_in_dst.

get_dst_day_of_year({WeekDay, DayOfWeek, Month}, Year) when (WeekDay == last) or (WeekDay == 5) ->
    IntMonth = month_to_int(Month),
    IntDayOfWeek = day_to_int(DayOfWeek),
    get_last_dst(IntDayOfWeek, IntMonth, Year);
get_dst_day_of_year({WeekDay, DayOfWeek, Month}, Year) when (WeekDay > 0) and (WeekDay =< 4) ->
    IntMonth = month_to_int(Month),
    IntDayOfWeek = day_to_int(DayOfWeek),
    DstDays = get_day_of_year({Year, IntMonth, 1}),
    DstDayOfWeek = calendar:day_of_the_week({Year, IntMonth, 1}),
    case (DstDayOfWeek =:= IntDayOfWeek) and (WeekDay =:= 1) of
        true ->
            DstDays;
        false ->
            AdjustedDstDays =
                case IntDayOfWeek >= DstDayOfWeek of
                    true ->
                        DstDays + (IntDayOfWeek - DstDayOfWeek);
                    false ->
                        DstDays + (7 - DstDayOfWeek) + IntDayOfWeek
                end,
            AdjustedDstDays + (WeekDay - 1) * 7
    end;
get_dst_day_of_year(_, _) ->
    throw({error, wrong_week_day}).

get_last_dst(IntDayOfWeek, IntMonth, Year) ->
    MonthLastDays = calendar:date_to_gregorian_days(Year, IntMonth, 1) + calendar:last_day_of_the_month(Year, IntMonth),
    MonthLastDate = calendar:gregorian_days_to_date(MonthLastDays),
    MonthLastDayOfWeek = calendar:day_of_the_week(MonthLastDate),
    GregorianDays = calendar:date_to_gregorian_days({Year - 1, 12, 31}),
    case MonthLastDayOfWeek > IntDayOfWeek of
        true ->
            MonthLastDays - (MonthLastDayOfWeek - IntDayOfWeek) - GregorianDays;
        false ->

            MonthLastDays - MonthLastDayOfWeek - (7 - IntDayOfWeek) - GregorianDays
    end.

get_day_of_year(Date = {Year, _Month, _Day}) ->
    calendar:date_to_gregorian_days(Date) - calendar:date_to_gregorian_days({Year - 1, 12, 31}).

month_to_int(jan) ->
    1;
month_to_int(feb) ->
    2;
month_to_int(mar) ->
    3;
month_to_int(apr) ->
    4;
month_to_int(may) ->
    5;
month_to_int(jun) ->
    6;
month_to_int(jul) ->
    7;
month_to_int(aug) ->
    8;
month_to_int(sep) ->
    9;
month_to_int(oct) ->
    10;
month_to_int(nov) ->
    11;
month_to_int(dec) ->
    12.

day_to_int(mon) ->
    1;
day_to_int(tue) ->
    2;
day_to_int(wed) ->
    3;
day_to_int(thu) ->
    4;
day_to_int(fri) ->
    5;
day_to_int(sat) ->
    6;
day_to_int(sun) ->
    7.

time_to_minutes({Hours, Minutes}) ->
    Hours * 60 + Minutes;
time_to_minutes({Hours, Minutes, _Seconds}) ->
    Hours * 60 + Minutes.
