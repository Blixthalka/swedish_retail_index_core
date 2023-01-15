-module(date_util).


%%% External API
-export([get_datetime/0,
         today/0,
         tomorrow/0,
         yesterday/0,
         shift/3,
         year_start/1,
         halfyear_start/1,
         month_start/1,
         quarter_start/1,
         week_start/1,
         date_to_string/1,
         date_to_binary/1,
         binary_to_date/1,
         is_after/2,
         is_after_or_equal/2,
         is_before/2,
         is_before_or_equal/2,
         is_valid_date_binary/1,
         days_between/2,
         day_split/3,
         is_bank_holiday/1,
         shift_workdays/2,
         posix_microseconds_to_binary/1,
         posix_microseconds_to_binary_date/1,
         date_to_microseconds/1,
         now_in_seconds/0,
         now_in_milli_seconds/0,
         now_in_micro_seconds/0]).

-type milli_seconds() :: pos_integer().
-type micro_seconds() :: pos_integer().

-export_type([milli_seconds/0,
              micro_seconds/0]).

-spec get_datetime() -> binary().
get_datetime() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    Date = io_lib:format("~p-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                         [Year, Month, Day, Hour, Min, Sec]),
    list_to_binary(lists:flatten(Date)).

-spec today() -> binary().
today() ->
    date_to_binary(edate:today()).

-spec tomorrow() -> binary().
tomorrow() ->
    date_to_binary(edate:tomorrow()).

-spec yesterday() -> binary().
yesterday() ->
    date_to_binary(edate:yesterday()).

-spec shift(binary(), integer(), atom()) -> binary().
shift(Date, Num, Unit) ->
    date_util:date_to_binary(edate:shift(date_util:binary_to_date(Date), Num, Unit)).

-spec year_start(binary()) -> binary().
year_start(Date) ->
    {Year, _, _} = binary_to_date(Date),
    date_to_binary({Year, 1, 1}).

-spec month_start(binary()) -> binary().
month_start(Date) ->
    {Year, Month, _} = binary_to_date(Date),
    date_to_binary({Year, Month, 1}).

-spec quarter_start(binary()) -> binary().
quarter_start(Date) ->
    {Year, Month, _} = binary_to_date(Date),
    QuarterMonth = (((Month - 1) div 3) * 3) + 1,
    date_to_binary({Year, QuarterMonth, 1}).

-spec halfyear_start(binary()) -> binary().
halfyear_start(Date) ->
    {Year, Month, _} = binary_to_date(Date),
    HalfyearMonth = (((Month - 1) div 6) * 6) + 1,
    date_to_binary({Year, HalfyearMonth, 1}).


%%
%% We calulate that the week starts on a monday.
-spec week_start(binary()) -> binary().
week_start(Date) ->
    D = binary_to_date(Date),
    WeekDay = calendar:day_of_the_week(D),
    date_util:shift(Date, (WeekDay - 1) * -1, days).

-spec date_to_string(tuple()) -> string().
date_to_string(Date) ->
    edate:date_to_string(Date).

-spec date_to_binary(tuple()) -> binary().
date_to_binary(Date) ->
    list_to_binary(date_to_string(Date)).

-spec binary_to_date(binary()) -> tuple().
binary_to_date(Bin) ->
    edate:string_to_date(binary_to_list(Bin)).


-spec is_valid_date_binary(binary()) -> boolean().
is_valid_date_binary(Date) ->
    try
        binary_to_date(Date),
        true
    catch
        _:_ ->
            false
    end.

-spec is_after(binary(), binary()) -> boolean().
is_after(inception, _Second) ->
    false;
is_after(_First, inception) ->
    true;
is_after(First, Second) ->
    edate:is_after(edate:string_to_date(binary_to_list(First)),
                   edate:string_to_date(binary_to_list(Second))).

-spec is_after_or_equal(binary(), binary()) -> boolean().
is_after_or_equal(Equal, Equal) ->
    true;
is_after_or_equal(First, Second) ->
    is_after(First, Second).

-spec is_before(binary(), binary()) -> boolean().
is_before(inception, _Second) ->
    true;
is_before(_First, inception) ->
    false;
is_before(First, Second) ->
    edate:is_before(edate:string_to_date(binary_to_list(First)),
                    edate:string_to_date(binary_to_list(Second))).

-spec is_before_or_equal(binary(), binary()) -> boolean().
is_before_or_equal(Equal, Equal) ->
    true;
is_before_or_equal(First, Second) ->
    is_before(First, Second).

-spec days_between(tuple(), tuple()) -> integer().
days_between(First, Second) ->
    {Days, _} = calendar:time_difference({First, {0,0,0}}, {Second, {0,0,0}}),
    Days.

%% Shift the days X workdays forward. Does not work backwards. If zero today is returned if workday otherwise
%% The first day workday forward is used.
-spec shift_workdays(Date::binary(), Days::non_neg_integer()) -> binary().
shift_workdays(Date, 0) ->
    case is_bank_holiday(Date) of
        false ->
            Date;
        true ->
            shift_workdays(shift(Date, 1, days), 0)
    end;
shift_workdays(Date, Days) when Days > 0 ->
    case is_bank_holiday(Date) of
        false ->
            shift_workdays(shift(Date, 1, days), Days -1);
        true ->
            shift_workdays(shift(Date, 1, days), Days)
    end.




%% TODO: In the future add real bank holidays also and not just weekends
-spec is_bank_holiday(binary()) -> boolean().
is_bank_holiday(Date) ->
    lists:member(edate:day_of_week(binary_to_date(Date)), ["saturday", "sunday"]).

-spec day_split(binary(), binary(), integer()) -> [binary()].
day_split(StartDate0, EndDate0, Points0) ->
    StartDate1 = binary_to_date(StartDate0),
    EndDate1 = binary_to_date(EndDate0),
    DaysBetween = days_between(StartDate1, EndDate1),
    case DaysBetween < Points0 of
        true ->
            day_split_all(StartDate1, EndDate1, [StartDate0]);
        false ->
            Points1 = Points0 - 2,
            Average = DaysBetween / (Points0 - 1),
            day_split_average(Points1, StartDate1, EndDate0, Average, [EndDate1])
    end.

day_split_all(Date, Date, List) ->
    lists:reverse(List);
day_split_all(Date0, EndDate, List) ->
    Date1 = edate:shift(Date0, 1, days),
    day_split_all(Date1, EndDate, [date_to_binary(Date1)|List]).

day_split_average(0, StartDate, _EndDate, _Average, Dates0) ->
    Dates1 = [date_to_binary(X) || X <- Dates0],
    [date_to_binary(StartDate)|Dates1];
day_split_average(Points, StartDate, EndDate, Average, Dates0) ->
    NextDate = edate:shift(StartDate, round(Average * Points), days),
    day_split_average(Points - 1, StartDate, EndDate, Average, [NextDate|Dates0]).


%%
%% Current POSIX timestamp in seconds (UTC)
%%
-spec now_in_seconds() -> types:timestamp().
now_in_seconds() ->
    erlang:system_time(second).

%%
%% Current POSIX timestamp in milli seconds (UTC)
%%
-spec now_in_milli_seconds() -> types:timestamp().
now_in_milli_seconds() ->
    erlang:system_time(millisecond).

%%
%% Current POSIX timestamp in micro seconds (UTC)
%%
-spec now_in_micro_seconds() -> types:timestamp().
now_in_micro_seconds() ->
    erlang:system_time(microsecond).


%%
%% Convert POSIX timestamp in ms to binary on the format <<"YYYY-MM-DD hh:mm:ss">>.
%%
-spec posix_microseconds_to_binary(Timestamp::types:timestamp()) -> binary().
posix_microseconds_to_binary(Timestamp) when is_integer(Timestamp) ->
    M = 1000000,
    ErlangTimestamp = {Timestamp div M div M, Timestamp div M rem M, Timestamp rem M},
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_universal_time(ErlangTimestamp),
    Date = io_lib:format("~p-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                         [Year, Month, Day, Hour, Min, Sec]),
    list_to_binary(lists:flatten(Date)).

%%
%% Convert POSIX timestamp in ms to binary on the format <<"YYYY-MM-DD">>.
%%
-spec posix_microseconds_to_binary_date(Timestamp::types:timestamp()) -> binary().
posix_microseconds_to_binary_date(Timestamp) when is_integer(Timestamp) ->
    M = 1000000,
    ErlangTimestamp = {Timestamp div M div M, Timestamp div M rem M, Timestamp rem M},
    {Date, _Time} = calendar:now_to_universal_time(ErlangTimestamp),
    date_to_binary(Date).


%%
%% Convert binary date <<"YYYY-MM-DD">> to microseconds timestamp
%%
-spec date_to_microseconds(Date::binary()) -> types:timestamp().
date_to_microseconds(Date) ->
    TS = <<Date/binary, " 00:00:00.000Z">>,
    calendar:rfc3339_to_system_time(binary_to_list(TS), [{unit, microsecond}]).
