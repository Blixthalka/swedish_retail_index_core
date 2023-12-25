-module(graph).
-export([graph_ejson/4, filter_period_points/2]).

filter_period_points(_Period, []) ->
    [];
filter_period_points(Period, Points) ->
    PointsInRange = filter_points(Period, Points),
    FirstDate = first_date(PointsInRange),

    case find_initial_point(Points, FirstDate) of
        undefined ->
            PointsInRange;
        InitialPoint ->
            [InitialPoint|PointsInRange]
    end.

find_initial_point(Points, FirstDate) ->
    lists:foldl(fun(Point, CurrentPoint) ->
        Date = point:date(Point),
        case date_util:is_before(Date, FirstDate) of
            true ->
                case CurrentPoint =:= undefined orelse date_util:is_after(Date, point:date(CurrentPoint))  of
                    true ->
                        Point;
                    false ->
                        CurrentPoint
                end;
            false ->
                CurrentPoint
        end


    end, undefined, Points).

first_date(Points) ->
    lists:foldl(fun(Point, FirstDate) ->
        Date = point:date(Point),
        case FirstDate =:= undefined orelse date_util:is_before(Date, FirstDate) of
            true ->
                Date;
            false ->
                FirstDate
        end
    end, undefined, Points).

filter_points(start, Points) ->
    Today = date_util:today(),
    Start = <<"2023-01-01">>,
    filter_between(Start, Today, Points);
filter_points('1y', Points) ->
    Today = date_util:today(),
    Start = date_util:shift(Today, -1, years),
    filter_between(Start, Today, Points);
filter_points(ytd, Points) ->
    Today = date_util:today(),
    YearStart = date_util:year_start(Today),
    filter_between(YearStart, Today, Points);
filter_points('6m', Points) ->
    month(6, Points);
filter_points('3m', Points) ->
    month(3, Points);
filter_points('1m', Points) ->
    month(1, Points).

month(Months, Points) ->
    Today = date_util:today(),
    Start = date_util:shift(Today, 0 - Months, months),
    filter_between(Start, Today, Points).

filter_between(From, To, Points) ->
    lists:filter(fun(Point) ->
        Date = point:date(Point),
        date_util:is_after_or_equal(Date, From) andalso date_util:is_before_or_equal(Date, To)
    end, Points).


graph_ejson(MainPoints, ComparePoints, MainName, CompareName) ->

    Combined = helper:normalize_compare(MainPoints, ComparePoints),

    Graph = lists:map(fun({Date, Price, ComparePrice}) ->
        {[
            {date, Date},
            {value, calc:to_binary(Price, 2)},
            {compare, calc:to_binary(ComparePrice, 2)}
        ]}
    end, Combined),

    AllValues = lists:flatmap(fun({_, MainPrice, ComparePrice}) -> [MainPrice, ComparePrice] end, Combined),
    Min = calc:min(AllValues),
    Max = calc:max(AllValues),

    Reversed = lists:reverse(MainPoints),

    FirstPoint = hd(MainPoints),
    LastPoint = hd(Reversed),
    NextLastPoint = hd(tl(Reversed)),
    LastChange = calc:percent_change(point:price(NextLastPoint), point:price(LastPoint)),
    FromStartChange = calc:percent_change(point:price(FirstPoint), point:price(LastPoint)),

    {[
        {main_name, MainName},
        {compare_name, CompareName},
        {date, point:date(LastPoint)},
        {value, calc:to_binary(point:price(LastPoint), 2)},
        {min, calc:to_binary(Min, 2)},
        {max, calc:to_binary(Max, 2)},
        {last_change, calc:to_binary_percent(LastChange)},
        {from_start_change, calc:to_binary_percent(FromStartChange)},
        {series, Graph}
    ]}.