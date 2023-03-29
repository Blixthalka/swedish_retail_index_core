-module(helper).

-define(INDEX_SIZE, 50).

-export([
    construct_members_most_recent/0,
    construct_members/1,
    member_diff/2,
    only_member_of_first/2,
    normalize_compare/2
]).


all() ->
    lists:foldl(fun(Instrument, InstrumentMap) ->
        Points = point:db_find_all(instrument:key(Instrument)),
        PointMap = lists:foldl(fun(Point, Map) ->
            maps:put(point:date(Point), Point, Map)
        end, #{}, Points),
        maps:put(instrument:key(Instrument), {Instrument, PointMap}, InstrumentMap)
    end, #{}, instrument:db_list_instruments()).

member_diff(DateFrom, DateTo) ->
    All = all(),
    {FromKeys, FromMap} = sort(construct_members_for_date(All, DateFrom)),
    {ToKeys, ToMap} = sort(construct_members_for_date(All, DateTo)),

    Old = to_name(only_member_of_first(FromKeys, ToKeys), FromMap),
    New = to_name(only_member_of_first(ToKeys, FromKeys), ToMap),
    {Old, New}.

to_name(List, Map) ->
    lists:map(fun(I) -> maps:get(I, Map) end, List).

only_member_of_first(A, B) ->
    lists:filter(fun(A2) -> not lists:member(A2, B) end, A).

sort(Day) ->
    lists:foldl(fun({Instrument, _, _}, {Keys, Map}) ->
        {[instrument:key(Instrument)|Keys], maps:put(instrument:key(Instrument), instrument:name(Instrument), Map)}
    end, {[], #{}}, Day).


construct_members_for_date(All, Date) ->
    Day = find_day_closest_to_date(All, Date),
    construct_members(Day).

construct_members_most_recent() ->
    construct_members_for_date(all(), date_util:today()).

find_day_closest_to_date(_, <<"2022-12-30">>) ->
    throw(not_found);
find_day_closest_to_date(All, Date) ->
    Res = lists:filtermap(fun({Instrument, Points}) ->
        case maps:get(Date, Points, undefined) of
            undefined ->
                false;
            Point ->
                {true, {Instrument, Point}}
        end
    end, maps:values(All)),
    case length(Res) >= 50 of
        true ->
            Res;
        false ->
            find_day_closest_to_date(All, date_util:shift(Date, -1, days))
    end.

construct_members(Day) ->
    Sorted = lists:sort(fun({_, A}, {_, B}) ->
       calc:compare(point:owners(A), point:owners(B)) > 0
    end, Day),
    Members = lists:sublist(Sorted, ?INDEX_SIZE),
    TotalSize = lists:foldl(fun({_, Point}, Acc) ->
        calc:add(Acc, point:owners(Point))
    end, calc:zero(), Members),
    lists:map(fun({Instrument, Point}) ->
        {Instrument, Point, calc:divide(point:owners(Point), TotalSize)}
    end, Members).


normalize_compare(MainPoints, ComparePoints) ->
    FirstDate = point:date(hd(MainPoints)),
    FirstMainPrice = point:price(hd(MainPoints)),
    FirstComparePrice = find_price(FirstDate, ComparePoints),

    lists:map(fun(Point) ->
        Date = point:date(Point),
        NonNormalizedPrice = find_price(Date, ComparePoints),
        Price = calc:multiply(calc:divide(NonNormalizedPrice, FirstComparePrice), FirstMainPrice),
        {Date, point:price(Point), Price}
    end, MainPoints).


find_price(<<"2021-01-01">>, _) ->
    calc:zero();
find_price(Date, Points) ->
    FoundPoints = lists:filter(fun(P) ->
        point:date(P) =:= Date
    end, Points),
    case FoundPoints of
        [] ->
            find_price(date_util:shift(Date, -1, days), Points);
        [Point] ->
            point:price(Point)
    end.

