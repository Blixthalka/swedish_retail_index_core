-module(helper).

-define(INDEX_SIZE, 50).

-export([
    construct_members_most_recent/0,
    construct_members/1
]).


construct_members_most_recent() ->
    Day = lists:map(fun(Instrument) ->
        Point = point:db_find_most_recent(instrument:key(Instrument)),
        {Instrument, Point}
    end, instrument:db_list()),
    construct_members(Day).


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

