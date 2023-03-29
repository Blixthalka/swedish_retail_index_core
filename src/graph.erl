-module(graph).

-export([graph_ejson/4]).



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