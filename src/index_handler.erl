-module(index_handler).

-export([init/2]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            get(Req, State);
        _ ->
            Req1 = cowboy_req:reply(404, #{}, Req),
            {ok, Req1, State}
    end.

get(Req, State) ->
    Points = index_server:read(),

    AllValues = lists:map(fun(Point) -> point:price(Point) end, Points),
    Min = calc:min(AllValues),
    Max = calc:max(AllValues),

    Reversed = lists:reverse(Points),

    FirstPoint = hd(Points),
    LastPoint = hd(Reversed),
    NextLastPoint = hd(tl(Reversed)),
    LastChange = calc:percent_change(point:price(NextLastPoint), point:price(LastPoint)),
    FromStartChange = calc:percent_change(point:price(FirstPoint), point:price(LastPoint)),

    Instrument = instrument:db_index(),
    ComparePoints = point:db_find_all(instrument:key(Instrument)),

    Graph = helper:normalize_compare(Points, ComparePoints),

    Ejson = {[
        {date, point:date(LastPoint)},
        {compare_name, instrument:name(Instrument)},
        {value, calc:to_binary(point:price(LastPoint), 2)},
        {min, calc:to_binary(Min, 2)},
        {max, calc:to_binary(Max, 2)},
        {last_change, calc:to_binary_percent(LastChange)},
        {from_start_change, calc:to_binary_percent(FromStartChange)},
        {graph, helper:graph_ejson(Graph)}
    ]},

    Json = jiffy:encode(Ejson),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req1, State}.




