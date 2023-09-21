-module(instrument_handler).

-export([init/2]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            get(Req, State, cowboy_req:binding(key, Req));
        _ ->
            Req1 = cowboy_req:reply(404, #{}, Req),
            {ok, Req1, State}
    end.

get(Req, State, undefined) ->
    Members = helper:construct_members_most_recent(),

    Ejson = lists:map(fun({Instrument, Point, Weight}) ->
        {[
            {key, instrument:key(Instrument)},
            {name, instrument:name(Instrument)},
            {is_index, instrument:is_index(Instrument)},
            {external_id, instrument:external_id(Instrument)},
            {owners, calc:to_binary(point:owners(Point), 0)},
            {weight, calc:to_binary_percent(Weight)}
        ]}
    end, Members),

    Json = jiffy:encode(Ejson),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req1, State};
get(Req, State, Key) ->
    Parameters = parameters(Req),
    Period = parse:atom(<<"period">>, Parameters, ['1m', '3m', '6m', ytd], ytd),

    case instrument:db_read(Key) of
        {error, not_found} ->
            Req1 = cowboy_req:reply(404, Req);
        {ok, Instrument} ->
            Points0 = point:db_find_all(instrument:key(Instrument)),
            Points1 = graph:filter_period_points(Period, Points0),

            Points = lists:map(fun(Point) ->
                Date = point:date(Point),
                Price = point:price(Point),
                Fx = fx:db_closest_rate(Date, instrument:currency(Instrument), <<"SEK">>),
                FxPrice = calc:multiply(Price, Fx),
                point:price(FxPrice, Point)
            end, Points1),

            OwnersSeries = lists:map(fun(Point) ->
                {[
                    {date, point:date(Point)},
                    {value, calc:to_binary(point:owners(Point))}
                ]}
            end, Points),

            AllValues = lists:map(fun(Point) -> point:owners(Point) end, Points),
            Min = calc:min(AllValues),
            Max = calc:max(AllValues),

            ComparePoints = index_server:read(),

            Ejson = {[
                {name, instrument:name(Instrument)},
                {owners, {[
                    {series, OwnersSeries},
                    {main_name, <<"Owners">>},
                    {min, calc:to_binary(Min, 2)},
                    {max, calc:to_binary(Max, 2)}
                ]}},
                {graph, graph:graph_ejson(Points, ComparePoints, instrument:name(Instrument), <<"SRI">>)}
            ]},



            Json = jiffy:encode(Ejson),
            Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req)
    end,
    {ok, Req1, State}.



parameters(Req) ->
    P = cowboy_req:parse_qs(Req),
    maps:from_list(P).