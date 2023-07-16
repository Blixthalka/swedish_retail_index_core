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

            Owners = construct_owners(Points),

            ComparePoints = index_server:read(),

            Ejson = {[
                {name, instrument:name(Instrument)},
                {owners, lists:map(fun({Date, DateOwners}) ->
                    {[
                        {date, Date},
                        {owners, calc:to_binary(DateOwners, 0)}
                    ]}
                end, Owners)},
                {graph, graph:graph_ejson(Points, ComparePoints, instrument:name(Instrument), <<"SRI">>)}
            ]},

            Json = jiffy:encode(Ejson),
            Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req)
    end,
    {ok, Req1, State}.



construct_owners(Points) ->
    GroupedByMonth = lists:foldl(fun(P, Acc) ->
        PointKey = binary:part(point:date(P), 0, 7),
        maps:update_with(PointKey, fun(List) -> [P|List] end, [P], Acc)
    end, #{}, Points),
    Owners0 = lists:map(fun(ListOfPoints) ->
        Sorted = lists:sort(fun(A, B) ->
            point:date(A) =< point:date(B)
        end, ListOfPoints),
        FirstPoint = hd(Sorted),
        {point:date(FirstPoint), point:owners(FirstPoint)}
    end, maps:values(GroupedByMonth)),

    lists:sort(Owners0).

parameters(Req) ->
    P = cowboy_req:parse_qs(Req),
    maps:from_list(P).