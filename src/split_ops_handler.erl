-module(split_ops_handler).


-export([init/2]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            post(Req, State);
        _ ->
            Req1 = cowboy_req:reply(404, #{}, Req),
            {ok, Req1, State}
    end.

post(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    BodyMap = jiffy:decode(Body, [return_maps]),

    InstrumentKey = maps:get(<<"instrument_key">>, BodyMap),
    ToDate = date_util:parse_date(<<"to_date">>, BodyMap),
    Divider = calc:to_decimal(maps:get(<<"divider">>, BodyMap)),

    Points0 = point:db_find_all(InstrumentKey),
    io:format("DATES0 ~p\n", [length(Points0)]),
    Points1 = lists:filter(fun(Point) ->
        point:date(Point) =< ToDate
    end, Points0),
    io:format("DATES1 ~p\n", [length(Points1)]),
    lists:foreach(fun(Point0) ->
        Price = calc:divide(point:price(Point0), Divider),
        Point1 = point:price(Price, Point0),
        point:db_create(Point1)
    end, Points1),

    cache_server:empty(),
    index_server:recalc(),
    Req1 = cowboy_req:reply(200, #{}, Req),
    {ok, Req1, State}.

