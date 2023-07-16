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
    Parameters = parameters(Req),
    Period = parse:atom(<<"period">>, Parameters, ['1m', '3m', '6m', ytd], ytd),
    Points0 = index_server:read(),
    Points1 = graph:filter_period_points(Period, Points0),


    Instrument = instrument:db_index(),
    ComparePoints = point:db_find_all(instrument:key(Instrument)),


    Ejson = {[
        {graph, graph:graph_ejson(Points1, ComparePoints, <<"SRI">>, instrument:name(Instrument))}
    ]},

    Json = jiffy:encode(Ejson),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req1, State}.




parameters(Req) ->
    P = cowboy_req:parse_qs(Req),
    maps:from_list(P).