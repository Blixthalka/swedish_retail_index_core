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

    Instrument = instrument:db_index(),
    ComparePoints = point:db_find_all(instrument:key(Instrument)),


    Ejson = {[
        {graph, graph:graph_ejson(Points, ComparePoints, <<"SRI">>, instrument:name(Instrument))}
    ]},

    Json = jiffy:encode(Ejson),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req1, State}.




