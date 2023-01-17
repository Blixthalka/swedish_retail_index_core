-module(instrument_ops_handler).

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
    Instrument0 = instrument:to_record(BodyMap),
    {ok, Instrument1} = instrument:db_create(Instrument0),
    Ejson = instrument:to_ejson(Instrument1),
    Json = jiffy:encode(Ejson),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req1),
    {ok, Req2, State}.