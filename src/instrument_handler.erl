-module(instrument_handler).

-export([init/2]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            get(Req, State);
        <<"POST">> ->
            post(Req, State);
        _ ->
            Req1 = cowboy_req:reply(404, #{}, Req),
            {ok, Req1, State}
    end.

get(Req, State) ->

    Members = helper:construct_members_most_recent(),

    Ejson = lists:map(fun({Instrument, Point, Weight}) ->
        {[
            {name, instrument:name(Instrument)},
            {external_id, instrument:external_id(Instrument)},
            {owners, calc:to_binary(calc:round(point:owners(Point), 0))},
            {weight, calc:to_binary(calc:round(calc:multiply(Weight, calc:to_decimal(<<"100">>)), 2))}
        ]}
    end, Members),

    Json = jiffy:encode(Ejson),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req1, State}.

post(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    BodyMap = jiffy:decode(Body, [return_maps]),
    Instrument0 = instrument:to_record(BodyMap),
    {ok, Instrument1} = instrument:db_create(Instrument0),
    Ejson = instrument:to_ejson(Instrument1),
    Json = jiffy:encode(Ejson),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req1),
    {ok, Req2, State}.