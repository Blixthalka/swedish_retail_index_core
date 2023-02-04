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
    case instrument:db_read(Key) of
        {error, not_found} ->
            Req1 = cowboy_req:reply(404, Req);
        {ok, Instrument} ->
            Points = lists:filter(fun(P) ->
                date_util:is_after_or_equal(point:date(P), <<"2023-01-01">>)
            end, point:db_find_all(instrument:key(Instrument))),


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

            Owners1 = lists:sort(Owners0),

            Ejson = {[
                {name, instrument:name(Instrument)},
                {owners, lists:map(fun({Date, Owners}) ->
                    {[
                        {date, Date},
                        {owners, calc:to_binary(Owners)}
                    ]}
                end, Owners1)}
            ]},

            Json = jiffy:encode(Ejson),
            Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req)
    end,
    {ok, Req1, State}.

