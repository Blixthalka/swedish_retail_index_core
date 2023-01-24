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
    {LastDate, Value, FromStartChange, Sequence} = index_server:read(),

    AllValues = lists:map(fun({_, _, LocalValue}) -> LocalValue end, Sequence),
    Min = calc:min(AllValues),
    Max = calc:max(AllValues),

    {_, LastChange, _} = hd(lists:reverse(Sequence)),

    {CompareInstrument, ComparePriceMap} = compare(Sequence),

    Ejson = {[
        {date, LastDate},
        {compare_name, instrument:name(CompareInstrument)},
        {value, calc:to_binary(Value, 2)},
        {min, calc:to_binary(Min, 2)},
        {max, calc:to_binary(Max, 2)},
        {last_change, calc:to_binary_percent(LastChange)},
        {from_start_change, calc:to_binary_percent(FromStartChange)},
        {graph, lists:map(fun({Date, _, LocalValue}) ->
            {[
                {date, Date},
                {value, calc:to_binary(LocalValue, 2)},
                {compare, calc:to_binary(find_in_map(Date, ComparePriceMap), 2)}
            ]}
        end, Sequence)}
    ]},

    Json = jiffy:encode(Ejson),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req1, State}.


find_in_map(<<"2022-12-01">>, _) ->
    calc:zero();
find_in_map(Date, Map) ->
    case maps:get(Date, Map, undefined) of
        undefined ->
            find_in_map(date_util:shift(Date, -1, days), Map);
        V ->
            V
    end.


compare(Sequence) ->
    Instrument = instrument:db_index(),
    Points = point:db_find_all(instrument:key(Instrument)),

    {FirstDate, _, _} = hd(Sequence),
    FirstPrice = find_price(FirstDate, Points),

    PriceMap = lists:foldl(fun({Date, _, _}, Map) ->
        NonNormalizedPrice = find_price(Date, Points),
        Price = calc:multiply(calc:divide(NonNormalizedPrice, FirstPrice), calc:to_decimal(<<"100">>)),
        maps:put(Date, Price, Map)
    end, #{}, Sequence),
    {Instrument, PriceMap}.


find_price(<<"2021-01-01">>, _) ->
    calc:zero();
find_price(Date, Points) ->
    FoundPoints = lists:filter(fun(P) ->
        point:date(P) =:= Date
    end, Points),
    case FoundPoints of
        [] ->
            find_price(date_util:shift(Date, -1, days), Points);
        [Point] ->
            point:price(Point)
    end.

