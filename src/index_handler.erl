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

    Ejson = {[
        {date, LastDate},
        {value, calc:to_binary(Value, 2)},
        {min, calc:to_binary(Min, 2)},
        {max, calc:to_binary(Max, 2)},
        {last_change, calc:to_binary_percent(LastChange)},
        {from_start_change, calc:to_binary_percent(FromStartChange)},
        {graph, lists:map(fun({Date, _, LocalValue}) ->
            {[
                {date, Date},
                {value, calc:to_binary(LocalValue, 2)}
            ]}
        end, Sequence)}
    ]},

    Json = jiffy:encode(Ejson),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req1, State}.
