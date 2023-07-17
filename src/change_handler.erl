-module(change_handler).

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
    Today = date_util:today(),
    From = date_util:shift(Today, -6, months),
    {Old, New} = helper:member_diff(From, Today),

    Ejson = {[
        {old, Old},
        {new, New}
    ]},

    Json = jiffy:encode(Ejson),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req1, State}.


