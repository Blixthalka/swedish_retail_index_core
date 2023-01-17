-module(index_ops_handler).

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
    index_server:recalc(),
    Req1 = cowboy_req:reply(200, #{}, Req),
    {ok, Req1, State}.

