-module(swedish_retail_index_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    db:start_mnesia(),
    start_cowboy(),

    Res = swedish_retail_index_sup:start_link(),
    io:format("Started Application\n"),
    Res.

start_cowboy() ->
    Dispatch = cowboy_router:compile([{'_', [
        {"/api/fx", fx_handler, []},
        {"/api/points", point_handler, []},
        {"/api/instruments", instrument_handler, []},
        {"/api/index", index_handler, []}
    ]}]),
    CowboyConfig = #{
        middlewares => [
            cowboy_router,
            cowboy_handler
        ],
        env => #{
            dispatch => Dispatch
        }
    },
    CowboyOptions = [
        {port, 8082}
    ],
    {ok, _} = cowboy:start_clear(http_api, CowboyOptions, CowboyConfig).

stop(_State) ->
    ok = cowboy:stop_listener(http_api),
    ok.

%% internal functions
