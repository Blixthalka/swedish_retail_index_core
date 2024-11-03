-module(swedish_retail_index_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    db:start_mnesia(),
    ok = start_cowboy(),

    Res = swedish_retail_index_sup:start_link(),
    io:format("Started Application ~p\n", [node()]),
    Res.

start_cowboy() ->
    ApiRoutes = cowboy_router:compile([{'_', [
        {"/api/instruments/[:key]", instrument_handler, []},
        {"/api/index", index_handler, []},
        {"/api/points/:key", point_handler, []},
        {"/api/change", change_handler, []},

        {"/api/ops/fx", fx_handler, []},
        {"/api/ops/points", point_ops_handler, []},
        {"/api/ops/instruments", instrument_ops_handler, []},
        {"/api/ops/index", index_ops_handler, []},
        {"/api/ops/split", split_ops_handler, []}
    ]}]),
    ApiConfig = #{
        middlewares => [
            middleware_log,
            cowboy_router,
            cowboy_handler
        ],
        env => #{
            dispatch => ApiRoutes
        }
    },
    ApiOptions = [
        {port, 8082},
        inet6
    ],
    {ok, _} = cowboy:start_clear(http_api, ApiOptions, ApiConfig),
    ok.

stop(_State) ->
    ok = cowboy:stop_listener(http_api),
    ok.

%% internal functions
