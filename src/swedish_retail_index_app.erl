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
        {"/api/instruments/", instrument_handler, []},
        {"/api/index", index_handler, []},
        {"/api/points/:key", point_handler, []}
    ]}]),
    ApiConfig = #{
        middlewares => [
            cowboy_router,
            cowboy_handler
        ],
        env => #{
            dispatch => ApiRoutes
        }
    },
    ApiOptions = [
        {port, 8082}
    ],
    {ok, _} = cowboy:start_clear(http_api, ApiOptions, ApiConfig),

    OpsRoutes = cowboy_router:compile([{'_', [
        {"/api/fx", fx_handler, []},
        {"/api/points", point_ops_handler, []},
        {"/api/instruments", instrument_ops_handler, []},
        {"/api/index", index_ops_handler, []}
    ]}]),
    OpsConfig = #{
        middlewares => [
            cowboy_router,
            cowboy_handler
        ],
        env => #{
            dispatch => OpsRoutes
        }
    },
    OpsOptions = [
        {port, 7082}
    ],
    {ok, _} = cowboy:start_clear(http_ops, OpsOptions, OpsConfig),
    ok.

stop(_State) ->
    ok = cowboy:stop_listener(http_api),
    ok.

%% internal functions
