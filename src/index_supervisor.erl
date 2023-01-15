-module(index_supervisor).

-behaviour(supervisor).

-export([
    init/1,
    start_link/0
]).

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init(_) ->
    MaxRestart = 1000,
    MaxTime = 3600,

    {ok, {{one_for_one, MaxRestart, MaxTime}, [index_server()]}}.

index_server() ->
    {
        index_server,
        {index_server, start_link, []},
        permanent,
        10000,
        worker,
        [index_server]
    }.