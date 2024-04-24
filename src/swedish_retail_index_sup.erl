-module(swedish_retail_index_sup).

-behaviour(supervisor).

-export([
    init/1,
    start_link/0
]).

start_link() ->
    io:format("Starting SRI supervisor\n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },

    {ok, {SupFlags, [index_supervisor(), cache_server()]}}.


cache_server() ->
    {
        cache_server,
        {cache_server, start_link, []},
        permanent,
        10000,
        worker,
        [cache_server]
    }.


index_supervisor() ->
    {
        index_supervisor,
        {index_supervisor, start_link, []},
        permanent,
        10000,
        supervisor,
        [index_supervisor]
    }.




%% internal functions
