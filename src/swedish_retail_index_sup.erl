-module(swedish_retail_index_sup).

-behaviour(supervisor).

-export([
    init/1,
    start_link/0,
    add_enduser_supervisor/1
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

    {ok, {SupFlags, [index_supervisor()]}}.


add_enduser_supervisor(_) ->
    ok.


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
