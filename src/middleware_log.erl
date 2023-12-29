-module(middleware_log).

-include_lib("kernel/include/logger.hrl").

-behaviour(cowboy_middleware).

%%% External API
-export([execute/2]).

%%%===================================================================
%%% External API
%%%===================================================================

%% Execute callback that handles option requests.

-spec execute(cowboy_req:req(), cowboy_middleware:env()) ->
                 {ok, cowboy_req:req(), cowboy_middleware:env()} | {stop, cowboy_req:req()}.
execute(Req, Env) ->
   io:format("GOT ~p ~p \n", [cowboy_req:method(Req), cowboy_req:path(Req)]),
   {ok, Req, Env}.

