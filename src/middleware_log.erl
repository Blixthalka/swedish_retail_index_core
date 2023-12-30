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

   case binary:match(cowboy_req:path(Req), <<"/ops/">>) of
      nomatch ->
         {ok, Req, Env};
      _ ->
         Headers = cowboy_req:headers(Req),
         case maps:get(<<"secret">>, Headers, undefined) of
            undefined ->
               {stop, cowboy_req:reply(403, #{}, Req)};
            UserSecret0 ->
               UserSecret1 = binary_to_list(UserSecret0),
               CorrectSecret = os:getenv("SECRET"),
               case CorrectSecret =:= UserSecret1 of
                  true ->
                     {ok, Req, Env};
                  false ->
                     {stop, cowboy_req:reply(403, #{}, Req)}
               end
         end
   end.



