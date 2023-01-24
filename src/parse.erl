-module(parse).


-export([
    boolean/2
]).

boolean(Field, Map) ->
    case maps:get(Field, Map, undefined) of
        true ->
            true;
        false ->
            false;
        <<"true">> ->
            true;
        <<"false">> ->
            false;
        _ ->
            throw({error, invalid_input})
    end.