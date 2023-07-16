-module(parse).


-export([
    boolean/2,
    atom/4
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

    atom(Field, Map, Valid, Default) ->
        try
            case maps:get(Field, Map, undefined) of
                Value when is_binary(Value) ->
                    Atom = binary_to_existing_atom(Value, utf8);
                undefined ->
                    Atom = Default;
                false ->
                    Atom = Default,
                    throw({error, invalid_input})
            end,
            case Valid == all orelse lists:member(Atom, Valid) of
                true ->
                    Atom;
                false ->
                    throw({error, invalid_input})
            end
        catch
            _:_ ->
                throw({error, invalid_input})
        end.