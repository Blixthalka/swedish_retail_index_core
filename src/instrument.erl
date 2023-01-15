-module(instrument).


-export([
    key/1,
    key/2,
    name/1,
    external_id/1,
    currency/1,

    to_ejson/1,
    to_record/1,

    db_create_table/0,
    db_create/1,
    db_list/0,
    db_delete/1
]).

-record(instrument, {
    key :: binary(),
    name :: binary(),
    external_id :: binary(),
    currency :: binary()
}).


key(#instrument{key = Key}) ->
    Key.

key(Key, Instrument) ->
    Instrument#instrument{key = Key}.

name(#instrument{name = Name}) ->
    Name.

external_id(#instrument{external_id = ExternalId}) ->
    ExternalId.

currency(#instrument{currency = Currency}) ->
    Currency.

to_record(Map) ->
    #instrument{
        name = maps:get(<<"name">>, Map),
        external_id = maps:get(<<"external_id">>, Map),
        currency = maps:get(<<"currency">>, Map)
    }.

to_ejson(#instrument{
    key = Key,
    name = Name,
    external_id = ExternalId,
    currency = Currency
}) ->
    {[
        {key, Key},
        {name, Name},
        {external_id, ExternalId},
        {currency, Currency}
    ]}.

db_delete(Instrument) ->
    db:delete(?MODULE, Instrument).

db_list() ->
    db:list(?MODULE).

db_create_table() ->
    db:create_table(?MODULE, record_info(fields, ?MODULE)).

db_create(Instrument) ->
    Res = lists:filter(fun(I) ->
        instrument:external_id(Instrument) =:= instrument:external_id(I)
    end, db_list()),
    case Res of
        [] ->
            db:create(?MODULE, Instrument);
        [Existing] ->
            NewInstrument = instrument:key(instrument:key(Existing), Instrument),
            db:update(?MODULE, NewInstrument);
        _ ->
            throw({error, too_many_with_same_external_id})
    end.
