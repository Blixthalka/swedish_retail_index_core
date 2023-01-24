-module(instrument).


-export([
    key/1,
    key/2,
    name/1,
    external_id/1,
    currency/1,
    is_index/1,

    to_ejson/1,
    to_record/1,

    db_create_table/0,
    db_create/1,
    db_list/0,
    db_list_instruments/0,
    db_index/0,
    db_delete/1,

    update_db/0
]).

-record(instrument, {
    key :: binary() | undefined,
    name :: binary(),
    external_id :: binary(),
    currency :: binary(),
    is_index :: boolean()
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

is_index(#instrument{is_index = IsIndex}) ->
    IsIndex.

to_record(Map) ->
    #instrument{
        name = maps:get(<<"name">>, Map),
        external_id = maps:get(<<"external_id">>, Map),
        currency = maps:get(<<"currency">>, Map),
        is_index = parse:boolean(<<"is_index">>, Map)
    }.

to_ejson(#instrument{
    key = Key,
    name = Name,
    external_id = ExternalId,
    currency = Currency,
    is_index = IsIndex
}) ->
    {[
        {key, Key},
        {name, Name},
        {external_id, ExternalId},
        {currency, Currency},
        {is_index, IsIndex}
    ]}.

db_delete(Instrument) ->
    db:delete(?MODULE, Instrument).

db_list_instruments() ->
    lists:filter(fun(I) ->
        not is_index(I)
    end, db:list(?MODULE)).

db_index() ->
    V = lists:filter(fun(I) ->
        is_index(I)
    end, db:list(?MODULE)),
    hd(V).

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

update_db() ->
    Fun = fun(X) -> update_row(X) end,
    Attributes = record_info(fields, ?MODULE),
    {atomic, ok} = mnesia:transform_table(?MODULE, Fun, Attributes).

update_row({instrument, Key, Name, ExternalId, Currency}) ->
    #instrument{
        key = Key,
        name = Name,
        external_id = ExternalId,
        currency = Currency,
        is_index = false
    };
update_row(Instrument) ->
    Instrument.