-module(point).

-export([
    create/4,
    key/1,
    key/2,
    owners/1,
    date/1,
    price/1,
    price/2,
    instrument_key/1,

    to_ejson/1,
    to_record/1,

    db_find_most_recent/1,
    db_find_all/1,
    db_create_table/0,
    db_create/1,
    db_list/0,
    db_delete/1,

    update_db/0
]).


-record(point, {
    key :: binary() | undefined,
    date :: binary(),
    instrument_key :: binary() | sri,
    price :: decimal:decimal(),
    owners :: decimal:decimal()
}).

-type point() :: #point{}.

-export_type([
    point/0
]).

create(Date, InstrumentKey, Price, Owners) ->
    #point{
        date = Date,
        instrument_key = InstrumentKey,
        price = Price,
        owners = Owners
    }.

date(#point{date = Date}) ->
    Date.

price(#point{price = Price}) ->
    Price.

price(Price, Point) ->
    Point#point{price = Price}.


instrument_key(#point{instrument_key = InstrumentKey}) ->
    InstrumentKey.

owners(#point{owners = Owners}) ->
    Owners.

to_ejson(#point{key = Key, date = Date, instrument_key = InstrumentKey, price = Price, owners = Owners}) ->
    {[
        {key, Key},
        {date, Date},
        {instrument_key, InstrumentKey},
        {price, calc:to_binary(Price)},
        {owners, calc:to_binary(Owners)}
    ]}.

to_record(Map) ->
    #point{
        date = date_util:parse_date(<<"date">>, Map),
        instrument_key = maps:get(<<"instrument_key">>, Map),
        price = calc:to_decimal(maps:get(<<"price">>, Map)),
        owners = calc:to_decimal(maps:get(<<"owners">>, Map))
    }.


key(Key, Point) ->
    Point#point{key = Key}.

key(#point{key = Key}) ->
    Key.


db_find_most_recent(InstrumentKey) ->
    RightInstruments = lists:filter(fun(#point{instrument_key = Key}) ->
        InstrumentKey =:= Key
    end, db_list()),
    [Recent | _] = lists:sort(fun(#point{date = A}, #point{date = B}) ->
        date_util:is_after(A, B)
    end, RightInstruments),
    Recent.

db_find_all(InstrumentKey) ->
    RightInstruments = lists:filter(fun(#point{instrument_key = Key}) ->
        InstrumentKey =:= Key
    end, db_list()),
    Points = lists:sort(fun(#point{date = A}, #point{date = B}) ->
        date_util:is_before(A, B)
    end, RightInstruments),
    Points.


db_delete(Point) ->
    db:delete(?MODULE, Point).

db_list() ->
    db:list(?MODULE).

db_create_table() ->
    db:create_table(?MODULE, record_info(fields, ?MODULE)).

db_create(Point) ->
    Res = lists:filter(fun(P) ->
        point:instrument_key(Point) =:= point:instrument_key(P) andalso
        point:date(Point) =:= point:date(P)
    end, db_list()),
    case Res of
        [] ->
            db:create(?MODULE, Point);
        [Existing] ->
            NewPoint = point:key(point:key(Existing), Point),
            db:update(?MODULE, NewPoint);
        _ ->
            throw({error, too_many_with_same_external_id})
    end.


update_db() ->
    Fun = fun(X) -> update_row(X) end,
    Attributes = record_info(fields, ?MODULE),
    {atomic, ok} = mnesia:transform_table(?MODULE, Fun, Attributes).


update_row(Instrument) ->
    Instrument.