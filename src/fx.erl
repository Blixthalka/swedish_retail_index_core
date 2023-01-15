-module(fx).


-export([
    key/1,
    key/2,
    rate/1,
    date/1,
    from/1,
    to/1,

    equal/2,

    to_ejson/1,
    to_record/1,

    db_closest/3,
    db_closest_rate/3,
    db_create_table/0,
    db_create/1,
    db_list/2,
    db_list/0,
    db_delete/1
]).

-record(fx, {
    key :: binary(),
    date :: binary(),
    from :: binary(),
    to :: binary(),
    rate :: decimal:decimal()
}).

key(#fx{key = Key}) ->
    Key.

key(Key, Fx) ->
    Fx#fx{key = Key}.

rate(#fx{rate = Rate}) ->
    Rate.

date(#fx{date = Date}) ->
    Date.

from(#fx{from = From}) ->
    From.

to(#fx{to = To}) ->
    To.

to_record(Map) ->
    #fx{
        date = maps:get(<<"date">>, Map),
        from = maps:get(<<"from">>, Map),
        to = maps:get(<<"to">>, Map),
        rate = calc:to_decimal(maps:get(<<"rate">>, Map))
    }.

to_ejson(#fx{
    key = Key,
    date = Date,
    from = From,
    to = To,
    rate = Rate
}) ->
    {[
        {key, Key},
        {date, Date},
        {from, From},
        {to, To},
        {rate, calc:to_binary(Rate)}
    ]}.

equal(A, B) ->
    fx:from(A) =:= fx:from(B) andalso
    fx:to(A) =:= fx:to(B) andalso
    fx:date(A) =:= fx:date(B).

db_delete(Fx) ->
    db:delete(?MODULE, Fx).

db_list() ->
    db:list(?MODULE).

db_create_table() ->
    db:create_table(?MODULE, record_info(fields, ?MODULE)).

db_closest(Date, Currency, Currency) ->
    #fx{date = Date, from = Currency, to = Currency, rate = calc:to_decimal(<<"1">>)};
db_closest(Date, From, To) ->
    Filtered = lists:filter(fun(Fx) ->
        From =:= fx:from(Fx) andalso
        To =:= fx:to(Fx) andalso
        date_util:is_before_or_equal(fx:date(Fx), Date)
    end, db_list()),
    Sorted = lists:sort(fun(A, B) ->
        date_util:is_after(date(A), date(B))
    end, Filtered),
    case Sorted of
        [] ->
            #fx{date = Date, from = From, to = To, rate = calc:to_decimal(<<"1">>)};
        _ ->
            hd(Sorted)
    end.

db_closest_rate(Date, From, To) ->
    Fx = db_closest(Date, From, To),
    fx:rate(Fx).

db_list(From, To) ->
    Filtered = lists:filter(fun(Fx) ->
        From =:= fx:from(Fx) andalso
        To =:= fx:to(Fx)
    end, db_list()),
    lists:sort(fun(A, B) ->
        date_util:is_after(date(A), date(B))
    end, Filtered).

db_create(Fx) ->
    Res = lists:filter(fun(I) ->
        equal(Fx, I)
    end, db_list()),
    case Res of
        [] ->
            db:create(?MODULE, Fx);
        [Existing] ->
            NewFx = fx:key(fx:key(Existing), Fx),
            db:update(?MODULE, NewFx);
        _ ->
            throw({error, too_many_equal})
    end.
