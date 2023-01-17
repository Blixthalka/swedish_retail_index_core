-module(db).

-include_lib("stdlib/include/qlc.hrl").

-export([
    start_mnesia/0,
    create/2,
    delete/2,
    read/2,
    update/2,
    list/1,
    create_table/2,
    generate_id/0
]).

-define(T(F), mnesia:activity(transaction, F)).

-define(TABLES, [
    instrument,
    point,
    fx
]).


update(Module, Entity) ->
    case read(Module, Module:key(Entity)) of
        {error, not_found} ->
            create(Module, Entity);
        {ok, OldEntity} ->
            delete(Module, OldEntity),
            write(Module, Entity)
    end.

read(Module, Key) ->
    Res = lists:filter(fun(Entity) ->
        Module:key(Entity) =:= Key
    end, list(Module)),
    case Res of
        [] ->
            {error, not_found};
        [Entity] ->
            {ok, Entity}
    end.


create(Module, Entity) ->
    EntityWithKey = Module:key(generate_id(), Entity),
    write(Module, EntityWithKey).


delete(Module, Entity) ->
    ?T(fun() ->
        mnesia:delete_object(Module, Entity, write)
    end),
    ok.


write(Module, EntityWithKey) ->
    ok = ?T(fun() ->
        mnesia:write(Module, EntityWithKey, write)
    end),
    {ok, EntityWithKey}.

create_table(Module, Fields) ->
    mnesia:create_table(Module, [
        {attributes, Fields},
        {disc_copies, [node()]}
    ]).

list(Module) ->
    ?T(fun() ->
		Q = qlc:q([E || E <- mnesia:table(Module), true]),
		qlc:e(Q)
    end).

-spec generate_id() -> binary().
generate_id() ->
    Time = erlang:monotonic_time(),
    Counter = erlang:unique_integer([positive, monotonic]),
    Unique = Time + Counter,
    Hash = crypto:hash(md5, integer_to_binary(Unique)),
    binary:encode_hex(Hash).


start_mnesia() ->
    application:set_env(mnesia, dir, "./mnesia"),
    mnesia:stop(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    lists:foreach(fun(Table) ->
        Table:db_create_table()
    end, ?TABLES),
    mnesia:wait_for_tables(?TABLES, infinity),
    ok.
