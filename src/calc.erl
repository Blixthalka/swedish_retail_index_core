-module(calc).

-export([
    to_decimal/1,
    to_binary/1,
    round/1,
    ejson_format/1,
    compare/2,
    zero/0,
    add/2,
    sub/2,
    divide/2,
    round/2,
    multiply/2,
    sum/1,
    min/1,
    max/1
]).

-define(PRECISION, 32).

add(A, B) ->
    decimal:add(A, B).


sub(A, B) ->
    decimal:sub(A, B).

to_decimal(Binary) ->
    Replaced = binary:replace(Binary, <<",">>, <<".">>),
    decimal_conv:from_binary(Replaced).

to_binary(Decimal) ->
    decimal_conv:to_binary(Decimal, #{ pretty => true }).

ejson_format(Decimal) ->
    binary:replace(to_binary(calc:round(Decimal)), <<".0">>, <<>>).

round(Decimal) ->
    decimal:round(round_half_up, Decimal, 0).

zero() ->
    calc:to_decimal(<<"0.0">>).

divide(A, B) ->
    decimal:divide(A, B, #{ precision => ?PRECISION, rounding => round_half_up}).


multiply(A, B) ->
    decimal:mult(A, B).

compare(A, B) ->
    decimal:cmp(A, B, #{ precision => ?PRECISION, rounding => round_half_up}).

round(Decimal, Decimals) ->
    decimal:round(round_half_up, Decimal, Decimals).

sum(List) ->
    lists:foldl(fun(V, Acc) ->
        calc:add(V, Acc)
    end, calc:zero(), List).


min([]) ->
    calc:zero();
min([H|T]) ->
    lists:foldl(fun(V, Acc) ->
        case compare(V, Acc) < 0 of
            true ->
                V;
            false ->
                Acc
        end
    end, H, T).

max([]) ->
    calc:zero();
max([H|T]) ->
    lists:foldl(fun(V, Acc) ->
        case compare(V, Acc) > 0 of
            true ->
                V;
            false ->
                Acc
        end
    end, H, T).