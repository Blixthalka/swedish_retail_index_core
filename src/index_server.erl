-module(index_server).
-behaviour(gen_server).

-export([
    start_link/0,
    stop/0,
    read/0,
    recalc/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2
]).


start_link() ->
    gen_server:start_link(server_ref(), ?MODULE, [], []).

stop() ->
    gen_server:call(server_ref(), stop).

recalc() ->
    spawn(fun() ->
        State = calculate(),
        gen_server:call(server_ref(), {set_state, State})
    end).

read() ->
    gen_server:call(server_ref(), read).

server_ref() ->
    {global, ?MODULE}.


init(_) ->
    {ok, calculate(), hibernate}.


handle_call(stop, _From, Active) ->
    {stop, normal, ok, Active};
handle_call({set_state, State}, _from, _Active) ->
    {reply, ok, State};
handle_call(read, _From, Active) ->
    {reply, Active, Active};
handle_call(_Request, _From, Active) ->
    {reply, {error, wrong_call}, Active}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

calculate() ->
    io:fwrite("Calculating State\n"),
    All = lists:foldl(fun(Instrument, InstrumentMap) ->
        Points = point:db_find_all(instrument:key(Instrument)),
        PointMap = lists:foldl(fun(Point, Map) ->
            maps:put(point:date(Point), Point, Map)
        end, #{}, Points),
        maps:put(instrument:key(Instrument), {Instrument, PointMap}, InstrumentMap)
    end, #{}, instrument:db_list_instruments()),

    case maps:keys(All) of
        [] ->
            [point:create(<<"1970-01-01">>, sri, calc:zero(), calc:zero())];
        _ ->
            StartDate = <<"2023-01-01">>,

            Res = lists:foldl(fun(Date, Acc) ->
                case Date =:= StartDate of
                    true ->
                        [{Date, calc:to_decimal(<<"100">>)}];
                    false ->
                        case create_day(maps:values(All), Date) of
                            [] ->
                                Acc;
                            Day ->
                                Members = helper:construct_members(Day),
                                IndexChanges = lists:map(fun({Instrument, Point, Weight}) ->
                                    DayPrice = fx_price(Date, point:price(Point), Instrument),
                                    {PrevPriceDate, PrevPrice} = find_prev_price(All, Date, Instrument, Point),
                                    YesterdayPrice = fx_price(PrevPriceDate, PrevPrice,Instrument),
                                    Change = calc:percent_change(YesterdayPrice, DayPrice),
                                    calc:multiply(Change, Weight)
                                end, Members),

                                IndexChange = calc:sum(IndexChanges),
                                {_, PrevValue} = hd(Acc),
                                Value = calc:multiply(calc:add(calc:to_decimal(<<"1">>), IndexChange), PrevValue),

                                [{Date, Value} | Acc]
                        end
                end
            end, [], date_sequence(StartDate, date_util:today())),

            Sequence = lists:reverse(Res),
            lists:map(fun({Date, Price}) ->
                point:create(Date, sri, calc:round(Price, 2), calc:zero())
            end, Sequence)
    end.

fx_price(Date, Price, Instrument) ->
    Fx = fx:db_closest_rate(Date, instrument:currency(Instrument), <<"SEK">>),
    calc:multiply(Price, Fx).

find_prev_price(_All, <<"2021-01-01">>, _Instrument, Point) ->
    {<<"2021-01-01">>, point:price(Point)};
find_prev_price(All, OldDate, Instrument, Point) ->
    Date = date_util:shift(OldDate, -1, days),
    {_, Points} = maps:get(instrument:key(Instrument), All),
    case maps:get(Date, Points, undefined) of
        undefined ->
            find_prev_price(All, Date, Instrument, Point);
        FoundPoint ->
            {Date, point:price(FoundPoint)}
    end.

create_day(List, Date) ->
    lists:filtermap(fun({Instrument, Points}) ->
        case maps:get(Date, Points, undefined) of
            undefined ->
                false;
            Point ->
                {true, {Instrument, Point}}
        end
    end, List).

date_sequence(FromDate, ToDate) ->
    case date_util:is_before(ToDate, FromDate) of
        true ->
            throw({error, bad_date_interval});
        false ->
            date_sequence_calc(FromDate, ToDate)
    end.

date_sequence_calc(Date, Date) ->
    [Date];
date_sequence_calc(Date, ToDate) ->
    [Date] ++ date_sequence_calc(date_util:shift(Date, 1, days), ToDate).


