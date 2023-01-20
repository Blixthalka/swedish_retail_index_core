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
    end, #{}, instrument:db_list()),

    case maps:keys(All) of
        [] ->
            {<<"1970-01-01">>, calc:zero(), calc:zero(), []};
        _ ->
            StartDate = <<"2023-01-01">>,

            Res = lists:foldl(fun(Date, Acc) ->
                case Date =:= StartDate of
                    true ->
                        [{Date, calc:zero(), calc:to_decimal(<<"100">>)}];
                    false ->
                        case create_day(maps:values(All), Date) of
                            [] ->
                                Acc;
                            Day ->
                                Members = helper:construct_members(Day),
                                IndexChanges = lists:map(fun({Instrument, Point, Weight}) ->
                                    Fx = fx:db_closest_rate(Date, instrument:currency(Instrument), <<"SEK">>),
                                    DayPrice = calc:multiply(point:price(Point), Fx),
                                    YesterdayPrice = calc:multiply(find_prev_price(All, Date, Instrument, Point), Fx),
                                    Change = change(YesterdayPrice, DayPrice),
                                    calc:multiply(Change, Weight)
                                end, Members),

                                IndexChange = calc:sum(IndexChanges),
                                {_, _, PrevValue} = hd(Acc),
                                Value = calc:multiply(calc:add(calc:to_decimal(<<"1">>), IndexChange), PrevValue),

                                [{Date, IndexChange, Value} | Acc]
                        end
                end
            end, [], date_sequence(StartDate, date_util:today())),

            {LastDate, _, LastValue} = hd(Res),
            Sequence = lists:reverse(Res),
            {_, _, FirstValue} = hd(Sequence),
            {LastDate, LastValue, change(FirstValue, LastValue), Sequence}
    end.

change(Start, End) ->
    calc:divide(calc:sub(End, Start), Start).

find_prev_price(All, OldDate, Instrument, Point) ->
    Date = date_util:shift(OldDate, -1, days),
    {_, Points} = maps:get(instrument:key(Instrument), All),
    case maps:get(Date, Points, undefined) of
        undefined ->
            find_prev_price(All, Date, Instrument, Point);
        FoundPoint ->
            point:price(FoundPoint)
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


