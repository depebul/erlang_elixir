%%%-------------------------------------------------------------------
%%% @author depebul
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Mar 2025 20:04
%%%-------------------------------------------------------------------
-module(air_quality).
-author("depebul").

-export([samples/0, number_of_readings/2, calculate_max/2, main/1]).

samples() ->
    [
        {"Nowa Hoota", {2025, 03, 22}, {10, 00, 00}, [
            {pm10, 20.5}, {pm25, 12.3}, {temperature, 15.0}
        ]},
        {"Swoshowice", {2025, 03, 23}, {11, 00, 00}, [{pm10, 22.1}, {pm25, 13.8}, {humidity, 60.0}]},
        {"Borek Falecki", {2025, 03, 23}, {12, 00, 00}, [
            {pm10, 18.0}, {pm25, 10.5}, {pressure, 1013.25}
        ]},
        {"Kobierzyn", {2025, 03, 23}, {13, 00, 00}, [
            {pm10, 25.0}, {pm25, 15.0}, {temperature, 16.0}
        ]},
        {"Nowa Huta", {2025, 03, 23}, {14, 30, 00}, [
            {pm10, 21.7}, {pm25, 11.9}, {temperature, 17.2}, {humidity, 55.0}
        ]},
        {"Krowodrza", {2025, 03, 23}, {15, 45, 00}, [
            {pm10, 19.3}, {pm25, 9.8}, {pressure, 1015.5}, {humidity, 62.0}
        ]},
        {"Podgorze", {2025, 03, 22}, {16, 15, 00}, [
            {pm10, 26.2}, {pm25, 14.7}, {temperature, 14.5}, {pressure, 1014.0}
        ]},
        {"Czyzyny", {2025, 03, 23}, {17, 00, 00}, [
            {pm10, 23.8}, {pm25, 13.1}, {humidity, 58.0}, {temperature, 15.8}
        ]}
    ].

number_of_readings(Readings, Date) -> number_of_readings(Readings, Date, 0).

number_of_readings([], _, Acc) -> Acc;
number_of_readings([{_, Date, _, _} | T], Date, Acc) -> number_of_readings(T, Date, Acc + 1);
number_of_readings([_ | T], Date, Acc) -> number_of_readings(T, Date, Acc).

max_value([], _, Max) -> Max;
max_value([{Type, Value} | T], Type, Max) when Value > Max -> max_value(T, Type, Value);
max_value([_ | T], Type, Max) -> max_value(T, Type, Max).

calculate_max(Readings, Type) -> calculate_max(Readings, Type, -1.0).

calculate_max([], _, Max) ->
    Max;
calculate_max([{_, _, _, Measurements} | T], Type, Max) ->
    calculate_max(T, Type, max_value(Measurements, Type, Max)).

mean_value([], _, {Sum, Count}) ->
    {Sum, Count};
mean_value([{Type, Value} | T], Type, {Sum, Count}) ->
    mean_value(T, Type, {Sum + Value, Count + 1});
mean_value([_ | T], Type, Tupel) ->
    mean_value(T, Type, Tupel).

calculate_mean(Readings, Type) -> calculate_mean(Readings, Type, {0.0, 0}).

calculate_mean([], _, {Sum, Count}) ->
    case Count of
        0 -> -1;
        _ -> Sum / Count
    end;
calculate_mean([{_, _, _, Measurements} | T], Type, {Sum, Count}) ->
    calculate_mean(T, Type, mean_value(Measurements, Type, {Sum, Count})).

main(_Args) ->
    Samples = samples(),
    Today = {2025, 03, 23},
    Count = number_of_readings(Samples, Today),
    io:format("Number of readings on ~p: ~p~n", [Today, Count]),

    MeanPM10 = calculate_mean(Samples, pm10),
    io:format("Mean PM10 reading: ~p~n", [MeanPM10]),

    MaxPM10 = calculate_max(Samples, pm10),
    io:format("Maximum PM10 reading: ~p~n", [MaxPM10]).
