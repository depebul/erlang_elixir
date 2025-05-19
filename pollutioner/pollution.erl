%%% @author Dawid Żak
%%% @copyright (C) 2025, Dawid Żak
%%% @doc
%%%
%%% @end
%%% Created : 06 Apr 2025 by Dawid Żak
-module(pollution).

-record(measurement, {
    type,
    time,
    value
}).

-record(station, {
    name,
    location,
    measurements = []
}).

-export([
    create_monitor/0,
    add_station/3,
    add_value/5,
    remove_value/4,
    get_one_value/4,
    get_station_min/3,
    get_daily_mean/3,
    get_closest_stations/2
]).

create_monitor() ->
    [].

add_station(Name, Location, Monitor) ->
    case
        lists:filter(
            fun(S) -> S#station.name =:= Name orelse S#station.location =:= Location end,
            Monitor
        )
    of
        [] ->
            Station = #station{name = Name, location = Location, measurements = []},

            [Station | Monitor];
        _ ->
            {error, station_already_exists}
    end.

add_value(NameLocation, Time, Measurement_type, Value, Monitor) ->
    case
        lists:filter(
            fun(S) ->
                S#station.name =:= NameLocation orelse S#station.location =:= NameLocation
            end,
            Monitor
        )
    of
        [] ->
            {error, station_not_found};
        [Station] ->
            case
                lists:filter(
                    fun(M) ->
                        M#measurement.type =:= Measurement_type andalso M#measurement.time =:= Time
                    end,
                    Station#station.measurements
                )
            of
                [] ->
                    Measurement = #measurement{type = Measurement_type, time = Time, value = Value},
                    NewStation = Station#station{
                        measurements = [Measurement | Station#station.measurements]
                    },

                    [NewStation | lists:delete(Station, Monitor)];
                _ ->
                    {error, measurement_already_exists}
            end
    end.

remove_value(NameLocation, Time, Measurement_type, Monitor) ->
    case
        lists:filter(
            fun(S) ->
                S#station.name =:= NameLocation orelse S#station.location =:= NameLocation
            end,
            Monitor
        )
    of
        [] ->
            {error, station_not_found};
        [Station] ->
            case
                lists:filter(
                    fun(M) ->
                        M#measurement.type =:= Measurement_type andalso M#measurement.time =:= Time
                    end,
                    Station#station.measurements
                )
            of
                [] ->
                    {error, measurement_not_found};
                [Measurement] ->
                    UpdatedStation = Station#station{
                        measurements = lists:delete(Measurement, Station#station.measurements)
                    },

                    [UpdatedStation | lists:delete(Station, Monitor)]
            end
    end.

get_one_value(NameLocation, Time, Measurement_type, Monitor) ->
    case
        lists:filter(
            fun(S) ->
                S#station.name =:= NameLocation orelse S#station.location =:= NameLocation
            end,
            Monitor
        )
    of
        [] ->
            {error, station_not_found};
        [Station] ->
            case
                lists:filter(
                    fun(M) ->
                        M#measurement.type =:= Measurement_type andalso M#measurement.time =:= Time
                    end,
                    Station#station.measurements
                )
            of
                [] ->
                    {error, measurement_not_found};
                [Measurement] ->
                    Measurement#measurement.value
            end
    end.

get_station_min(NameLocation, Measurement_type, Monitor) ->
    case
        lists:filter(
            fun(S) ->
                S#station.name =:= NameLocation orelse S#station.location =:= NameLocation
            end,
            Monitor
        )
    of
        [] ->
            {error, station_not_found};
        [Station] ->
            case
                lists:filter(
                    fun(M) -> M#measurement.type =:= Measurement_type end,
                    Station#station.measurements
                )
            of
                [] ->
                    {error, measurement_not_found};
                Measurements ->
                    MinValue = lists:min(
                        lists:map(fun(M) -> M#measurement.value end, Measurements)
                    ),
                    MinValue
            end
    end.

get_daily_mean(Measurement_type, Day, Monitor) ->
    AllMeasurements = lists:flatmap(
        fun(Station) ->
            lists:filter(
                fun(M) ->
                    M#measurement.type =:= Measurement_type andalso
                        element(1, M#measurement.time) =:= Day
                end,
                Station#station.measurements
            )
        end,
        Monitor
    ),

    case AllMeasurements of
        [] ->
            {error, no_matching_measurements};
        Measurements ->
            Values = lists:map(fun(M) -> M#measurement.value end, Measurements),
            lists:sum(Values) / length(Values)
    end.

get_euclidean_distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt((X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2)).

get_closest_stations(NameLocation, Monitor) ->
    case
        lists:filter(
            fun(S) ->
                S#station.name =:= NameLocation orelse S#station.location =:= NameLocation
            end,
            Monitor
        )
    of
        [] ->
            {error, station_not_found};
        [Station] ->
            DistanceList = lists:map(
                fun(S) ->
                    {
                        S#station.name,
                        S#station.location,
                        get_euclidean_distance(Station#station.location, S#station.location)
                    }
                end,
                lists:filter(fun(S) -> S#station.name =/= Station#station.name end, Monitor)
            ),

            SortedByDistance = lists:keysort(3, DistanceList),
            lists:sublist(SortedByDistance, 4)
    end.
