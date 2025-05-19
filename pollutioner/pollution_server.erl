%%%-------------------------------------------------------------------
%%% @author Dawid Żak
%%% @copyright (C) 2025, Dawid Żak
%%% @doc
%%% Server module for pollution monitoring system
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_server).
-export([
    start/0,
    stop/0,
    init/0,
    create_monitor/0,
    add_station/2,
    add_value/4,
    remove_value/3,
    get_one_value/3,
    get_station_min/2,
    get_daily_mean/2,
    get_closest_stations/1
]).

start() ->
    case whereis(pollution_server) of
        undefined -> register(pollution_server, spawn(?MODULE, init, []));
        _ -> {error, already_started}
    end.

stop() ->
    pollution_server ! {self(), stop},
    receive
        {pollution_server, stopped} -> ok
    after 1000 ->
        {error, timeout}
    end.

init() ->
    Monitor = pollution:create_monitor(),
    loop(Monitor).

loop(Monitor) ->
    receive
        {Pid, stop} ->
            Pid ! {pollution_server, stopped};
        {Pid, create_monitor} ->
            NewMonitor = pollution:create_monitor(),
            Pid ! {pollution_server, ok},
            loop(NewMonitor);
        {Pid, add_station, Name, Location} ->
            case pollution:add_station(Name, Location, Monitor) of
                {error, Reason} ->
                    Pid ! {pollution_server, {error, Reason}},
                    loop(Monitor);
                NewMonitor ->
                    Pid ! {pollution_server, ok},
                    loop(NewMonitor)
            end;
        {Pid, add_value, NameLocation, Time, Type, Value} ->
            case pollution:add_value(NameLocation, Time, Type, Value, Monitor) of
                {error, Reason} ->
                    Pid ! {pollution_server, {error, Reason}},
                    loop(Monitor);
                NewMonitor ->
                    Pid ! {pollution_server, ok},
                    loop(NewMonitor)
            end;
        {Pid, remove_value, NameLocation, Time, Type} ->
            case pollution:remove_value(NameLocation, Time, Type, Monitor) of
                {error, Reason} ->
                    Pid ! {pollution_server, {error, Reason}},
                    loop(Monitor);
                NewMonitor ->
                    Pid ! {pollution_server, ok},
                    loop(NewMonitor)
            end;
        {Pid, get_one_value, NameLocation, Time, Type} ->
            Result = pollution:get_one_value(NameLocation, Time, Type, Monitor),
            Pid ! {pollution_server, Result},
            loop(Monitor);
        {Pid, get_station_min, NameLocation, Type} ->
            Result = pollution:get_station_min(NameLocation, Type, Monitor),
            Pid ! {pollution_server, Result},
            loop(Monitor);
        {Pid, get_daily_mean, Type, Day} ->
            Result = pollution:get_daily_mean(Type, Day, Monitor),
            Pid ! {pollution_server, Result},
            loop(Monitor);
        {Pid, get_closest_stations, NameLocation} ->
            Result = pollution:get_closest_stations(NameLocation, Monitor),
            Pid ! {pollution_server, Result},
            loop(Monitor);
        _ ->
            loop(Monitor)
    end.

create_monitor() ->
    pollution_server ! {self(), create_monitor},
    receive
        {pollution_server, Response} -> Response
    after 1000 ->
        {error, timeout}
    end.

add_station(Name, Location) ->
    pollution_server ! {self(), add_station, Name, Location},
    receive
        {pollution_server, Response} -> Response
    after 1000 ->
        {error, timeout}
    end.

add_value(NameLocation, Time, Type, Value) ->
    pollution_server ! {self(), add_value, NameLocation, Time, Type, Value},
    receive
        {pollution_server, Response} -> Response
    after 1000 ->
        {error, timeout}
    end.

remove_value(NameLocation, Time, Type) ->
    pollution_server ! {self(), remove_value, NameLocation, Time, Type},
    receive
        {pollution_server, Response} -> Response
    after 1000 ->
        {error, timeout}
    end.

get_one_value(NameLocation, Time, Type) ->
    pollution_server ! {self(), get_one_value, NameLocation, Time, Type},
    receive
        {pollution_server, Response} -> Response
    after 1000 ->
        {error, timeout}
    end.

get_station_min(NameLocation, Type) ->
    pollution_server ! {self(), get_station_min, NameLocation, Type},
    receive
        {pollution_server, Response} -> Response
    after 1000 ->
        {error, timeout}
    end.

get_daily_mean(Type, Day) ->
    pollution_server ! {self(), get_daily_mean, Type, Day},
    receive
        {pollution_server, Response} -> Response
    after 1000 ->
        {error, timeout}
    end.

get_closest_stations(NameLocation) ->
    pollution_server ! {self(), get_closest_stations, NameLocation},
    receive
        {pollution_server, Response} -> Response
    after 1000 ->
        {error, timeout}
    end.
