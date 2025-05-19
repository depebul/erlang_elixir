%%%-------------------------------------------------------------------
%%% @author depebul
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([
    start_link/0,
    stop/0,
    crash/0,
    create_monitor/0,
    add_station/2,
    add_value/4,
    remove_value/3,
    get_one_value/3,
    get_station_min/2,
    get_daily_mean/2,
    get_closest_stations/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

crash() ->
    gen_server:cast(?SERVER, crash).

create_monitor() ->
    gen_server:call(?SERVER, create_monitor).

add_station(Name, Coords) ->
    gen_server:call(?SERVER, {add_station, Name, Coords}).

add_value(Station, DateTime, Type, Value) ->
    gen_server:call(?SERVER, {add_value, Station, DateTime, Type, Value}).

remove_value(Station, DateTime, Type) ->
    gen_server:call(?SERVER, {remove_value, Station, DateTime, Type}).

get_one_value(Station, DateTime, Type) ->
    gen_server:call(?SERVER, {get_one_value, Station, DateTime, Type}).

get_station_min(Station, Type) ->
    gen_server:call(?SERVER, {get_station_min, Station, Type}).

get_daily_mean(Type, Date) ->
    gen_server:call(?SERVER, {get_daily_mean, Type, Date}).

get_closest_stations(Station) ->
    gen_server:call(?SERVER, {get_closest_stations, Station}).

init([]) ->
    {ok, pollution:create_monitor()}.

handle_call(create_monitor, _From, _State) ->
    Monitor = pollution:create_monitor(),
    {reply, ok, Monitor};
handle_call({add_station, Name, Coords}, _From, Monitor) ->
    case pollution:add_station(Name, Coords, Monitor) of
        {error, Reason} ->
            {reply, {error, Reason}, Monitor};
        NewMonitor ->
            {reply, ok, NewMonitor}
    end;
handle_call({add_value, Station, DateTime, Type, Value}, _From, Monitor) ->
    case pollution:add_value(Station, DateTime, Type, Value, Monitor) of
        {error, Reason} ->
            {reply, {error, Reason}, Monitor};
        NewMonitor ->
            {reply, ok, NewMonitor}
    end;
handle_call({remove_value, Station, DateTime, Type}, _From, Monitor) ->
    case pollution:remove_value(Station, DateTime, Type, Monitor) of
        {error, Reason} ->
            {reply, {error, Reason}, Monitor};
        NewMonitor ->
            {reply, ok, NewMonitor}
    end;
handle_call({get_one_value, Station, DateTime, Type}, _From, Monitor) ->
    Result = pollution:get_one_value(Station, DateTime, Type, Monitor),
    {reply, Result, Monitor};
handle_call({get_station_min, Station, Type}, _From, Monitor) ->
    Result = pollution:get_station_min(Station, Type, Monitor),
    {reply, Result, Monitor};
handle_call({get_daily_mean, Type, Date}, _From, Monitor) ->
    Result = pollution:get_daily_mean(Type, Date, Monitor),
    {reply, Result, Monitor};
handle_call({get_closest_stations, Station}, _From, Monitor) ->
    Result = pollution:get_closest_stations(Station, Monitor),
    {reply, Result, Monitor}.

handle_cast(stop, Monitor) ->
    {stop, normal, Monitor};
handle_cast(crash, _Monitor) ->
    1 = 2,
    {noreply, _Monitor}.

handle_info(_Info, Monitor) ->
    {noreply, Monitor}.

terminate(_Reason, _Monitor) ->
    ok.

code_change(_OldVsn, Monitor, _Extra) ->
    {ok, Monitor}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
