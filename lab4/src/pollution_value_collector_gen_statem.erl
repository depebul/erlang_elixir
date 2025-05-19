%%%-------------------------------------------------------------------
%%% @author depebul
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. May 2025 14:54
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("depebul").

-behaviour(gen_statem).

%% API
-export([start_link/0, stop/0, set_station/1, add_value/3, store_data/0]).

%% gen_statem callbacks
-export([init/1, handle_event/4, terminate/3, callback_mode/0]).

-define(SERVER, ?MODULE).

-record(pollution_value_collector_gen_statem_state, {
    station,
    measurements = []
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(?SERVER).

set_station(StationIdentifier) ->
    gen_statem:call(?SERVER, {set_station, StationIdentifier}).

add_value(DateTime, Type, Value) ->
    gen_statem:call(?SERVER, {add_value, DateTime, Type, Value}).

store_data() ->
    gen_statem:call(?SERVER, store_data).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([]) ->
    {ok, idle, #pollution_value_collector_gen_statem_state{}}.

callback_mode() ->
    handle_event_function.

handle_event({call, From}, {set_station, StationIdent}, idle, _State) ->
    NewState = #pollution_value_collector_gen_statem_state{
        station = StationIdent, measurements = []
    },
    {next_state, collecting, NewState, [{reply, From, ok}]};
handle_event({call, From}, {set_station, _}, collecting, _State) ->
    {keep_state_and_data, [{reply, From, {error, already_collecting}}]};
handle_event(
    {call, From},
    {add_value, DateTime, Type, Value},
    collecting,
    State = #pollution_value_collector_gen_statem_state{measurements = Measurements}
) ->
    NewState = State#pollution_value_collector_gen_statem_state{
        measurements = [{DateTime, Type, Value} | Measurements]
    },
    {keep_state, NewState, [{reply, From, ok}]};
handle_event({call, From}, {add_value, _DateTime, _Type, _Value}, idle, _State) ->
    {keep_state_and_data, [{reply, From, {error, no_station_selected}}]};
handle_event(
    {call, From},
    store_data,
    collecting,
    #pollution_value_collector_gen_statem_state{station = Station, measurements = Measurements}
) ->
    Result = store_all_measurements(Station, Measurements),
    {next_state, idle, #pollution_value_collector_gen_statem_state{}, [{reply, From, Result}]};
handle_event({call, From}, store_data, idle, _State) ->
    {keep_state_and_data, [{reply, From, {error, no_data_to_store}}]};
handle_event(_EventType, _EventContent, _StateName, State) ->
    {keep_state, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

store_all_measurements(Station, Measurements) ->
    try
        lists:foreach(
            fun({DateTime, Type, Value}) ->
                case pollution_gen_server:add_value(Station, DateTime, Type, Value) of
                    ok -> ok;
                    {error, Reason} -> throw({error, Reason})
                end
            end,
            Measurements
        ),
        ok
    catch
        throw:Error -> Error;
        _:_ -> {error, store_failed}
    end.
