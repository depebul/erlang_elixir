%%%-------------------------------------------------------------------
%%% @author depebul
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% Test suite for pollution_gen_server
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server_tests).
-include_lib("eunit/include/eunit.hrl").

%% Helper function to restart the server with a clean state
setup() ->
    case whereis(pollution_gen_server) of
        undefined ->
            ok;
        _ ->
            pollution_gen_server:stop(),
            % Wait for server to stop
            timer:sleep(100)
    end,
    application:start(lab4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_monitor_test() ->
    setup(),
    ?assertEqual(ok, pollution_gen_server:create_monitor()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_station_test() ->
    setup(),
    ?assertEqual(ok, pollution_gen_server:add_station("Stacja 1", {1, 1})),
    ?assertMatch({error, _}, pollution_gen_server:add_station("Stacja 1", {1, 1})),
    ?assertMatch({error, _}, pollution_gen_server:add_station("Stacja 1", {2, 2})),
    ?assertMatch({error, _}, pollution_gen_server:add_station("Stacja 2", {1, 1})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_test() ->
    setup(),
    pollution_gen_server:add_station("Stacja 1", {1, 1}),
    Time = calendar:local_time(),

    ?assertEqual(ok, pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3)),
    ?assertEqual(ok, pollution_gen_server:add_value("Stacja 1", Time, "PM1", 46.3)),
    ?assertEqual(
        ok, pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10", 46.3)
    ),

    timer:sleep(1100),
    Time2 = calendar:local_time(),
    ?assertEqual(ok, pollution_gen_server:add_value({1, 1}, Time2, "PM10", 46.3)),
    ?assertEqual(ok, pollution_gen_server:add_value({1, 1}, Time2, "PM1", 46.3)),
    ?assertEqual(
        ok, pollution_gen_server:add_value({1, 1}, {{2023, 3, 27}, {11, 16, 10}}, "PM10", 46.3)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_fail_test() ->
    setup(),
    pollution_gen_server:add_station("Stacja 1", {1, 1}),
    Time = calendar:local_time(),

    ?assertEqual(ok, pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3)),
    ?assertMatch({error, _}, pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3)),
    ?assertMatch({error, _}, pollution_gen_server:add_value("Stacja 1", Time, "PM10", 36.3)),
    ?assertMatch({error, _}, pollution_gen_server:add_value({1, 1}, Time, "PM10", 46.3)),
    ?assertMatch({error, _}, pollution_gen_server:add_value({1, 1}, Time, "PM10", 36.3)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_non_existing_station_test() ->
    setup(),
    pollution_gen_server:add_station("Stacja 1", {1, 1}),

    ?assertMatch(
        {error, _}, pollution_gen_server:add_value("Stacja 2", calendar:local_time(), "PM10", 46.3)
    ),
    ?assertMatch(
        {error, _}, pollution_gen_server:add_value({1, 2}, calendar:local_time(), "PM10", 46.3)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_test() ->
    setup(),
    pollution_gen_server:add_station("Stacja 1", {1, 1}),
    Time = calendar:local_time(),

    pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_gen_server:add_value("Stacja 1", Time, "PM1", 46.3),
    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10", 46.3),

    ?assertEqual(ok, pollution_gen_server:remove_value("Stacja 1", Time, "PM10")),
    ?assertEqual(
        ok, pollution_gen_server:remove_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10")
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_and_add_back_test() ->
    setup(),
    pollution_gen_server:add_station("Stacja 1", {1, 1}),
    Time = {{2023, 3, 27}, {11, 16, 9}},

    pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
    ?assertEqual(46.3, pollution_gen_server:get_one_value("Stacja 1", Time, "PM10")),

    pollution_gen_server:remove_value("Stacja 1", Time, "PM10"),
    ?assertMatch({error, _}, pollution_gen_server:get_one_value("Stacja 1", Time, "PM10")),

    pollution_gen_server:add_value({1, 1}, Time, "PM10", 46.3),
    ?assertEqual(46.3, pollution_gen_server:get_one_value({1, 1}, Time, "PM10")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_fail_test() ->
    setup(),
    pollution_gen_server:add_station("Stacja 1", {1, 1}),
    Time = calendar:local_time(),

    pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_gen_server:add_value("Stacja 1", Time, "PM1", 46.3),
    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10", 46.3),

    ?assertMatch({error, _}, pollution_gen_server:remove_value("Stacja 1", Time, "PM25")),
    ?assertMatch(
        {error, _},
        pollution_gen_server:remove_value("Stacja 1", {{2023, 3, 27}, {11, 16, 10}}, "PM10")
    ),
    ?assertMatch({error, _}, pollution_gen_server:remove_value({1, 2}, Time, "PM10")),
    ?assertMatch({error, _}, pollution_gen_server:remove_value("Stacja 2", Time, "PM10")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_test() ->
    setup(),
    pollution_gen_server:add_station("Stacja 1", {1, 1}),
    Time = calendar:local_time(),

    pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_gen_server:add_value("Stacja 1", Time, "PM1", 36.3),
    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10", 26.3),

    ?assertEqual(46.3, pollution_gen_server:get_one_value("Stacja 1", Time, "PM10")),
    ?assertEqual(36.3, pollution_gen_server:get_one_value("Stacja 1", Time, "PM1")),
    ?assertEqual(46.3, pollution_gen_server:get_one_value({1, 1}, Time, "PM10")),
    ?assertEqual(
        26.3, pollution_gen_server:get_one_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10")
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_fail_test() ->
    setup(),
    pollution_gen_server:add_station("Stacja 1", {1, 1}),
    Time = calendar:local_time(),

    pollution_gen_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_gen_server:add_value("Stacja 1", Time, "PM1", 36.3),
    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10", 26.3),

    ?assertMatch({error, _}, pollution_gen_server:get_one_value("Stacja 1", Time, "PM25")),
    ?assertMatch({error, _}, pollution_gen_server:get_one_value({1, 1}, Time, "PM25")),
    ?assertMatch(
        {error, _},
        pollution_gen_server:get_one_value("Stacja 1", {{2023, 3, 27}, {11, 16, 10}}, "PM10")
    ),
    ?assertMatch({error, _}, pollution_gen_server:get_one_value("Stacja 2", Time, "PM1")),
    ?assertMatch({error, _}, pollution_gen_server:get_one_value({1, 2}, Time, "PM10")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_test() ->
    setup(),
    pollution_gen_server:add_station("Stacja 1", {1, 1}),

    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 10}}, "PM10", 10),
    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 11}}, "PM10", 20),
    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 12}}, "PM10", 10),
    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 13}}, "PM10", 20),

    ?assertEqual(10, pollution_gen_server:get_station_min("Stacja 1", "PM10")),
    ?assertEqual(10, pollution_gen_server:get_station_min({1, 1}, "PM10")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_fail_test() ->
    setup(),
    pollution_gen_server:add_station("Stacja 1", {1, 1}),

    ?assertMatch({error, _}, pollution_gen_server:get_station_min("Stacja 1", "PM10")),

    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 10}}, "PM10", 10),
    ?assertMatch({error, _}, pollution_gen_server:get_station_min("Stacja 1", "PM25")),
    ?assertMatch({error, _}, pollution_gen_server:get_station_min("Stacja 2", "PM25")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_test() ->
    setup(),
    pollution_gen_server:add_station("Stacja 1", {1, 1}),
    pollution_gen_server:add_station("Stacja 2", {2, 2}),
    pollution_gen_server:add_station("Stacja 3", {3, 3}),

    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 10}}, "PM10", 10),
    pollution_gen_server:add_value("Stacja 2", {{2023, 3, 27}, {11, 16, 11}}, "PM10", 20),
    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 12}}, "PM10", 10),
    pollution_gen_server:add_value("Stacja 2", {{2023, 3, 27}, {11, 16, 13}}, "PM10", 20),

    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 14}}, "PM25", 100),
    pollution_gen_server:add_value("Stacja 2", {{2023, 3, 27}, {11, 16, 15}}, "PM25", 220),

    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 28}, {11, 16, 16}}, "PM10", 2000),
    pollution_gen_server:add_value("Stacja 2", {{2023, 3, 28}, {11, 16, 17}}, "PM10", 3000),

    pollution_gen_server:add_value("Stacja 3", {{2023, 3, 27}, {11, 16, 18}}, "PM10", 1234),

    ?assertEqual(160.0, pollution_gen_server:get_daily_mean("PM25", {2023, 3, 27})),
    ?assertEqual(258.8, pollution_gen_server:get_daily_mean("PM10", {2023, 3, 27})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_fail_test() ->
    setup(),
    pollution_gen_server:add_station("Stacja 1", {1, 1}),
    pollution_gen_server:add_station("Stacja 2", {2, 2}),

    ?assertMatch({error, _}, pollution_gen_server:get_daily_mean("PM10", {2023, 3, 27})),

    pollution_gen_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 10}}, "PM10", 10),
    pollution_gen_server:add_value("Stacja 2", {{2023, 3, 27}, {11, 16, 11}}, "PM10", 20),

    ?assertMatch({error, _}, pollution_gen_server:get_daily_mean("PM25", {2023, 3, 27})),
    ?assertMatch({error, _}, pollution_gen_server:get_daily_mean("PM10", {2023, 3, 29})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_closest_stations_test() ->
    setup(),
    pollution_gen_server:add_station("Stacja 1", {1.0, 1.0}),
    pollution_gen_server:add_station("Stacja 2", {2.5, 2.1}),
    pollution_gen_server:add_station("Stacja 3", {3.2, 3.8}),
    pollution_gen_server:add_station("Stacja 4", {1.5, 1.7}),
    pollution_gen_server:add_station("Stacja 5", {5.9, 5.2}),
    pollution_gen_server:add_station("Stacja 6", {6.3, 6.7}),
    pollution_gen_server:add_station("Stacja 7", {7.1, 7.8}),
    pollution_gen_server:add_station("Stacja 8", {8.4, 8.0}),
    pollution_gen_server:add_station("Stacja 9", {9.7, 9.3}),
    pollution_gen_server:add_station("Stacja 10", {10.2, 10.5}),

    Result = pollution_gen_server:get_closest_stations("Stacja 1"),
    ?assertMatch(
        [
            {"Stacja 4", {1.5, 1.7}, 0.8602325267042626},
            {"Stacja 2", {2.5, 2.1}, 1.8601075237738274},
            {"Stacja 3", {3.2, 3.8}, 3.560898762952971},
            {"Stacja 5", {5.9, 5.2}, 6.4536811201050215}
        ],
        Result
    ),

    ?assertMatch(
        {error, station_not_found}, pollution_gen_server:get_closest_stations("Stacja 11")
    ),
    ?assertMatch({error, station_not_found}, pollution_gen_server:get_closest_stations({1.2, 1.0})).

collector_setup() ->
    setup(),

    pollution_gen_server:add_station("Test Station", {5, 5}),
    pollution_gen_server:add_station("Station 2", {10, 20}),

    case whereis(pollution_value_collector_gen_statem) of
        undefined ->
            pollution_value_collector_gen_statem:start_link();
        _ ->
            ok
    end.

collector_workflow_test() ->
    collector_setup(),

    ?assertEqual(ok, pollution_value_collector_gen_statem:set_station("Test Station")),
    ?assertEqual(
        ok, pollution_value_collector_gen_statem:add_value({{2025, 5, 19}, {12, 0, 0}}, "PM10", 50)
    ),
    ?assertEqual(
        ok, pollution_value_collector_gen_statem:add_value({{2025, 5, 19}, {13, 0, 0}}, "PM10", 55)
    ),
    ?assertEqual(ok, pollution_value_collector_gen_statem:store_data()),

    ?assertEqual(
        50, pollution_gen_server:get_one_value("Test Station", {{2025, 5, 19}, {12, 0, 0}}, "PM10")
    ),
    ?assertEqual(
        55, pollution_gen_server:get_one_value("Test Station", {{2025, 5, 19}, {13, 0, 0}}, "PM10")
    ),

    ?assertEqual(ok, pollution_value_collector_gen_statem:set_station({10, 20})),
    ?assertEqual(
        ok, pollution_value_collector_gen_statem:add_value({{2025, 5, 19}, {14, 0, 0}}, "PM25", 75)
    ),
    ?assertEqual(ok, pollution_value_collector_gen_statem:store_data()),

    ?assertEqual(
        75, pollution_gen_server:get_one_value({10, 20}, {{2025, 5, 19}, {14, 0, 0}}, "PM25")
    ).

collector_error_cases_test() ->
    collector_setup(),

    ?assertMatch({error, no_data_to_store}, pollution_value_collector_gen_statem:store_data()),
    ?assertMatch(
        {error, no_station_selected},
        pollution_value_collector_gen_statem:add_value({{2025, 5, 19}, {15, 0, 0}}, "PM10", 60)
    ),

    pollution_value_collector_gen_statem:set_station("Test Station"),

    ?assertMatch(
        {error, already_collecting}, pollution_value_collector_gen_statem:set_station("Station 2")
    ),

    pollution_value_collector_gen_statem:store_data().
