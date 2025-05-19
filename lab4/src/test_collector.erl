-module(test_collector).
-export([run/0]).

run() ->
    % Start the application if not already started
    application:start(lab4),

    % Create monitor if needed
    pollution_gen_server:create_monitor(),

    % Create stations in the pollution server
    io:format("Creating stations in pollution server...~n"),
    pollution_gen_server:add_station("Test Station", {5, 5}),
    pollution_gen_server:add_station("Station 2", {10, 20}),

    % Start collector if not already running
    case whereis(pollution_value_collector_gen_statem) of
        undefined -> pollution_value_collector_gen_statem:start_link();
        _ -> ok
    end,

    % Test the workflow
    io:format("Setting station...~n"),
    pollution_value_collector_gen_statem:set_station("Test Station"),

    io:format("Adding values...~n"),
    pollution_value_collector_gen_statem:add_value({{2025, 5, 19}, {12, 0, 0}}, "PM10", 50),
    pollution_value_collector_gen_statem:add_value({{2025, 5, 19}, {13, 0, 0}}, "PM10", 55),

    io:format("Current state: ~p~n", [sys:get_state(pollution_value_collector_gen_statem)]),

    io:format("Storing data...~n"),
    pollution_value_collector_gen_statem:store_data(),

    io:format("Verifying data...~n"),
    Value = pollution_gen_server:get_one_value("Test Station", {{2025, 5, 19}, {12, 0, 0}}, "PM10"),
    io:format("Retrieved value: ~p~n", [Value]),

    % Test second workflow for another station
    io:format("~nStarting second workflow...~n"),
    % Using coordinates
    pollution_value_collector_gen_statem:set_station({10, 20}),
    pollution_value_collector_gen_statem:add_value({{2025, 5, 19}, {14, 0, 0}}, "PM25", 75),
    pollution_value_collector_gen_statem:store_data(),

    % Verify second station data
    Value2 = pollution_gen_server:get_one_value({10, 20}, {{2025, 5, 19}, {14, 0, 0}}, "PM25"),
    io:format("Retrieved value from second station: ~p~n", [Value2]),

    % Test error cases
    io:format("~nTesting error cases...~n"),
    StoreResult = pollution_value_collector_gen_statem:store_data(),
    io:format("Trying to store with no data: ~p~n", [StoreResult]),

    io:format("All tests completed.~n").
