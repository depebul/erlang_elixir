%%%-------------------------------------------------------------------
%%% @author Już nie wojturek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. mar 2019 12:50
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("Wojciech Turek").

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_monitor_test() ->
    pollution_server:start(),
    Result = pollution_server:create_monitor(),
    pollution_server:stop(),
    ?assertEqual(ok, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_station_test() ->
    pollution_server:start(),
    Result1 = pollution_server:add_station("Stacja 1", {1, 1}),
    Result2 = pollution_server:add_station("Stacja 1", {1, 1}),
    Result3 = pollution_server:add_station("Stacja 1", {2, 2}),
    Result4 = pollution_server:add_station("Stacja 2", {1, 1}),
    pollution_server:stop(),
    ?assertEqual(ok, Result1),
    ?assertMatch({error, _}, Result2),
    ?assertMatch({error, _}, Result3),
    ?assertMatch({error, _}, Result4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_test() ->
    pollution_server:start(),
    pollution_server:add_station("Stacja 1", {1, 1}),
    Time = calendar:local_time(),

    Result1 = pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
    Result2 = pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
    Result3 = pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10", 46.3),

    timer:sleep(1100),
    Time2 = calendar:local_time(),

    Result4 = pollution_server:add_value({1, 1}, Time2, "PM10", 46.3),
    Result5 = pollution_server:add_value({1, 1}, Time2, "PM1", 46.3),
    Result6 = pollution_server:add_value({1, 1}, {{2023, 3, 27}, {11, 16, 10}}, "PM10", 46.3),

    pollution_server:stop(),

    ?assertEqual(ok, Result1),
    ?assertEqual(ok, Result2),
    ?assertEqual(ok, Result3),
    ?assertEqual(ok, Result4),
    ?assertEqual(ok, Result5),
    ?assertEqual(ok, Result6).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_fail_test() ->
    pollution_server:start(),
    pollution_server:add_station("Stacja 1", {1, 1}),
    Time = calendar:local_time(),
    pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),

    Result1 = pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
    Result2 = pollution_server:add_value("Stacja 1", Time, "PM10", 36.3),
    Result3 = pollution_server:add_value({1, 1}, Time, "PM10", 46.3),
    Result4 = pollution_server:add_value({1, 1}, Time, "PM10", 36.3),

    pollution_server:stop(),

    ?assertMatch({error, _}, Result1),
    ?assertMatch({error, _}, Result2),
    ?assertMatch({error, _}, Result3),
    ?assertMatch({error, _}, Result4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_non_existing_station_test() ->
    pollution_server:start(),
    pollution_server:add_station("Stacja 1", {1, 1}),

    Result1 = pollution_server:add_value("Stacja 2", calendar:local_time(), "PM10", 46.3),
    Result2 = pollution_server:add_value({1, 2}, calendar:local_time(), "PM10", 46.3),

    pollution_server:stop(),

    ?assertMatch({error, _}, Result1),
    ?assertMatch({error, _}, Result2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_test() ->
    pollution_server:start(),
    pollution_server:add_station("Stacja 1", {1, 1}),
    Time = calendar:local_time(),

    pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10", 46.3),

    Result1 = pollution_server:remove_value("Stacja 1", Time, "PM10"),
    Result2 = pollution_server:remove_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10"),

    pollution_server:stop(),

    ?assertEqual(ok, Result1),
    ?assertEqual(ok, Result2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_and_add_back_test() ->
    pollution_server:start(),
    pollution_server:add_station("Stacja 1", {1, 1}),
    Time = calendar:local_time(),

    pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10", 46.3),

    pollution_server:remove_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10"),
    Result = pollution_server:add_value({1, 1}, {{2023, 3, 27}, {11, 16, 9}}, "PM10", 46.3),

    pollution_server:stop(),

    ?assertEqual(ok, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_fail_test() ->
    pollution_server:start(),
    pollution_server:add_station("Stacja 1", {1, 1}),
    Time = calendar:local_time(),

    pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10", 46.3),

    Result1 = pollution_server:remove_value("Stacja 1", Time, "PM25"),
    Result2 = pollution_server:remove_value("Stacja 1", {{2023, 3, 27}, {11, 16, 10}}, "PM10"),
    Result3 = pollution_server:remove_value({1, 2}, Time, "PM10"),
    Result4 = pollution_server:remove_value("Stacja 2", Time, "PM10"),

    pollution_server:stop(),

    ?assertMatch({error, _}, Result1),
    ?assertMatch({error, _}, Result2),
    ?assertMatch({error, _}, Result3),
    ?assertMatch({error, _}, Result4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_test() ->
    pollution_server:start(),
    pollution_server:add_station("Stacja 1", {1, 1}),
    Time = calendar:local_time(),

    pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_server:add_value("Stacja 1", Time, "PM1", 36.3),
    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10", 26.3),

    Result1 = pollution_server:get_one_value("Stacja 1", Time, "PM10"),
    Result2 = pollution_server:get_one_value("Stacja 1", Time, "PM1"),
    Result3 = pollution_server:get_one_value({1, 1}, Time, "PM10"),
    Result4 = pollution_server:get_one_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10"),

    pollution_server:stop(),

    ?assertEqual(46.3, Result1),
    ?assertEqual(36.3, Result2),
    ?assertEqual(46.3, Result3),
    ?assertEqual(26.3, Result4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_fail_test() ->
    pollution_server:start(),
    pollution_server:add_station("Stacja 1", {1, 1}),
    Time = calendar:local_time(),

    pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
    pollution_server:add_value("Stacja 1", Time, "PM1", 36.3),
    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 9}}, "PM10", 26.3),

    Result1 = pollution_server:get_one_value("Stacja 1", Time, "PM25"),
    Result2 = pollution_server:get_one_value({1, 1}, Time, "PM25"),
    Result3 = pollution_server:get_one_value("Stacja 1", {{2023, 3, 27}, {11, 16, 10}}, "PM10"),
    Result4 = pollution_server:get_one_value("Stacja 2", Time, "PM1"),
    Result5 = pollution_server:get_one_value({1, 2}, Time, "PM10"),

    pollution_server:stop(),

    ?assertMatch({error, _}, Result1),
    ?assertMatch({error, _}, Result2),
    ?assertMatch({error, _}, Result3),
    ?assertMatch({error, _}, Result4),
    ?assertMatch({error, _}, Result5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_test() ->
    pollution_server:start(),
    pollution_server:add_station("Stacja 1", {1, 1}),

    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 10}}, "PM10", 10),
    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 11}}, "PM10", 20),
    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 12}}, "PM10", 10),
    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 13}}, "PM10", 20),

    Result1 = pollution_server:get_station_min("Stacja 1", "PM10"),
    Result2 = pollution_server:get_station_min({1, 1}, "PM10"),

    pollution_server:stop(),

    ?assertEqual(10, Result1),
    ?assertEqual(10, Result2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_fail_test() ->
    pollution_server:start(),
    pollution_server:add_station("Stacja 1", {1, 1}),

    Result1 = pollution_server:get_station_min("Stacja 1", "PM10"),

    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 10}}, "PM10", 10),

    Result2 = pollution_server:get_station_min("Stacja 1", "PM25"),
    Result3 = pollution_server:get_station_min("Stacja 2", "PM25"),

    pollution_server:stop(),

    ?assertMatch({error, _}, Result1),
    ?assertMatch({error, _}, Result2),
    ?assertMatch({error, _}, Result3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_test() ->
    pollution_server:start(),
    pollution_server:add_station("Stacja 1", {1, 1}),
    pollution_server:add_station("Stacja 2", {2, 2}),
    pollution_server:add_station("Stacja 3", {3, 3}),

    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 10}}, "PM10", 10),
    pollution_server:add_value("Stacja 2", {{2023, 3, 27}, {11, 16, 11}}, "PM10", 20),
    Result1 = pollution_server:get_daily_mean("PM10", {2023, 3, 27}),

    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 12}}, "PM10", 10),
    pollution_server:add_value("Stacja 2", {{2023, 3, 27}, {11, 16, 13}}, "PM10", 20),

    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 14}}, "PM25", 100),
    pollution_server:add_value("Stacja 2", {{2023, 3, 27}, {11, 16, 15}}, "PM25", 220),

    pollution_server:add_value("Stacja 1", {{2023, 3, 28}, {11, 16, 16}}, "PM10", 2000),
    pollution_server:add_value("Stacja 2", {{2023, 3, 28}, {11, 16, 17}}, "PM10", 3000),

    pollution_server:add_value("Stacja 3", {{2023, 3, 27}, {11, 16, 18}}, "PM10", 1234),
    Result2 = pollution_server:get_daily_mean("PM10", {2023, 3, 27}),

    pollution_server:stop(),

    ?assertEqual(15.0, Result1),
    ?assertEqual(258.8, Result2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_fail_test() ->
    pollution_server:start(),
    pollution_server:add_station("Stacja 1", {1, 1}),
    pollution_server:add_station("Stacja 2", {2, 2}),

    Result1 = pollution_server:get_daily_mean("PM10", {2023, 3, 27}),

    pollution_server:add_value("Stacja 1", {{2023, 3, 27}, {11, 16, 10}}, "PM10", 10),
    pollution_server:add_value("Stacja 2", {{2023, 3, 27}, {11, 16, 11}}, "PM10", 20),

    Result2 = pollution_server:get_daily_mean("PM25", {2023, 3, 27}),
    Result3 = pollution_server:get_daily_mean("PM10", {2023, 3, 29}),

    pollution_server:stop(),

    ?assertMatch({error, _}, Result1),
    ?assertMatch({error, _}, Result2),
    ?assertMatch({error, _}, Result3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_closest_stations_test() ->
    pollution_server:start(),
    pollution_server:add_station("Stacja 1", {1.0, 1.0}),
    pollution_server:add_station("Stacja 2", {2.5, 2.1}),
    pollution_server:add_station("Stacja 3", {3.2, 3.8}),
    pollution_server:add_station("Stacja 4", {1.5, 1.7}),
    pollution_server:add_station("Stacja 5", {5.9, 5.2}),
    pollution_server:add_station("Stacja 6", {6.3, 6.7}),
    pollution_server:add_station("Stacja 7", {7.1, 7.8}),
    pollution_server:add_station("Stacja 8", {8.4, 8.0}),
    pollution_server:add_station("Stacja 9", {9.7, 9.3}),
    pollution_server:add_station("Stacja 10", {10.2, 10.5}),

    Result = pollution_server:get_closest_stations("Stacja 1"),
    Result2 = pollution_server:get_closest_stations("Stacja 11"),
    Result3 = pollution_server:get_closest_stations({1.2, 1.0}),

    pollution_server:stop(),

    ?assertMatch(
        [
            {"Stacja 4", {1.5, 1.7}, 0.8602325267042626},
            {"Stacja 2", {2.5, 2.1}, 1.8601075237738274},
            {"Stacja 3", {3.2, 3.8}, 3.560898762952971},
            {"Stacja 5", {5.9, 5.2}, 6.4536811201050215}
        ],
        Result
    ),
    ?assertMatch({error, station_not_found}, Result2),
    ?assertMatch({error, station_not_found}, Result3).
