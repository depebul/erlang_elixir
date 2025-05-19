-module(sensor_dist).

get_euclidean_distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt((X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2)).
