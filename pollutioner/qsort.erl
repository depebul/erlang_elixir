-module(qsort).

-export([main/1]).

less_than(List, X) -> [Y || Y <- List, Y < X].

grt_eq_than(List, X) -> [Y || Y <- List, Y >= X].

random_elems(N, Min, Max) -> [rand:uniform(Max - Min) + Min || _ <- lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) ->
    {Time1, _} = timer:tc(Fun1, [List]),
    {Time2, _} = timer:tc(Fun2, [List]),
    io:format("czas 1 - ~w~n", [Time1]),
    io:format("czas 2 - ~w~n", [Time2]).

qs([]) -> [];
qs([Pivot | Tail]) -> qs(less_than(Tail, Pivot)) ++ [Pivot] ++ qs(grt_eq_than(Tail, Pivot)).

main(_Args) ->
    Rand_list = random_elems(10000, 0, 1),
    % io:format("List: ~w~n", [Rand_list]),
    compare_speeds(Rand_list, fun qs/1, fun lists:sort/1).
