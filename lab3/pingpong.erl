%%% @author Dawid Żak
%%% @copyright (C) 2025, Dawid Żak
%%% @doc
%%%
%%% @end
%%% Created : 07 Apr 2025 by Dawid Żak ś
-module(pingpong).

-export([start/0, play/1, stop/0]).

start() ->
    register(
        ping,
        spawn(fun Loop() ->
            receive
                N when N > 0 ->
                    timer:sleep(100),
                    io:format("ping ~p \n", [N]),
                    pong ! N - 1,
                    Loop();
                _ ->
                    ok
            after 20000 -> ok
            end
        end)
    ),
    register(
        pong,
        spawn(fun Loop() ->
            receive
                N when N > 0 ->
                    timer:sleep(100),
                    io:format("pong ~p \n", [N]),
                    ping ! N - 1,
                    Loop();
                _ ->
                    ok
            after 20000 -> ok
            end
        end)
    ).

play(N) ->
    ping ! N.

stop() ->
    ping ! -1,
    pong ! -1.
