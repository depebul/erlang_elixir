%%%-------------------------------------------------------------------
%%% @author depebul
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2025 17:41
%%%-------------------------------------------------------------------
-module(lab1).
-author("depebul").

%% API
-export([hello_world/0, hello_world/1]).

hello_world() ->
    io:format("Hello, World!~n").
hello_world(Name) ->
    io:format("Hello, ~s!~n", [Name]).