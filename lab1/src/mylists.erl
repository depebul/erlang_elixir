%%%-------------------------------------------------------------------
%%% @author depebul
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2025 18:11
%%%-------------------------------------------------------------------
-module(mylists).
-author("depebul").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1]).

contains([], _) -> false;
contains([H|T], X) -> (H =:= X) orelse contains(T, X).

duplicateElements([]) -> [];
duplicateElements([H|T]) -> [H, H] ++ duplicateElements(T).

%%sumFloats([]) -> 0;
%%sumFloats([H|T]) when is_float(H) -> H + sumFloats(T);
%%sumFloats([_|T]) -> sumFloats(T).

sumFloats(List) -> sumFloats(List, 0).

sumFloats([], Acc) -> Acc;
sumFloats([H|T], Acc) when is_float(H) -> sumFloats(T, Acc + H);
sumFloats([_|T], Acc) -> sumFloats(T, Acc).