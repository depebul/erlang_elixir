%%%-------------------------------------------------------------------
%%% @author depebul
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2025 18:03
%%%-------------------------------------------------------------------
-module(power).
-author("depebul").

%% API
-export([power/2]).

power(X,1) -> X;
power(X,Y) -> X * power(X,Y-1).
