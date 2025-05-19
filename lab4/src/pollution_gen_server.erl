%%%-------------------------------------------------------------------
%%% @author depebul
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE). 

-record(pollution_gen_server_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #pollution_gen_server_state{}}.

handle_call(_Request, _From, State = #pollution_gen_server_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #pollution_gen_server_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #pollution_gen_server_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #pollution_gen_server_state{}) ->
    ok.

code_change(_OldVsn, State = #pollution_gen_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
