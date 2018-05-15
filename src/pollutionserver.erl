%%%-------------------------------------------------------------------
%%% @author Aleksander
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. maj 2018 15:35
%%%-------------------------------------------------------------------
%%http://blog.rusty.io/2011/01/13/beautiful-erlang-print/
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-module(pollutionserver).
-author("Aleksander").
-behavior(gen_server).
%% API
-export([stop/0, start_link/1, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getMaximumVariationStation/1, getMonitor/0, crash/0, init/1, handle_call/3, handle_cast/2]).


start_link(_) ->
  gen_server:start_link(
    {local,server},
    pollutionserver,
    [], []).

stop() ->
  gen_server:stop(pollutionserver1).

getNewState(Monitor,Request) ->
  case Request of
    getMonitor ->{{ok, Monitor},Monitor};
    {stop} -> Reply = ok;
    {addStation, Name, Location} ->
      Result = pollution:addStation(Name, Location, Monitor),
      case Result of
        {ok, UpdatedMonitor} -> {ok,UpdatedMonitor};
        error -> {error,Monitor}
      end;

    {addValue, LocationOrName, DateTime, Type, Value} ->
      Result = pollution:addValue(LocationOrName, DateTime, Type, Value, Monitor),
      case Result of
        {ok, UpdatedMonitor} -> {ok,UpdatedMonitor};
        error -> {error,Monitor}
      end;
    {removeValue, LocationOrName, DateTime, Type} ->
      Result = pollution:removeValue(LocationOrName, DateTime, Type, Monitor),
      case Result of
        {ok, UpdatedMonitor} -> {ok,UpdatedMonitor};
        error -> {error,Monitor}
      end;
    {getOneValue, LocationOrName, DateTime, Type} ->
      Result = pollution:getOneValue(LocationOrName, DateTime, Type, Monitor),
      {Result,Monitor};
    {getStationMean, LocationOrName, Type} ->
      Result = pollution:getStationMean(LocationOrName, Type, Monitor),
      {Result,Monitor};
    {getDailyMean, LocationOrName, Type} ->
      Result = pollution:getDailyMean(LocationOrName, Type, Monitor),
      {Result,Monitor};
    {getMaximumVariationStation, Type} ->
      Result = pollution:getMaximumVariationStation(Type, Monitor),
      {Result,Monitor};
    {crash} -> {error,Monitor}
  end.

getSuccessState({ok, _}) -> ok;
getSuccessState({error, Response}) -> {error, Response};
getSuccessState(_) -> {error, "Server error"}.

getMonitor() -> gen_server:call(server,getMonitor).
addStation(Name, Location) -> server ! {self(), addStation, Name, Location}.
addValue(LocationOrName, DateTime, Type, Value) -> server ! {self(), addValue, LocationOrName, DateTime, Type, Value}.
removeValue(LocationOrName, DateTime, Type) -> server ! {self(), removeValue, LocationOrName, DateTime, Type}.
getOneValue(LocationOrName, DateTime, Type) -> server ! {self(), getOneValue, LocationOrName, DateTime, Type}.
getStationMean(LocationOrName, Type) -> server ! {self(), getStationMean, LocationOrName, Type}.
getDailyMean(LocationOrName, Type) -> server ! {self(), getDailyMean, LocationOrName, Type}.
getMaximumVariationStation(Type) -> server ! {self(), getMaximumVariationStation, Type}.
crash() -> server ! {self(), crash}.

init(_) ->
  Monitor = pollution:createMonitor(),
  {ok,Monitor}.


handle_call(Request, From, State) ->
  {Reply,NewState}=getNewState(State,Request),
  {reply,Reply,NewState}.

handle_cast(Request, State) ->
  erlang:error(not_implemented).