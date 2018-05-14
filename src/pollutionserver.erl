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

%% API
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getMaximumVariationStation/1,getMonitor/0]).


start() ->
  Monitor = pollution:createMonitor(),
  PID=spawn(fun() -> loop(Monitor) end),
  register(server, PID).


stop() ->
  server ! stop,
unregister(server).

loop(Monitor) ->
  receive
    {Pid,getMonitor} -> Pid!{ok,Monitor},loop(Monitor);
    {Pid,stop} -> Pid ! ok;
    {Pid,addStation,Name,Location} ->
      Result = pollution:addStation(Name,Location,Monitor),
      Pid ! getSuccessState(Result),
      case Result of
        {ok, UpdatedMonitor} -> loop(UpdatedMonitor);
        _ -> loop(Monitor)
      end;

    {Pid,addValue,LocationOrName,DateTime,Type,Value}->
      Result = pollution:addValue(LocationOrName,DateTime,Type,Value,Monitor),
      Pid ! getSuccessState(Result),
      case Result of
        {ok, UpdatedMonitor} -> loop(UpdatedMonitor);
        _ -> loop(Monitor)
      end;
    {Pid,removeValue,LocationOrName,DateTime,Type}->
      Result=pollution:removeValue(LocationOrName,DateTime,Type,Monitor),
      Pid ! getSuccessState(Result),
      case Result of
        {ok, UpdatedMonitor} -> loop(UpdatedMonitor);
        _ -> loop(Monitor)
      end;
    {Pid,getOneValue,LocationOrName,DateTime,Type}->
      Result=pollution:getOneValue(LocationOrName,DateTime,Type,Monitor),
      Pid !Result,
      loop(Monitor);
    {Pid,getStationMean,LocationOrName,Type} ->
      Result=pollution:getStationMean(LocationOrName,Type,Monitor),
      Pid !Result,
      loop(Monitor);
    {Pid,getDailyMean,LocationOrName,Type} ->
      Result=pollution:getDailyMean(LocationOrName,Type,Monitor),
      Pid !Result,
      loop(Monitor);
    {Pid,getMaximumVariationStation,Type} ->
      Result=pollution:getMaximumVariationStation(Type,Monitor),
      Pid !Result,
      loop(Monitor)

  end.

getSuccessState({ok,_})-> ok;
getSuccessState({error,Response})-> {error,Response};
getSuccessState(_) -> {error,"Server error"}.

getMonitor() -> server ! {self(),getMonitor},receiveResponse().
addStation(Name, Location) -> server ! {self(),addStation,Name,Location},receiveResponse().
addValue(LocationOrName,DateTime,Type,Value) -> server ! {self(),addValue,LocationOrName,DateTime,Type,Value},receiveResponse().
removeValue(LocationOrName,DateTime,Type) ->  server ! {self(),removeValue,LocationOrName,DateTime,Type},receiveResponse().
getOneValue(LocationOrName,DateTime,Type) ->  server ! {self(),getOneValue,LocationOrName,DateTime,Type},receiveResponse().
getStationMean(LocationOrName,Type) -> server ! {self(),getStationMean,LocationOrName,Type},receiveResponse().
getDailyMean(LocationOrName,Type)-> server ! {self(),getDailyMean,LocationOrName,Type},receiveResponse().
getMaximumVariationStation(Type)-> server ! {self(),getMaximumVariationStation,Type},receiveResponse().

receiveResponse() ->
  receive
    ok -> ok;
    {ok,Result} -> {ok,Result};
    {error,Response} -> {error,Response};
    _ -> {error,malformed_data}
  after
  1000-> {error,no_data}
  end.