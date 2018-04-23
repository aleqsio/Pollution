%%%-------------------------------------------------------------------
%%% @author Aleksander
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%% Created : 22. kwi 2018 20:28
%%%-------------------------------------------------------------------
-module(pollution).
-author("Aleksander").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getMaximumVariationStation/2]).

%%createMonitor/0 - tworzy i zwraca nowy monitor zanieczyszczeń;
%%addStation/3 - dodaje do monitora wpis o nowej stacji pomiarowej (nazwa i współrzędne geograficzne), zwraca zaktualizowany monitor;
%%addValue/5 - dodaje odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru, wartość), zwraca zaktualizowany monitor;
%%removeValue/4 - usuwa odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru), zwraca zaktualizowany monitor;
%%getOneValue/4 - zwraca wartość pomiaru o zadanym typie, z zadanej daty i stacji;
%%getStationMean/3 - zwraca średnią wartość parametru danego typu z zadanej stacji;
%%getDailyMean/3 -

-record(monitor, {namesToLocations = maps:new(), locationsToNames = maps:new(), stations = maps:new()}).
-record(station, {name, location, measurements = maps:new()}).

createMonitor() -> #monitor{}.

addStation(Name, Location, Monitor) ->
  case {maps:is_key(Name, Monitor#monitor.namesToLocations), maps:is_key(Location, Monitor#monitor.locationsToNames)} of
    {false, false} -> {ok, Monitor#monitor{
      namesToLocations = maps:put(Name, Location, Monitor#monitor.namesToLocations),
      locationsToNames = maps:put(Location, Name, Monitor#monitor.locationsToNames),
      stations = maps:put(Name, #station{name = Name, location = Location}, Monitor#monitor.stations)}};
    {true, false} -> {error, "A station with that name exists"};
    {false, true} -> {error, "A station with that location exists"};
    {true, true} -> {error, "This station exists"}
  end.

nameFromLocation(Location, Monitor) ->
  case maps:is_key(Location, Monitor#monitor.locationsToNames) of
    false ->
      {error, "Such station does not exist"};
    true ->
      {ok, maps:get(Location, Monitor#monitor.locationsToNames)} end.

addValue({_, _} = Location, DateTime, Type, Value, Monitor) ->
  case nameFromLocation(Location, Monitor) of {ok, Name} -> addValue(Name, DateTime, Type, Value, Monitor);
    {error, _} = Error -> Error end;
addValue(Name, Date, Type, Value, Monitor) -> case maps:is_key(Name, Monitor#monitor.stations) of
                                                false -> {error, "Such station does not exist"};
                                                true -> addValue(exists, Name, Date, Type, Value, Monitor) end.
addValue(exists, Name, DateTime, Type, Value, Monitor) ->
  MeasurementExists = maps:is_key({DateTime, Type}, (maps:get(Name, Monitor#monitor.stations))#station.measurements),
  case MeasurementExists of
    true -> {error, "This mesurement exists"};
    false -> {ok, Monitor#monitor{stations = maps:update_with(Name, fun(Station) ->
      Station#station{measurements = maps:put({DateTime, Type}, Value, Station#station.measurements)} end, Monitor#monitor.stations)}} end.

%%
%%maps:is_key(Name,Monitor#monitor.stations)
%%{ok, #monitor{
%%namesToLocations = maps:put(Name, Location, Monitor#monitor.namesToLocations),
%%locationsToNames = maps:put(Location, Name, Monitor#monitor.locationsToNames),
%%stations = maps:put(Name, #station{name = Name, location = Location, measurements = maps:new()}, Monitor#monitor.stations)}};

removeValue({_, _} = Location, DateTime, Type, Monitor) ->
  case nameFromLocation(Location, Monitor) of {ok, Name} -> removeValue(Name, DateTime, Type, Monitor);
    {error, _} = Error -> Error end;
removeValue(Name, DateTime, Type, Monitor) -> case maps:is_key(Name, Monitor#monitor.stations) of
                                                false -> {error, "Such station does not exist"};
                                                true -> removeValue(exists, Name, DateTime, Type, Monitor) end.
removeValue(exists, Name, DateTime, Type, Monitor) ->
  {ok, Monitor#monitor{stations = maps:update_with(Name,
    fun(Station) -> Station#station{measurements = maps:remove({DateTime, Type}, Station#station.measurements)}
    end, Monitor#monitor.stations)}}.


getOneValue({_, _} = Location, DateTime, Type, Monitor) ->
  case nameFromLocation(Location, Monitor) of {ok, Name} -> getOneValue(Name, DateTime, Type, Monitor);
    {error, _} = Error -> Error end;
getOneValue(exists, DateTime, Type, Station) ->
  {ok, maps:get({DateTime, Type}, Station#station.measurements)};
getOneValue(Name, DateTime, Type, Monitor) -> case maps:is_key(Name, Monitor#monitor.stations) of
                                                false -> {error, "Such station does not exist"};
                                                true ->
                                                  getOneValue(exists, DateTime, Type, maps:get(Name, Monitor#monitor.stations)) end.


getStationMean({_, _} = Location, Type, Monitor) ->
  case nameFromLocation(Location, Monitor) of {ok, Name} -> getStationMean(Name, Type, Monitor);
    {error, _} = Error -> Error end;
getStationMean(exists, Type, Station) ->
  List = maps:values(maps:filter(fun({_, CType}, _) -> Type =:= CType end, Station#station.measurements)),
  case List of
    [] -> {error, "No mesurements"};
    _ -> {ok,lists:sum(List) / length(List)} end;
getStationMean(Name, Type, Monitor) -> case maps:is_key(Name, Monitor#monitor.stations) of
                                         false -> {error, "Such station does not exist"};
                                         true ->
                                           getStationMean(exists, Type, maps:get(Name, Monitor#monitor.stations)) end.


getDailyMean(DateTime, Type, Monitor) -> AllMeasurements = maps:fold(fun(_,Station,Acc) ->
  maps:to_list(Station#station.measurements)++Acc end, [], Monitor#monitor.stations),
  MeasurementsFitting = lists:map(fun({_,V})-> V end, lists:filter(fun({{KeyDateTime, KeyType}, _}) when DateTime =:= KeyDateTime; Type =:= KeyType ->
    true;(_) -> false end, AllMeasurements)),
  case MeasurementsFitting of
    [] -> {error, "No measurements fitting criteria"};
    _ -> {ok,lists:sum(MeasurementsFitting) / length(MeasurementsFitting)} end.


getMaximumVariationStation(Type, Monitor) -> Stations = maps:values(Monitor#monitor.stations),
  case Stations of
    [] -> {error, "No stations"};
    List -> getMaximumVariationStation(exists, Type, List) end.
getMaximumVariationStation(exists, Type, List) ->
  Variations = lists:map(fun(Station) -> getVariation(Type, Station) end, List),
  FilteredVariations = lists:sort(fun({_, Variation}, {_, Variation2}) ->
    Variation > Variation2 end, lists:filter(fun({ok, _}) -> true;(_) -> false end, Variations)),
  case FilteredVariations of
    [] -> {error, "No station with measurements matching criteria"};
    [{ok,TopVariation} | _] -> {ok, TopVariation}
  end.

getVariation(Type, Station) ->
  Measurements = lists:map(fun({{_, _}, Value}) -> Value end, lists:filter(fun({{_, KeyType}, _}) ->
    KeyType =:= Type end, maps:to_list(Station#station.measurements))),
  case Measurements of
    [] -> {error, "No measurements fitting criteria"};
    _ -> {ok, {Station, lists:max(Measurements) - lists:min(Measurements)}}
  end.

