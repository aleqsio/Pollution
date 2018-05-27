%%%-------------------------------------------------------------------
%%% @author Aleksander
%%% @copyright (C) 2018
%%% @doc
%%%
%%% @end
%%% Created : 23. kwi 2018 19:47
%%%-------------------------------------------------------------------

%%http://blog.rusty.io/2011/01/13/beautiful-erlang-print/
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-module(pollution_tests).
-include_lib("eunit/include/eunit.hrl").
-author("Aleksander").

-record(monitor, {namesToLocations = maps:new(), locationsToNames = maps:new(), stations = maps:new()}).
-record(station, {name, location, measurements = maps:new()}).

addStation_NewStation_AddToMonitor_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  ?assertEqual([#station{name = "Station1", location = {0, 0}, measurements = maps:new()}], maps:values(M1#monitor.stations)),
  {ok, _} = pollution:addStation("Station2", {0, 1}, M1).

addStation_DuplicateStation_FailToAdd_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {error, "A station with that location exists"} = pollution:addStation("Station2", {0, 0}, M1),
  {error, "A station with that name exists"} = pollution:addStation("Station1", {0, 1}, M1),
  {error, "This station exists"} = pollution:addStation("Station1", {0, 0}, M1).

addValue_ExistingStationUniqueValues_AddToMonitor_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {ok, M2} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", 100.0, M1),
  {ok, M3} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 1}}, "T1", 100.0, M2),
  {ok, M4} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T2", 100.0, M3),
  3 = maps:size((maps:get("Station1", M4#monitor.stations))#station.measurements).

addValue_NonExistentStation_FailToAdd_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {error, "Such station does not exist"} = pollution:addValue("Station2", {{0, 0, 0}, {0, 0, 0}}, "T1", 100.0, M1).

addValue_DuplicateMeasurement_FailToAdd_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {ok, M2} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", 100.0, M1),
  {error, "This mesurement exists"} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", 50.0, M2).

removeValue_ExistingMeasurement_AddToMonitor_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {ok, M2} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", 100.0, M1),
  {ok, _} = pollution:removeValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", M2).

removeValue_NonExistingMeasurement_NoChange_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {ok, M2} = pollution:removeValue("Station1", {{0, 0, 0}, {0, 0, 1}}, "T1", M1),
  M2 = M1.

removeValue_NonExistentStation_FailToAdd_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {error, "Such station does not exist"} = pollution:removeValue("Station2", {{0, 0, 0}, {0, 0, 0}}, "T1", M1).

getStationMean_MultipleMeasurements_AddToMonitor_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {ok, M2} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 1}}, "T1", 50.0, M1),
  {ok, M3} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 2}}, "T1", 100.0, M2),
  {ok, 75.0} = pollution:getStationMean({0, 0}, "T1", M3).

getStationMean_OneMeasurement_AddToMonitor_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {ok, M2} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 1}}, "T1", 50.0, M1),
  {ok, 50.0} = pollution:getStationMean({0, 0}, "T1", M2).

getStationMean_NonExistentStation_FailToAdd_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {ok, M2} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 1}}, "T1", 50.0, M1),
  {error, "Such station does not exist"} = pollution:getStationMean({0, 1}, "T1", M2).

getStationMean_NoMeasurements_FailToAdd_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {error, "No mesurements"} = pollution:getStationMean({0, 0}, "T1", M1).


getDailyMean_MultipleMeasurements_AddToMonitor_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {ok, M2} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", 50.0, M1),
  {ok, M3} = pollution:addStation("Station2", {0, 1}, M2),
  {ok, M4} = pollution:addValue("Station2", {{0, 0, 0}, {0, 0, 0}}, "T1", 100.0, M3),
  {ok, 75.0} = pollution:getDailyMean({{0, 0, 0}, {0, 0, 0}}, "T1", M4).

getDailyMean_NoMeasurements_FailToAdd_test() ->
  M = pollution:createMonitor(),
  {error, "No measurements fitting criteria"} = pollution:getDailyMean({0, 0, 0}, "T1", M).

getMaximumVariationStation_MultipleMeasurements_AddToMonitor_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {ok, M2} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", 5.0, M1),
  {ok, M3} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 1}}, "T1", 10.0, M2),
  {ok, M4} = pollution:addStation("Station2", {0, 1}, M3),
  {ok, M5} = pollution:addValue("Station2", {{0, 0, 0}, {0, 0, 0}}, "T1", 5.0, M4),
  {ok, M6} = pollution:addValue("Station2", {{0, 0, 0}, {0, 0, 1}}, "T1", 100.0, M5),
  {ok, {Station, Variation}} = pollution:getMaximumVariationStation("T1", M6),
  "Station2" = Station#station.name,
  95.0 = Variation.

getMaximumVariationStation_OneMeasurement_ZeroVariation_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {ok, M2} = pollution:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", 5.0, M1),
  {ok, {Station, Variation}} = pollution:getMaximumVariationStation("T1", M2),
  "Station1" = Station#station.name,
  0.0 = Variation.

getMaximumVariationStation_NoMeasurements_FailToAdd_test() ->
  M = pollution:createMonitor(),
  {ok, M1} = pollution:addStation("Station1", {0, 0}, M),
  {error, "No station with measurements matching criteria"} = pollution:getMaximumVariationStation("T1", M1).