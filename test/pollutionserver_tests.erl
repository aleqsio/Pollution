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

-module(pollutionserver_tests).
-include_lib("eunit/include/eunit.hrl").
-author("Aleksander").

-record(monitor, {namesToLocations = maps:new(), locationsToNames = maps:new(), stations = maps:new()}).
-record(station, {name, location, measurements = maps:new()}).

addStation_NewStation_AddToMonitor_test() ->
  pollutionserver:start(),
  pollutionserver:addStation("Station1", {0, 0}),
  {ok, M1} = (pollutionserver:getMonitor()),
  ?assertEqual([#station{name = "Station1", location = {0, 0}, measurements = maps:new()}], maps:values(M1#monitor.stations)),
  ok = pollutionserver:addStation("Station2", {0, 1}),
  pollutionserver:stop().
addStation_DuplicateStation_FailToAdd_test() ->
  pollutionserver:start(),
  pollutionserver:addStation("Station1", {0, 0}),
  {error, "A station with that location exists"} = pollutionserver:addStation("Station2", {0, 0}),
  {error, "A station with that name exists"} = pollutionserver:addStation("Station1", {0, 1}),
  pollutionserver:stop().

addValue_ExistingStationUniqueValues_AddToMonitor_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  ok = pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", 100.0),
  ok = pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 1}}, "T1", 100.0),
  ok = pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T2", 100.0),
  {ok, M} = pollutionserver:getMonitor(),
  3 = maps:size((maps:get("Station1", M#monitor.stations))#station.measurements),
  pollutionserver:stop().

addValue_NonExistentStation_FailToAdd_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  {error, "Such station does not exist"} = pollutionserver:addValue("Station2", {{0, 0, 0}, {0, 0, 0}}, "T1", 100.0),
  pollutionserver:stop().

addValue_DuplicateMeasurement_FailToAdd_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  ok = pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", 100.0),
  {error, "This mesurement exists"} = pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", 50.0),
  pollutionserver:stop().

removeValue_ExistingMeasurement_AddToMonitor_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  ok=pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1",100.0),
  ok = pollutionserver:removeValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1"),
  pollutionserver:stop().

removeValue_NonExistingMeasurement_NoChange_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  M = pollutionserver:getMonitor(),
  ok = pollutionserver:removeValue("Station1", {{0, 0, 0}, {0, 0, 1}}, "T1"),
  M = pollutionserver:getMonitor(),
  pollutionserver:stop().

removeValue_NonExistentStation_FailToAdd_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  {error, "Such station does not exist"} = pollutionserver:removeValue("Station2", {{0, 0, 0}, {0, 0, 0}}, "T1"),
  pollutionserver:stop().
getStationMean_MultipleMeasurements_AddToMonitor_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  ok = pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 1}}, "T1", 50.0),
  ok = pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 2}}, "T1", 100.0),
  {ok, 75.0} = pollutionserver:getStationMean({0, 0}, "T1"),
  pollutionserver:stop().

getStationMean_OneMeasurement_AddToMonitor_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  ok = pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 1}}, "T1", 50.0),
  {ok, 50.0} = pollutionserver:getStationMean({0, 0}, "T1"),
  pollutionserver:stop().

getStationMean_NonExistentStation_FailToAdd_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  ok = pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 1}}, "T1", 50.0),
  {error, "Such station does not exist"} = pollutionserver:getStationMean({0, 1}, "T1"),
  pollutionserver:stop().
getStationMean_NoMeasurements_FailToAdd_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  {error, "No mesurements"} = pollutionserver:getStationMean({0, 0}, "T1"),
  pollutionserver:stop().

getDailyMean_MultipleMeasurements_AddToMonitor_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  ok = pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", 50.0),
  ok = pollutionserver:addStation("Station2", {0, 1}),
  ok = pollutionserver:addValue("Station2", {{0, 0, 0}, {0, 0, 0}}, "T1", 100.0),
  {ok,75.0} = pollutionserver:getDailyMean({{0, 0, 0}, {0, 0, 0}}, "T1"),
  pollutionserver:stop().

getDailyMean_NoMeasurements_FailToAdd_test() ->
  pollutionserver:start(),
  {error, "No measurements fitting criteria"} = pollutionserver:getDailyMean({0, 0, 0}, "T1"),
  pollutionserver:stop().

getMaximumVariationStation_MultipleMeasurements_AddToMonitor_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  ok = pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", 5.0),
  ok = pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 1}}, "T1", 10.0),
  ok = pollutionserver:addStation("Station2", {0, 1}),
  ok = pollutionserver:addValue("Station2", {{0, 0, 0}, {0, 0, 0}}, "T1", 5.0),
  ok = pollutionserver:addValue("Station2", {{0, 0, 0}, {0, 0, 1}}, "T1", 100.0),
  {ok, {Station, Variation}} = pollutionserver:getMaximumVariationStation("T1"),
  "Station2" = Station#station.name,
  95.0 = Variation,
  pollutionserver:stop().

getMaximumVariationStation_OneMeasurement_ZeroVariation_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  ok = pollutionserver:addValue("Station1", {{0, 0, 0}, {0, 0, 0}}, "T1", 5.0),
  {ok, {Station, Variation}} = pollutionserver:getMaximumVariationStation("T1"),
  "Station1" = Station#station.name,
  0.0 = Variation,
  pollutionserver:stop().


getMaximumVariationStation_NoMeasurements_FailToAdd_test() ->
  pollutionserver:start(),
  ok = pollutionserver:addStation("Station1", {0, 0}),
  {error, "No station with measurements matching criteria"} = pollutionserver:getMaximumVariationStation("T1"),
  pollutionserver:stop().