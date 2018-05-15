%%%-------------------------------------------------------------------
%%% @author Aleksander
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. maj 2018 11:38
%%%-------------------------------------------------------------------
-module(pollutionsup).
-author("Aleksander").

%% API
-export([start/0, init/0]).




start() ->
  PID = spawn(?MODULE, init, []),
  register(supervisor, PID).
init() ->
  process_flag(trap_exit, true),
  pollutionserver:start_link(),
  receive
    {'EXIT', Pid, Reason} ->
      init()
  end.