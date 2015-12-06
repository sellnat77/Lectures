%%%-------------------------------------------------------------------
%%% @author Russell
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Dec 2015 6:45 PM
%%%-------------------------------------------------------------------
-module(lab5).
-author("Russell").

%% API
-export([test/0]).

test() ->
  A = 42,
  io:format("A = ~B",[A]).
