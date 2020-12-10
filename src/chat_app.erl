-module(chat_app).
-behaviour(application).

-export([start/2, stop/1]).
-define(DEFAULT_ACCEPTORS, 10).
-define(DEFAULT_PORTNO, 1234).

start(_StartType, [Portno, MaxAcceptors]) ->
  io:format("Starting app~n"),
  chat_root_sup:start_link(Portno, MaxAcceptors);
start(_StartType, [Portno]) ->
  io:format("Starting app with default acceptors~n"),
  chat_root_sup:start_link(Portno, ?DEFAULT_ACCEPTORS);
start(_StartType, []) ->
  io:format("Starting app with default portno and acceptors~n"),
  chat_root_sup:start_link(?DEFAULT_PORTNO, ?DEFAULT_ACCEPTORS).

stop(_State) ->
  ok.
