-module(chat_accept_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start_link(Portno, MaxAcceptors) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Portno, MaxAcceptors]).

init([Portno, MaxAcceptors]) ->
  {ok, ListenerSocket} = gen_tcp:listen(Portno, ?TCP_OPTIONS),
  SpawnWorker = fun () -> {make_ref(), {chat_client, client_spawn, [ListenerSocket]}, transient, brutal_kill, worker, [chat_client]}
                end,
  Workers = [SpawnWorker() || _ <- lists:seq(0, MaxAcceptors)],
  {ok, {{one_for_one, 50, 50}, Workers}}.
