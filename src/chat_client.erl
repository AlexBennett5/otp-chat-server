-module(chat_client).
-export([client_spawn/1]).

client_spawn(ListenerSocket) ->
  spawn_link(fun() -> client_accept(ListenerSocket) end),
  {ok, self()}.

client_accept(ListenerSocket) ->
  {ok, ClientSocket} = gen_tcp:accept(ListenerSocket),
  spawn(fun() -> client_setup(ClientSocket) end),
  client_accept(ListenerSocket).

client_setup(ClientSocket) ->
  gen_tcp:send(ClientSocket, "Enter your username: "),
  case gen_tcp:recv(ClientSocket, 0) of
    {ok, Username} ->
      chat_genserv:add_client(Username, ClientSocket),
      client_loop(ClientSocket);
    {error, closed} ->
      ok
  end.

client_loop(ClientSocket) ->
  {ok, Msg} = gen_tcp:recv(ClientSocket, 0),
  Message = process_string(Msg),
  case Message of
    "{quit}" -> chat_genserv:remove_client(self());
    _ -> chat_genserv:broadcast_message(Msg),
         client_loop(ClientSocket)
  end.

process_string(Binary) ->
  string:trim(binary_to_list(Binary)).
