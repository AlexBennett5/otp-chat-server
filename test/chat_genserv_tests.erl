-module(chat_genserv_tests).
-include_lib("eunit/include/eunit.hrl").

-record(state, {clients}).
-record(client_info, {username, socket, pid}).

-define(setup(TestName), {setup, fun start/0, fun teardown/1, fun TestName/1}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Descriptions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

internal_functions_test_() ->
  [{"Broadcast message broadcasts to all clients",
    ?setup(broadcast_message_works)},
   {"Retrieve from list works",
    ?setup(retrieve_from_list_works)},
   {"Process string trims and converts from binary",
    ?setup(process_string_works)}].

adding_client_test_() ->
  [{"Server initializes with an empty client list",
    ?setup(initial_empty)},
   {"Adding client to empty server adds to server's client list",
    ?setup(add_client_to_empty_server)},
   {"Adding client to server with one other client",
    ?setup(add_client_to_server_with_one)},
   {"Adding client to server with multiple clients already registered",
    ?setup(add_client_to_server_with_multiple)}].

remove_client_test_() ->
  [{"Removing client from server with multiple clients",
    ?setup(remove_client_from_server_with_clients)},
   {"Client disconnecting removes client from list",
    ?setup(remove_client_disconnect)}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Setup Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  ServerPid = spawn_link(fun() -> chat_genserv:start_link() end),
  ServerPid.

teardown(ServerPid) ->
  exit(ServerPid, kill),
  ?assertEqual(false, is_process_alive(ServerPid)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Internal Functions Tests %%%

broadcast_message_works(ServerPid) ->
  mock_gen_tcp_return_socket(),
  Clients = [#client_info{username="Test", socket=X, pid=100} || X <- lists:seq(0, 10)],
  BroadcastList = chat_genserv:broadcast(Clients, "Test"),
  unmock_gen_tcp(),
  [?_assertEqual(BroadcastList, [Y || Y <- lists:seq(0, 10)])].

retrieve_from_list_works(ServerPid) ->
  Clients = [#client_info{username="Test", socket=X, pid=X} || X <- lists:seq(0, 10)],
  SearchedClient = chat_genserv:retrieve_from_list(Clients, 5),
  [?_assertEqual(SearchedClient, #client_info{username="Test", socket=5, pid=5})].

process_string_works(ServerPid) ->
  ProcessedString = chat_genserv:process_string(<<"  Hey  ">>),
  [?_assertEqual(ProcessedString, "Hey")].

%%% Add_Client Tests %%%

initial_empty(ServerPid) ->
  CurrentState = sys:get_state(ServerPid),
  ?_assertEqual(CurrentState#state.clients, []).

add_client_to_empty_server(ServerPid) ->
  ClientPid = spawn_link(fun() -> mock_gen_tcp_return_one(),
                                  chat_genserv:add_client("Test", 100), 
                                  unmock_gen_tcp() end),
  CurrentState = sys:get_state(ServerPid),
  [?_assertEqual(length(CurrentState#state.clients), 1),
   ?_assertEqual(CurrentState#state.clients, [#client_info{username="Test", socket=100, pid=ClientPid}])].

add_client_to_server_with_one(ServerPid) ->
  ClientPidOne = spawn_link(fun() -> mock_gen_tcp_return_one(),
                                     chat_genserv:add_client("Test One", 100), 
                                     unmock_gen_tcp() end),
  ClientPidTwo = spawn_link(fun() -> mock_gen_tcp_return_one(),
                                     chat_genserv:add_client("Test Two", 101),
                                     unmock_gen_tcp() end),
  CurrentState = sys:get_state(ServerPid),
  CompareState = [#client_info{username="Test Two", socket=101, pid=ClientPidTwo}, #client_info{username="Test One", socket=100, pid=ClientPidOne}],
  [?_assertEqual(length(CurrentState#state.clients), 2),
   ?_assertEqual(CurrentState#state.clients, CompareState)].

add_client_to_server_with_multiple(ServerPid) ->
  ClientPidOne = spawn_link(fun() -> mock_gen_tcp_return_one(),
                                     chat_genserv:add_client("Test One", 100),
                                     unmock_gen_tcp() end),
  ClientPidTwo = spawn_link(fun() -> mock_gen_tcp_return_one(),
                                     chat_genserv:add_client("Test Two", 101),
                                     unmock_gen_tcp() end),
  ClientPidThree = spawn_link(fun() -> mock_gen_tcp_return_one(),
                                     chat_genserv:add_client("Test Three", 102),
                                     unmock_gen_tcp() end),
  CurrentState = sys:get_state(ServerPid),
  CompareState = [#client_info{username="Test Three", socket=102, pid=ClientPidThree}, #client_info{username="Test Two", socket=101, pid=ClientPidTwo}, #client_info{username="Test One", socket=100, pid=ClientPidOne}],
  [?_assertEqual(length(CurrentState#state.clients), 3),
   ?_assertEqual(CurrentState#state.clients, CompareState)].

%%% Remove_Client Tests %%%

remove_client_from_server_with_clients(ServerPid) ->
  ClientPidOne = spawn_link(fun() -> mock_gen_tcp_return_one(),
                                     chat_genserv:add_client("Test One", 100),
                                     unmock_gen_tcp() end),
  ClientPidTwo = spawn_link(fun() -> mock_gen_tcp_return_one(),
                                     chat_genserv:add_client("Test Two", 101),
                                     unmock_gen_tcp() end),
  ClientPidRemove = spawn_link(fun() -> mock_gen_tcp_return_one(),
                                     chat_genserv:remove_client(ClientPidOne),
                                     unmock_gen_tcp() end),
  CurrentState = sys:get_state(ServerPid),
  CompareState = [#client_info{username="Test Two", socket=101, pid=ClientPidTwo}],
  [?_assertEqual(length(CurrentState#state.clients), 1),
   ?_assertEqual(CurrentState#state.clients, CompareState)].

remove_client_disconnect(ServerPid) ->
  ClientPidOne = spawn_link(fun() -> mock_gen_tcp_return_one(),
                                     chat_genserv:add_client("Test One", 100),
                                     unmock_gen_tcp() end),
  ClientPidTwo = spawn_link(fun() -> mock_gen_tcp_return_one(),
                                     chat_genserv:add_client("Test Two", 101),
                                     unmock_gen_tcp() end),
  exit(ClientPidOne, kill),
  CurrentState = sys:get_state(ServerPid),
  CompareState = [#client_info{username="Test Two", socket=101, pid=ClientPidTwo}],
  [?_assertEqual(length(CurrentState#state.clients), 1),
   ?_assertEqual(CurrentState#state.clients, CompareState)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mock_gen_tcp_return_one() ->
  meck:new(gen_tcp, [unstick]),
  meck:expect(gen_tcp, send, fun(Socket, Message) -> 1 end),
  ?assert(meck:validate(gen_tcp)).

mock_gen_tcp_return_socket() ->
  meck:new(gen_tcp, [unstick]),
  meck:expect(gen_tcp, send, fun(Socket, Message) -> Socket end),
  ?assert(meck:validate(gen_tcp)).

unmock_gen_tcp() ->
  meck:unload(gen_tcp).

