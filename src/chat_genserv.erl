-module(chat_genserv).
-behavior(gen_server).

-export([start_link/0, stop/0, add_client/2, remove_client/1, broadcast_message/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/3, terminate/2, code_change/3, broadcast/2, retrieve_from_list/2]).

-record(state, {clients}).
-record(client_info, {username, socket, pid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Server API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Client API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_client(Username, Socket) ->
  gen_server:call(?MODULE, {add_client, Username, Socket}).

remove_client(Pid) ->
  gen_server:cast(?MODULE, {remove_client, Pid}).

broadcast_message(Message) ->
  gen_server:cast(?MODULE, {broadcast_message, self(), Message}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
  {ok, #state{clients=[]}}.

handle_call({add_client, Username, Socket}, {Pid, _Tag}, State) ->
  link(Pid),
  NewClient = #client_info{username=Username, socket=Socket, pid=Pid},
  NewState = State#state{clients=[NewClient|State#state.clients]},
  MessageString = "[" ++ process_string(Username) ++ " has entered the chat]\n",
  broadcast(State#state.clients, MessageString),
  io:format(MessageString),
  {reply, ok, NewState};

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};

handle_call(_Other, _From, State) ->
  io:format("Server received unexpected call: ~p~n", [_Other]),
  {reply, "???", State}.

handle_cast({remove_client, Pid}, State) ->
  ClientBeingRemoved = retrieve_from_list(State#state.clients, Pid),
  NewClients = [Client || Client <- State#state.clients, Client#client_info.pid /= Pid],
  NewState = State#state{clients=NewClients},
  MessageString = "[" ++ process_string(ClientBeingRemoved#client_info.username) ++ " has left the chat]\n",
  broadcast(NewClients, MessageString),
  io:format(MessageString),
  {noreply, NewState};

handle_cast({broadcast_message, Pid, Message}, State) ->
  ClientSendingMessage = retrieve_from_list(State#state.clients, Pid),
  ClientUsername = ClientSendingMessage#client_info.username,
  MessageString = "<" ++ process_string(ClientUsername) ++ "> " ++ process_string(Message) ++ "\n",
  broadcast(State#state.clients, MessageString),
  io:format(MessageString),
  {noreply, State}.

handle_info(_E, _From, _State) -> {noreply, _State}.
terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

broadcast(Clients, Message) ->
  [gen_tcp:send(Client#client_info.socket, Message) || Client <- Clients].

retrieve_from_list(Clients, ClientPid) ->
  hd([Client || Client <- Clients, Client#client_info.pid == ClientPid]).

process_string(Input) ->
  if
    is_binary(Input) ->
      string:trim(binary_to_list(Input));
    true ->
      string:trim(Input)
  end.
