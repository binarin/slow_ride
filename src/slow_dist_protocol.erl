-module(slow_dist_protocol).
-behaviour(ranch_protocol).

-export([start_listener/3]).

-export([start_link/4, init/4]).

-record(state, { transport
               , transport_ok
               , transport_error
               , transport_closed
               , socket
               , target_socket
               , node
               , from
               , parent
               , callback_module
               , callback_args
               }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_listener(EpmdConn :: pid(), NodeName :: binary(), RealPort :: 1..65535) -> Port when
      Port :: 1..65535.

start_listener(EpmdConn, NodeName, RealPort) ->
    Listener = listener_name(NodeName),
    ranch:start_listener(Listener, 2, ranch_tcp,
                         [{port, 0}], slow_dist_protocol, [EpmdConn, RealPort]),
    ranch:get_port(Listener).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ranch_protocol callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Ref, Socket, Tranport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Tranport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, [EpmdConnPid, RealPort]) ->
    ok = ranch:accept_ack(Ref),
    State = init_transport(Transport, Socket),
    State1 = State#state{parent = EpmdConnPid},
    State2 = open_target(RealPort, State1),
    State3 = init_callback(State2),
    loop(State3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listener_name(NodeName) ->
    {slow_dist_protocol, NodeName}.

init_callback(State) ->
    {Mod, Args} = slow_ride:callback_module(),
    State#state{callback_module = Mod, callback_args = Args}.

init_transport(Transport, Socket) ->
    {OK, Closed, Error} = Transport:messages(),
    Transport:setopts(Socket, [binary, {packet, 2}, {active, once}]),
    #state{transport_ok = OK
          ,transport_closed = Closed
          ,transport_error = Error
          ,transport = Transport
          ,socket = Socket
          }.

%% active_once(#state{socket = S, transport = T}) ->
%%     T:setopts(S, [{active, once}]).

loop(#state{transport_ok = OK, transport_closed = Closed, transport_error = Error,
            transport = Transport, socket = Socket, target_socket = TargetSocket} = State) ->
    receive
        {OK, TargetSocket, <<$a, _/binary>> = Data} ->
            handle_challenge_ack(Data, State);
        {OK, Socket, <<$n, _/binary>> = Data} ->
            handle_send_name(Data, State);
        {OK, TargetSocket, <<$n, _/binary>> = Data} ->
            handle_send_challenge(Data, State);
        {OK, Socket, Data} ->
            handle_handshake_packet_from_source(Data, State);
        {OK, TargetSocket, Data} ->
            handle_handshake_packet_from_target(Data, State);
        %% Errors
        {Error, Socket, _Reason} ->
            lager:debug("Handshake - err on incoming socket ~p", [_Reason]),
            gen_tcp:close(TargetSocket);
        {Closed, Socket} ->
            lager:debug("Handshake - close on incoming socket", []),
            gen_tcp:close(TargetSocket);
        {Error, TargetSocket, _Reason} ->
            lager:debug("Handshake - err on outgoing socket ~p", [_Reason]),
            Transport:close(Socket);
        {Closed, TargetSocket} ->
            lager:debug("Handhake - close on outgoing socket", []),
            Transport:close(Socket)
    end.

open_target(RealPort, State) ->
    {ok, Sock} = gen_tcp:connect({127, 0, 0, 1}, RealPort, [binary, {packet, 2}, {active, once}]),
    State#state{target_socket = Sock}.

loop_dist(#state{transport_ok = OK, transport_closed = Closed, transport_error = Error,
                 transport = Transport, socket = Socket, target_socket = TargetSocket} = State) ->
    receive
        {OK, Socket, Data} ->
            ok = gen_tcp:send(TargetSocket, Data),
            {ok, Args} = invoke_forward_packet_callback(Data, State),
            Transport:setopts(Socket, [{active, once}]),
            loop_dist(State#state{callback_args = Args});
        {OK, TargetSocket, Data} ->
            ok = Transport:send(Socket, Data),
            inet:setopts(TargetSocket, [{active, once}]),
            {ok, Args} = invoke_reverse_packet_callback(Data, State),
            loop_dist(State#state{callback_args = Args});
        {Error, Socket, _Reason} ->
            lager:debug("Dist - err on incoming socket ~p", [_Reason]),
            gen_tcp:close(TargetSocket);
        {Closed, Socket} ->
            lager:debug("Dist - close on incoming socket", []),
            gen_tcp:close(TargetSocket);
        {Error, TargetSocket, _Reason} ->
            lager:debug("Dist - err on outgoing socket ~p", [_Reason]),
            Transport:close(Socket);
        {Closed, TargetSocket} ->
            lager:debug("Dist - close on outgoing socket", []),
            Transport:close(Socket)
    end.

handle_challenge_ack(Data, #state{target_socket = TargetSocket, transport = Transport, socket = Socket} = State) ->
    lager:debug("Handshake - target ack"),
    inet:setopts(TargetSocket, [{packet, 4}, {active, once}]),
    %% Preventing race during packet size switch - so we don't
    %% accidentially get first packet of dist traffic.
    Transport:setopts(Socket, [{active, false}]),
    ok = Transport:send(Socket, Data),
    Transport:setopts(Socket, [{packet, 4}, {active, once}]),
    {ok, Args} = invoke_connection_established_callback(State),
    loop_dist(State#state{callback_args = Args}).

handle_send_name(<<$n, _Version:16, _Flag:32, From/binary>> = Data, State) ->
    lager:debug("Handshake - to target ~p (from ~s)", [Data, From]),
    send_target(Data, State),
    source_active_once(State),
    loop(State#state{from = binary_to_atom(From, utf8)}).

send_target(Data, #state{target_socket = TargetSocket}) ->
    ok = gen_tcp:send(TargetSocket, Data).

send_source(Data, #state{socket = Socket, transport = Transport}) ->
    ok = Transport:send(Socket, Data).

source_active_once(#state{transport = Transport, socket = Socket}) ->
    Transport:setopts(Socket, [{active, once}]).

target_active_once(#state{target_socket = TargetSocket}) ->
    inet:setopts(TargetSocket, [{active, once}]).

handle_handshake_packet_from_source(Data, #state{target_socket = TargetSocket, transport = Transport, socket = Socket} = State) ->
    lager:debug("Handshake - to target ~p", [Data]),
    ok = gen_tcp:send(TargetSocket, Data),
    Transport:setopts(Socket, [{active, once}]),
    loop(State).

handle_handshake_packet_from_target(Data, #state{transport = Transport, socket = Socket, target_socket = TargetSocket} = State) ->
    lager:debug("Handshake - from target ~p", [Data]),
    ok = Transport:send(Socket, Data),
    inet:setopts(TargetSocket, [{active, once}]),
    loop(State).

invoke_connection_established_callback(#state{callback_module = Mod, callback_args = Args0, node = Node, from = From}) ->
    Mod:connection_established(From, Node, Args0).

handle_send_challenge(<<$n, _Version:16, _Flag:32, _Challenge:32, Name/binary>> = Data, State) ->
    lager:debug("Handshake - full name of target is '~s'", [Name]),
    send_source(Data, State),
    target_active_once(State),
    loop(State#state{node = binary_to_atom(Name, utf8)}).

invoke_forward_packet_callback(Data, #state{callback_module = Mod, callback_args = Args, node = Target, from = Source}) ->
    Mod:packet(Source, Target, Data, Args).

invoke_reverse_packet_callback(Data, #state{callback_module = Mod, callback_args = Args, node = Target, from = Source}) ->
    Mod:packet(Target, Source, Data, Args).
