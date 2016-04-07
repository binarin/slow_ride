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
               , parent
               }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_listener(EpmdConn :: pid(), NodeName :: binary(), RealPort :: 1..65535) -> Port when
      Port :: 1..65535.

start_listener(EpmdConn, NodeName, RealPort) ->
    Listener = listener_name(NodeName),
    ranch:start_listener(Listener, 2, ranch_tcp,
                         [{port, 0}], slow_dist_protocol, [EpmdConn, NodeName, RealPort]),
    ranch:get_port(Listener).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ranch_protocol callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Ref, Socket, Tranport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Tranport, Opts]),
    {ok, Pid}.

-define(DD(__Fmt, __Args),
        (fun() ->
                 __UserMsg = io_lib:format(__Fmt, __Args),
                 {{__Year, __Month, __Day}, {__Hour, __Minute, __Second}} = calendar:now_to_local_time(erlang:timestamp()),
                 __Msg = io_lib:format("[~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B ~s ~s:~B] ~s~n",
                                       [__Year, __Month, __Day, __Hour, __Minute, __Second, node(), ?MODULE, ?LINE, __UserMsg]),
                 file:write_file("/tmp/erl-debug.log", __Msg, [append]),
                 error_logger:info_msg("~s", [__Msg])
         end)()).

init(Ref, Socket, Transport, [EpmdConnPid, NodeName, RealPort]) ->
    ok = ranch:accept_ack(Ref),
    State = init_transport(Transport, Socket),
    State1 = State#state{parent = EpmdConnPid, node = NodeName},
    State2 = open_target(RealPort, State1),
    loop(State2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listener_name(NodeName) ->
    {slow_dist_protocol, NodeName}.

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
        %% Hadshake completed
        {OK, TargetSocket, <<$a, _/binary>> = Data} ->
            lager:debug("Handshake - target ack"),
            inet:setopts(TargetSocket, [{packet, 4}, {active, once}]),

            %% Preventing race during packet size switch
            Transport:setopts(Socket, [{active, false}]),
            ok = Transport:send(Socket, Data),
            Transport:setopts(Socket, [{packet, 4}, {active, once}]),

            loop_dist(State);
        %% Handshake data
        {OK, Socket, Data} ->
            lager:debug("Handshake - to target ~p", [Data]),
            ok = gen_tcp:send(TargetSocket, Data),
            Transport:setopts(Socket, [{active, once}]),
            loop(State);
        {OK, TargetSocket, Data} ->
            lager:debug("Handshake - from target ~p", [Data]),
            ok = Transport:send(Socket, Data),
            inet:setopts(TargetSocket, [{active, once}]),
            loop(State);
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
            Transport:setopts(Socket, [{active, once}]),
            loop_dist(State);
        {OK, TargetSocket, Data} ->
            ok = Transport:send(Socket, Data),
            inet:setopts(TargetSocket, [{active, once}]),
            loop_dist(State);
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
