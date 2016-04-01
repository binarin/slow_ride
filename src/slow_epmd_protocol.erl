-module(slow_epmd_protocol).
-behaviour(ranch_protocol).

-define(EPMD_ALIVE2_REQ, $x).
-define(EPMD_PORT_PLEASE2_REQ, $z).
-define(EPMD_ALIVE2_RESP, $y).
-define(EPMD_PORT2_RESP, $w).
-define(EPMD_NAMES, $n).

-export([start_link/4]).
-export([init/4]).

-record(state, {transport_ok
               ,transport_closed
               ,transport_error
               ,transport
               ,socket
               }).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    Transport:setopts(Socket, [binary, {packet, 2}, {active, once}]),
    {OK, Closed, Error} = Transport:messages(),
    State = #state{transport_ok = OK
                  ,transport_closed = Closed
                  ,transport_error = Error
                  ,transport = Transport
                  ,socket = Socket
                  },
    loop(State).

loop(#state{socket = Socket, transport = Transport, transport_ok = OK} = State) ->
    receive
        {OK, Socket, Data} ->
            switch_to_raw(State),
            handle_packet(Data, State);
        M ->
            io:format(standard_error, "~p~n", [M]),
            Transport:setopts(Socket, [{active, once}]),
            loop(State)
    after
        2000 ->
            Transport:close(Socket)
    end.

switch_to_raw(#state{transport = Transport, socket = Socket}) ->
    Transport:setopts(Socket, [{packet, raw}]).

handle_packet(<<?EPMD_PORT_PLEASE2_REQ, NodeName/binary>>, #state{transport = Transport, socket = Socket}) ->
    case slow_ride:port_please(NodeName) of
        {ok, Port, NodeType, Proto, HiVer, LoVer, Extra} ->
            NLen = size(NodeName),
            ELen = size(Extra),
            Transport:send(Socket,
                           <<?EPMD_PORT2_RESP, 0:8, Port:16, NodeType:8, Proto:8, HiVer:16, LoVer:16,
                             NLen:16, NodeName:NLen/binary, ELen:16, Extra:ELen/binary>>),
            Transport:close(Socket);
        _ ->
            Transport:send(Socket, <<?EPMD_PORT2_RESP, 1:8>>),
            Transport:close(Socket)
    end;

handle_packet(<<?EPMD_NAMES>>, #state{socket = Socket, transport = Transport}) ->
    Names = [io_lib:format("name ~s at port ~b~n", [Name, Port]) || {Name, Port} <- slow_ride:names()],
    Transport:send(Socket, [<<(slow_ride:get_port()):32>>, Names]),
    Transport:close(Socket);

handle_packet(<<?EPMD_ALIVE2_REQ, PortNo:16, NodeType:8, Proto:8, HiVer:16, LoVer:16, NLen:16, Rest/binary>>,
              State) ->
    <<NodeName:NLen/binary, _ELen:16, Extra/binary>> = Rest,
    {ok, Creation} = slow_ride:alive(self(), NodeName, PortNo, NodeType, Proto, HiVer, LoVer, Extra),
    send_alive_resp(Creation, State),
    loop_alive(State);

handle_packet(Data, State) ->
    io:format(standard_error, "DATA: ~p~n", [Data]),
    loop(State).

send_alive_resp(Creation, #state{transport = Transport, socket = Socket}) ->
    Transport:setopts(Socket, [{packet, raw}]),
    Packet = <<?EPMD_ALIVE2_RESP, 0:8, Creation:16>>,
    Transport:send(Socket, Packet).

loop_alive(#state{transport_closed = Closed,
                  transport_error = Error,
                  socket = Socket}) ->
    receive
        {Closed, Socket} ->
            ok;
        {Error, Socket, _Reason} ->
            ok;
        Unknown ->
            exit(Unknown)
    end.
