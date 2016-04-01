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

handle_packet(<<?EPMD_NAMES>>, #state{socket = Socket, transport = Transport} = State) ->
    switch_to_raw(State),
    Names = [io_lib:format("name ~s at port ~b~n", [Name, Port]) || {Name, Port} <- slow_ride:names()],
    Transport:send(Socket, [<<(slow_ride:get_port()):32>>, Names]),
    Transport:close(Socket);

handle_packet(<<?EPMD_ALIVE2_REQ, PortNo:16, _NodeType:8, _Proto:8, _HiVer:16, _LoVer:16, NLen:16, Rest/binary>>,
              State) ->
    <<NodeName:NLen/binary, _ELen:16, _Extra/binary>> = Rest,
    slow_ride:alive(self(), NodeName, PortNo),
    send_alive_resp(State),
    loop_alive(State);

handle_packet(Data, State) ->
    io:format(standard_error, "DATA: ~p~n", [Data]),
    loop(State).

send_alive_resp(#state{transport = Transport, socket = Socket}) ->
    Transport:setopts(Socket, [{packet, raw}]),
    Creation = rand:uniform(3),
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
