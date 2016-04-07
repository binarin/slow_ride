-module(slow_epmd_protocol).
-behaviour(ranch_protocol).
-behaviour(gen_server).

-include_lib("slow_ride/include/slow_ride.hrl").

-define(SERVER, ?MODULE).

-define(EPMD_ALIVE2_REQ, $x).
-define(EPMD_PORT_PLEASE2_REQ, $z).
-define(EPMD_ALIVE2_RESP, $y).
-define(EPMD_PORT2_RESP, $w).
-define(EPMD_NAMES, $n).

%% ranch protocol
-export([start_link/4]).

%% gen_server
-export([code_change/3, init/1, terminate/2, handle_call/3, handle_cast/2,
         handle_info/2]).

%% API

%% internal functions
-export([init/4]).

-record(state, {transport_ok
               ,transport_closed
               ,transport_error
               ,transport
               ,socket
               ,alive = false
               ,name
               }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ranch_protocol callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_) ->
    exit(should_be_started_as_ranch_protocol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({OK, Socket, Data}, #state{transport_ok = OK, socket = Socket} = State) ->
    switch_to_raw(State),
    handle_packet(Data, State);
handle_info({Error, Socket, _Reason}, #state{transport_error = Error, socket = Socket} = State) ->
    {stop, normal, State};
handle_info({Closed, Socket}, #state{transport_closed = Closed, socket = Socket} = State) ->
    {stop, normal, State};
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, #state{transport = Transport, socket = Socket, name = NodeName} = State) ->
    case State#state.alive of
        true ->
            ranch:stop_listener({slow_dist_protocol, NodeName});
        _ ->
            ok
    end,
    Transport:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Ref, Socket, Transport, _Opts = []) ->
    process_flag(trap_exit, true),
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    init_socket_opts(Transport, Socket),
    State = new_state(Transport, Socket),
    gen_server:enter_loop(?MODULE, [], State).

handle_packet(<<?EPMD_PORT_PLEASE2_REQ, NodeName/binary>>,
              #state{transport = Transport, socket = Socket} = State) ->
    case slow_ride:port_please(NodeName) of
        {ok, Port, NodeType, Proto, HiVer, LoVer, Extra} ->
            NLen = size(NodeName),
            ELen = size(Extra),
            Transport:send(Socket,
                           <<?EPMD_PORT2_RESP, 0:8, Port:16, NodeType:8, Proto:8, HiVer:16, LoVer:16,
                             NLen:16, NodeName:NLen/binary, ELen:16, Extra:ELen/binary>>);
        _ ->
            Transport:send(Socket, <<?EPMD_PORT2_RESP, 1:8>>)
    end,
    {stop, normal, State};

handle_packet(<<?EPMD_NAMES>>, #state{socket = Socket, transport = Transport} = State) ->
    Names = [io_lib:format("name ~s at port ~b~n", [Name, Port]) || {Name, Port} <- slow_ride:names()],
    Transport:send(Socket, [<<(slow_ride:get_port()):32>>, Names]),
    {stop, normal, State};

handle_packet(<<?EPMD_ALIVE2_REQ, PortNo:16, NodeType:8, Proto:8, HiVer:16,
                LoVer:16, NLen:16, Rest/binary>>,
              State) ->
    <<NodeName:NLen/binary, _ELen:16, Extra/binary>> = Rest,
    Creation = slow_ride:next_creation(NodeName),
    Node = #node{id = NodeName, real_port = PortNo, node_type = NodeType,
                 proto = Proto, hi_ver = HiVer, lo_ver = LoVer, extra = Extra,
                 creation = Creation},
    handle_alive_req(Node, State);

handle_packet(Data, State) ->
    lager:warning("Unknown EPMD packet: ~p", [Data]),
    active_once(State),
    {noreply, State}.

send_alive_resp(Creation, #state{transport = Transport, socket = Socket}) ->
    Transport:setopts(Socket, [{packet, raw}, {active, once}]),
    Packet = <<?EPMD_ALIVE2_RESP, 0:8, Creation:16>>,
    Transport:send(Socket, Packet).

active_once(#state{socket = Socket, transport = Transport}) ->
    Transport:setopts(Socket, [{active, once}]).

init_socket_opts(Transport, Socket) ->
    Transport:setopts(Socket, [binary, {packet, 2}, {active, once}]).

new_state(Transport, Socket) ->
    {OK, Closed, Error} = Transport:messages(),
    #state{transport_ok = OK
          ,transport_closed = Closed
          ,transport_error = Error
          ,transport = Transport
          ,socket = Socket
          }.

switch_to_raw(#state{transport = Transport, socket = Socket}) ->
    Transport:setopts(Socket, [{packet, raw}]).

handle_alive_req(#node{id = NodeName, creation = Creation} = Node, State) ->
    Port = slow_dist_protocol:start_listener(self(), Node#node.id, Node#node.real_port),
    slow_ride:alive(self(), Node#node{fake_port = Port}),
    send_alive_resp(Creation, State),
    {noreply, State#state{alive = true, name = NodeName}}.
