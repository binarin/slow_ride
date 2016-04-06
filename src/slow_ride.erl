-module(slow_ride).
-behaviour(gen_server).

-export([terminate/2, init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3]).

-export([get_port/0
        ,alive/8
        ,start_link/0
        ,names/0
        ,port_please/1
        ,connect_callback/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {connect_callback
               ,nodes = #{}
               }).

-record(node, {id
              ,real_port
              ,fake_port
              ,node_type
              ,proto
              ,hi_ver
              ,lo_ver
              ,extra
              ,creation
              }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_port() ->
    ranch:get_port(slow_ride_listener).

alive(ConnectionPid, NodeName, PortNo, NodeType, Proto, HiVer, LoVer, Extra) ->
    gen_server:call(?SERVER, {alive, ConnectionPid, NodeName, PortNo, NodeType, Proto, HiVer, LoVer, Extra}).

port_please(NodeName) ->
    case ets:lookup(slow_ride_nodes, NodeName) of
        [#node{real_port = Port, node_type = NodeType, proto = Proto, hi_ver = HiVer, lo_ver = LoVer, extra = Extra}] ->
            {ok, Port, NodeType, Proto, HiVer, LoVer, Extra};
        [] ->
            {error, not_found}
    end.

-spec names() -> [{binary(), non_neg_integer()}].
names() ->
    ets:foldl(fun(#node{id = Id, real_port = Port}, Acc) ->
                      [{Id, Port} | Acc]
              end, [], slow_ride_nodes).

-spec connect_callback(module(), atom(), term()) -> ok.
connect_callback(M, F, A) ->
    gen_server:call(?SERVER, {connect_callback, M, F, A}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gen server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    process_flag(trap_exit, true),
    ets:new(slow_ride_nodes, [public, named_table, {keypos, 2}]),
    start_listener(),
    {ok, #state{}}.

handle_call({alive, ConnectionPid, NodeName, PortNo, NodeType, Proto, HiVer, LoVer, Extra}, _From, State) ->
    handle_alive(ConnectionPid, NodeName, PortNo, NodeType, Proto, HiVer, LoVer, Extra, State);
handle_call({connect_callback, M, F, A}, _From, State) ->
    handle_connect_callback(M, F, A, State);
handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    stop_listener(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_alive(ConnectionPid, NodeName, PortNo, NodeType, Proto, HiVer, LoVer, Extra, State) ->
    link(ConnectionPid),
    Creation = rand:uniform(3),
    ets:insert(slow_ride_nodes, #node{id = NodeName, real_port = PortNo, node_type = NodeType, proto = Proto, hi_ver = HiVer, lo_ver = LoVer, extra = Extra, creation = Creation}),
    {reply, {ok, Creation}, add_node(ConnectionPid, NodeName, State)}.

handle_connect_callback(M, F, A, State) ->
    {reply, ok, State#state{connect_callback = {M, F, A}}}.

add_node(ConnectionPid, NodeName, #state{nodes = Nodes} = State) ->
    State#state{nodes = Nodes#{ConnectionPid => NodeName}}.

start_listener() ->
    {ok, _} = ranch:start_listener(slow_ride_listener, 100, ranch_tcp, [{port, 0}],
                                   slow_epmd_protocol, []).

stop_listener() ->
    ranch:stop_listener(slow_ride_listener).
