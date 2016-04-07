-module(slow_ride).
-behaviour(gen_server).

-export([terminate/2, init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3]).

-export([get_port/0
        ,alive/2
        ,start_link/0
        ,names/0
        ,port_please/1
        ,connect_callback/3
        ,next_creation/1
        ]).

-define(SERVER, ?MODULE).

-record(state, {connect_callback
               ,nodes = #{}
               }).

-include_lib("slow_ride/include/slow_ride.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_port() ->
    ranch:get_port(slow_ride_listener).

alive(ConnectionPid, Node) ->
    gen_server:call(?SERVER, {alive, ConnectionPid, Node}).

port_please(NodeName) ->
    case ets:lookup(slow_ride_nodes, NodeName) of
        [#node{fake_port = Port, node_type = NodeType, proto = Proto, hi_ver = HiVer, lo_ver = LoVer, extra = Extra}] ->
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

-spec next_creation(binary()) -> 1..3.
next_creation(NodeName) ->
    gen_server:call(?SERVER, {next_creation, NodeName}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gen server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    process_flag(trap_exit, true),
    ets:new(slow_ride_nodes, [public, named_table, {keypos, 2}]),
    start_listener(),
    {ok, #state{}}.

handle_call({alive, ConnectionPid, Node}, _From, State) ->
    handle_alive(ConnectionPid, Node, State);

handle_call({connect_callback, M, F, A}, _From, State) ->
    handle_connect_callback(M, F, A, State);

handle_call({next_creation, NodeName}, _From, State) ->
    case ets:lookup(slow_ride_nodes, NodeName) of
        [#node{creation = Creation}] ->
            {reply, 1 + (Creation rem 3), State};
        [] ->
            {reply, 1, State}
    end;

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
handle_alive(ConnectionPid, #node{id = NodeName} = Node, State) ->
    link(ConnectionPid),
    ets:insert(slow_ride_nodes, Node),
    {reply, ok, add_node(ConnectionPid, NodeName, State)}.

handle_connect_callback(M, F, A, State) ->
    {reply, ok, State#state{connect_callback = {M, F, A}}}.

add_node(ConnectionPid, NodeName, #state{nodes = Nodes} = State) ->
    State#state{nodes = Nodes#{ConnectionPid => NodeName}}.

start_listener() ->
    {ok, _} = ranch:start_listener(slow_ride_listener, 2, ranch_tcp, [{port, 0}],
                                   slow_epmd_protocol, []).

stop_listener() ->
    ranch:stop_listener(slow_ride_listener).
