-module(slow_ride).
-behaviour(gen_server).

-callback connection_established(node(), node(), State) -> {ok, State} when State :: term().
-callback node_registered(NodeWithoutHost :: atom(), State) -> {ok, State} when State :: term().
-callback packet(node(), node(), binary(), State) -> {ok, State} when State :: term().
%% init_global called once, its result is then passed to init of every connection
-callback init_global(term()) -> {ok, State} when State :: term().
-callback init(term()) -> {ok, State} when State :: term().

-export([terminate/2, init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3]).

-export([ get_port/0
        , alive/2
        , start_link/0
        , names/0
        , port_please/1
        , callback_module/2
        , callback_module/0
        , next_creation/1
        , node_pair/2
        ]).

-define(SERVER, ?MODULE).

-record(state, {callback_module = slow_ride_noop_callback
               ,callback_args = undefined
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

-spec callback_module() -> {module(), term()}.
callback_module() ->
    gen_server:call(?SERVER, callback_module).

-spec callback_module(module(), term()) -> ok.
callback_module(Mod, Args) ->
    gen_server:call(?SERVER, {callback_module, Mod, Args}).

-spec next_creation(binary()) -> 1..3.
next_creation(NodeName) ->
    gen_server:call(?SERVER, {next_creation, NodeName}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helpers for callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Sorted tuple of two nodes, so we can identify connection between
%% nodes irregarding who was the initiator of that connection.
node_pair(N1, N2) when N1 < N2 ->
    {N1, N2};
node_pair(N1, N2) ->
    {N2, N1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gen server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    process_flag(trap_exit, true),
    ets:new(slow_ride_nodes, [public, named_table, {keypos, 2}]),
    start_listener(),
    {ok, CallbackModule} = application:get_env(callback_module),
    {ok, CallbackArgs} = application:get_env(callback_args),
    {ok, CallbackState} = CallbackModule:init_global(CallbackArgs),
    State = #state{callback_module = CallbackModule, callback_args = CallbackState},
    lager:info("Started slow_ride on port ~b with callback ~s", [slow_ride:get_port(), CallbackModule]),
    {ok, State}.

handle_call({alive, ConnectionPid, Node}, _From, State) ->
    handle_alive(ConnectionPid, Node, State);

handle_call({callback_module, M, A}, _From, State) ->
    handle_callback_module(M, A, State);

handle_call(callback_module, _From, #state{callback_module = Module, callback_args = Args} = State) ->
    {reply, {Module, Args}, State};

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

handle_callback_module(M, A, State) ->
    {reply, ok, State#state{callback_module = M, callback_args = A}}.

add_node(ConnectionPid, NodeName, #state{nodes = Nodes} = State) ->
    State#state{nodes = Nodes#{ConnectionPid => NodeName}}.

start_listener() ->
    {ok, Port} = application:get_env(port),
    {ok, _} = ranch:start_listener(slow_ride_listener, 2, ranch_tcp, [{port, Port}],
                                   slow_epmd_protocol, []).

stop_listener() ->
    ranch:stop_listener(slow_ride_listener).
