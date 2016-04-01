-module(slow_ride).
-behaviour(gen_server).

-export([terminate/2, init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3]).

-export([get_port/0
        ,alive/3
        ,start_link/0
        ,names/0
        ]).

-define(SERVER, ?MODULE).

-record(state, {}).

-record(node, {id
              ,real_port
              }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_port() ->
    ranch:get_port(slow_ride_listener).

alive(ConnectionPid, NodeName, PortNo) ->
    gen_server:call(?MODULE, {alive, ConnectionPid, NodeName, PortNo}).

-spec names() -> [{binary(), non_neg_integer()}].
names() ->
    ets:foldl(fun(#node{id = Id, real_port = Port}, Acc) ->
                      [{Id, Port} | Acc]
              end, [], slow_ride_nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gen server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    process_flag(trap_exit, true),
    ets:new(slow_ride_nodes, [public, named_table, {keypos, 2}]),
    {ok, #state{}}.

handle_call({alive, ConnectionPid, NodeName, PortNo}, _From, State) ->
    handle_alive(ConnectionPid, NodeName, PortNo, State);
handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_alive(ConnectionPid, NodeName, PortNo, State) ->
    link(ConnectionPid),
    ets:insert(slow_ride_nodes, #node{id = NodeName, real_port = PortNo}),
    {reply, ok, State}.
