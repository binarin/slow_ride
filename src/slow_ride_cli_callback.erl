-module(slow_ride_cli_callback).
-behaviour(slow_ride).

-export([connection_established/3
        ,node_registered/2
        ,packet/4
        ,init/1
        ,init_global/1
        ,handle_dist_info/2
        ]).

-export([action/3]).

init(State) ->
    {ok, State}.

init_global(State) ->
    lager:info("Initializing ~s", [?MODULE]),
    ets:new(blocked_connections, [public, named_table, {read_concurrency, true}]),
    {ok, State}.

connection_established(Source, Dest, State) ->
    case is_connection_allowed(Source, Dest) of
        true ->
            lager:info("Connection between '~s' and '~s' established", [Source, Dest]),
            gproc:add_local_name(slow_ride:node_pair(Source, Dest)),
            {ok, #{source => Source, destination => Dest}};
        false ->
            lager:info("Connection between '~s' and '~s' blocked, dropping", [Source, Dest]),
            {drop, State}
    end.

node_registered(NodeName, State) ->
    lager:info("Node '~s' registered", [NodeName]),
    {ok, State}.

packet(_SourceNode, _DestinationNode, _Data, State) ->
    {ok, State}.

handle_dist_info(drop, State) ->
    #{source := Source, destination := Dest} = State,
    lager:info("Dropping existing connection between ~s and ~s", [Source, Dest]),
    {drop, State};
handle_dist_info(Info, State) ->
    lager:info("Unknown info ~p", [Info]),
    {ok, State}.

action("block", [N1, N2], _) ->
    io:format("blocking communications between ~s and ~s", [N1, N2]),
    block(N1, N2),
    ok;
action("unblock", [N1, N2], _) ->
    io:format("unblocking communications between ~s and ~s", [N1, N2]),
    unblock(list_to_atom(N1), list_to_atom(N2));
action(_, _, _) ->
    not_supported.

block(N1, N2) ->
    Name = slow_ride:node_pair(list_to_atom(N1), list_to_atom(N2)),
    ets:insert(blocked_connections, [{Name}]),
    notify_dist_proc(Name, drop),
    ok.

unblock(N1, N2) ->
    Name = slow_ride:node_pair(N1, N2),
    ets:delete(blocked_connections, Name),
    ok.

is_connection_allowed(N1, N2) ->
    case ets:lookup(blocked_connections, slow_ride:node_pair(N1, N2)) of
        [_] ->
            false;
        _ ->
            true
    end.

notify_dist_proc(Name, Message) ->
    case gproc:lookup_pids({n, l, Name}) of
        [Pid] ->
            Pid ! Message;
        _ ->
            ok
    end,
    ok.
