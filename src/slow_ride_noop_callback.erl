-module(slow_ride_noop_callback).
-behaviour(slow_ride).

-export([connection_established/3
        ,node_registered/2
        ,packet/4
        ,init/1
        ,init_global/1
        ,handle_dist_info/2
        ]).

init(State) ->
    {ok, State}.

init_global(State) ->
    {ok, State}.

handle_dist_info(_, State) ->
    {ok, State}.

connection_established(_SourceNode, _DestinationNode, State) ->
    {ok, State}.

node_registered(_NodeName, State) ->
    {ok, State}.

packet(_SourceNode, _DestinationNode, _Data, State) ->
    {ok, State}.
