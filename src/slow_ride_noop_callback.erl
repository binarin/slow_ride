-module(slow_ride_noop_callback).
-behaviour(slow_ride).

-export([connection_established/3
        ,node_registered/2
        ,packet/4
        ]).

connection_established(_SourceNode, _DestinationNode, State) ->
    {ok, State}.

node_registered(_NodeName, State) ->
    {ok, State}.

packet(_SourceNode, _DestinationNode, _Data, State) ->
    {ok, State}.
