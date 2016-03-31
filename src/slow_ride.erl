-module(slow_ride).
-export([get_port/0]).

get_port() ->
    ranch:get_port(slow_ride).
