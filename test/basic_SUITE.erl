-module(basic_SUITE).
-export([all/0]).
-export([started_app_listens_on_port/0]).

all() ->
    [started_app_listens_on_port].

started_app_listens_on_port() ->
    ok.
