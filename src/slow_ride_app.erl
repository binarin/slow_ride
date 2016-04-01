%%%-------------------------------------------------------------------
%% @doc slow_ride public API
%% @end
%%%-------------------------------------------------------------------

-module(slow_ride_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    start_listener(),
    slow_ride_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ranch:stop_listener(slow_ride_listener),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_listener() ->
    {ok, _} = ranch:start_listener(slow_ride_listener, 100, ranch_tcp, [{port, 0}],
                                   slow_epmd_protocol, []).
