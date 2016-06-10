-module(cli_SUITE).
-include_lib("common_test/include/ct.hrl").

-define(TEST_NODE, 'slow-ride-cli-test@localhost').

%% ct
-export([all/0, init_per_suite/1]).

%% cases
-export([blocking_prevents_new_connections/1
        ,blocking_drops_existing_connection/1
        ,custom_callback_action_invoked/1
        ]).

%% cli action callback
-export([action/3]).


action(_Cmd, _Args, _State) ->
    not_supported.

all() ->
    [blocking_prevents_new_connections
    ,blocking_drops_existing_connection
    ,custom_callback_action_invoked
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_per_suite(Config) ->
    SlowRideExe = prepare_executable(),
    [{exe, SlowRideExe}|Config].

blocking_prevents_new_connections(Config) ->
    EpmdPort = start_slow_ride([], Config),
    {ok, N1, _} = slow_ride_ct:start_waiting_node(EpmdPort),
    N2 = list_to_atom(slow_ride_ct:random_node_name() ++ "@localhost"),
    run_ctl(["block", atom_to_list(N1), atom_to_list(N2)], Config),
    "pang" = slow_ride_ct:pinger_node(N2, EpmdPort, N1),
    ok.

blocking_drops_existing_connection(_Config) ->
    ok.

custom_callback_action_invoked(Config) ->
    start_slow_ride([], Config),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
force_shutdown() ->
    os:cmd(io_lib:format("pkill -f 'beam.*-sname ~s'", [?TEST_NODE])).

start_slow_ride(Args, Config) ->
    force_shutdown(),
    open_slow_ride_port(Args, Config),
    wait_app(Config).

wait_app(Config) ->
    timer:sleep(1000),
    wait_app(20, Config).

wait_app(0, _Config) ->
    exit(listener_wait_timeout);
wait_app(N, Config) ->
    case slow_ride_ct:run_for_stdout([?config(exe, Config), "port", "--node", atom_to_list(?TEST_NODE)]) of
        [Digit|_] = TCPPort when Digit >= $0 andalso Digit =< $9 ->
            list_to_integer(string:strip(TCPPort, right, $\n));
        Err when N =:= 1 ->
            exit(Err);
        _ ->
            timer:sleep(500),
            wait_app(N-1, Config)
    end.

open_slow_ride_port(Args, Config) ->
    SlowRideWithArgs = [?config(exe, Config), "foreground",
                        "--port", "0",
                        "--node", atom_to_list(?TEST_NODE)
                       ] ++ Args,
    slow_ride_ct:run_in_background(SlowRideWithArgs),
    ok.

run_ctl(Args, Config) ->
    slow_ride_ct:run_in_foreground([?config(exe, Config),
                                    "--node", atom_to_list(?TEST_NODE)] ++
                                       Args, [stderr_to_stdout]).

prepare_executable() ->
    TestProfilePriv = code:priv_dir(slow_ride),
    ["priv", "slow_ride", "lib", "test", "_build"|Rest] = lists:reverse(filename:split(TestProfilePriv)),
    Root = filename:join(lists:reverse(Rest)),
    slow_ride_ct:run_in_foreground([os:find_executable("rebar3"), "release"], [{cd, Root}, stderr_to_stdout]),
    filename:join(Root, "scripts/slow-ride-ctl").
