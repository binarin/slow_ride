-module(cli_SUITE).
-include_lib("common_test/include/ct.hrl").

-define(TEST_NODE, 'slow-ride-cli-test@localhost').

%% ct
-export([all/0, init_per_suite/1]).

%% cases
-export([blocking_prevents_new_connections/1
        ,blocking_drops_existing_connection/1
        ,unblocking_works/1
        ]).

%% cli action callback
-export([action/3]).


action(_Cmd, _Args, _State) ->
    not_supported.

all() ->
    [blocking_prevents_new_connections
    ,blocking_drops_existing_connection
    ,unblocking_works
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
    N2 = slow_ride_ct:random_node_name(),
    run_ctl(["block", atom_to_list(N1), atom_to_list(N2)], Config),
    "pang" = slow_ride_ct:pinger_node(N2, EpmdPort, N1),
    ok.

blocking_drops_existing_connection(Config) ->
    EpmdPort = start_slow_ride([], Config),
    {ok, N1, _} = slow_ride_ct:start_waiting_node(EpmdPort),
    Script = io_lib:format("
        monitor_node('~s', true),
        pong = net_adm:ping('~s'),
        io:format(\"CONNECT!!!\"),
        receive
          {nodedown, '~s'} -> io:format(\"DOWN!!!\")
        after
          10000 -> io:format(\"MISSING!!!\")
        end
    ", [N1, N1, N1]),
    {ok, N2, P2} = slow_ride_ct:start_waiting_node(EpmdPort, ["-eval", Script]),
    receive {P2, {data, "CONNECT!!!"}} -> ok after 10000 -> exit(no_connect_ack) end,
    run_ctl(["block", atom_to_list(N1), atom_to_list(N2)], Config),
    receive
        {P2, {data, "DOWN!!!"}} ->
            ct:pal("Disconnect acknowledged"),
            ok;
        {P2, {data, "MISSING!!!"}} ->
            exit(missing_nodedown_detected)
    after
        20000 ->
            exit(test_script_timeout)
    end,
    ok.

-define(UNBLOCK_SCRIPT, "
    Res = lists:foldr(fun (_, trying)->
                              case net_adm:ping(@N1@) of
                                  pong ->
                                      success;
                                  pang ->
                                      timer:sleep(100),
                                      trying
                              end;
                          (_, success) ->
                              success;
                          (_, failure) ->
                              failure
                      end,
                      trying,
                      lists:seq(1, 20)),
    case Res of
        success ->
            io:format(\"UNBLOCKED!!!\");
        failure ->
            io:format(\"FAILED!!!\")
    end.
").

unblocking_works(Config) ->
    EpmdPort = start_slow_ride([], Config),
    {ok, N1, _} = slow_ride_ct:start_waiting_node(EpmdPort),
    N2 = slow_ride_ct:random_node_name(),
    Script = re:replace(?UNBLOCK_SCRIPT, "@N1@", "'" ++ atom_to_list(N2) ++ "'", [{return, list}]),
    {ok, N2, P2} = slow_ride_ct:start_waiting_node(N2, EpmdPort, ["-eval", Script]),
    run_ctl(["unblock", atom_to_list(N1), atom_to_list(N2)], Config),
    receive
        {P2, {data, "UNBLOCKED!!!"}} ->
            ct:pal("Reconnect acknowledged"),
            ok;
        {P2, {data, "FAILED!!!"}} ->
            exit(missing_nodedown_detected)
    after
        5000 ->
            exit(test_script_timeout)
    end,
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
