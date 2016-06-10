-module(slow_ride_ct).
-export([start_waiting_node/1
        ,random_node_name/0
        ,run_in_background/1
        ,run_in_foreground/1
        ,run_in_foreground/2
        ,run_for_stdout/1
        ,pinger_node/3
        ]).

random_node_name() ->
    [ $a + rand:uniform(10) - 1 || _ <- lists:seq(1, 26) ].

start_waiting_node(EpmdPort) ->
    {ok, N, P} = start_node(EpmdPort, ["-eval", "io:format(\"ok\")."]),
    receive {P, {data, "ok"}} -> ok after 10000 -> exit(node_failed_to_start) end,
    {ok, N, P}.

start_node(EpmdPort, Args) ->
    start_node(list_to_atom(random_node_name() ++ "@localhost"), EpmdPort, Args).

start_node(Name, EpmdPort, Args)->
    Port = erlang:open_port({spawn_executable, erlsh:fdlink_executable()},
                            [{args, [os:find_executable("erl"), "-noshell", "-sname", node_name_as_str(Name), "-cookie", "test"] ++ Args}
                            ,{env, [{"ERL_EPMD_PORT", integer_to_list(EpmdPort)}]}
                            ,exit_status
                            ]),
    {ok, Name, Port}.

start_short_lived_node(Name, EpmdPort, Args) ->
    {ok, _, Port} = start_node(Name, EpmdPort, Args ++ ["-s", "erlang", "halt"]),
    port_loop(Port, []).

run_in_background(CmdAndArgs) ->
    spawn_link(fun() ->
                       run_in_foreground(CmdAndArgs)
               end),
    ok.

run_in_foreground(CmdAndArgs) ->
    run_in_foreground(CmdAndArgs, []).

run_in_foreground(CmdAndArgs, Opts) ->
    Port = erlang:open_port({spawn_executable, erlsh:fdlink_executable()},
                            Opts ++ [{args, CmdAndArgs},
                                     exit_status,
                                     stderr_to_stdout]),
    ct:pal("~p: Starting ~p", [Port, CmdAndArgs]),
    pal_loop(Port).

pal_loop(Port) ->
    receive
        {Port, {data, Data}} ->
            ct:pal("~p: ~s", [Port, Data]),
            pal_loop(Port);
        {Port, {exit_status, 0}} ->
            ct:pal("~p: normal exit", [Port]),
            ok;
        {Port, {exit_status, Code}} ->
            exit({non_zero_exit, Port, Code})
    end.

run_for_stdout(CmdAndArgs) ->
    Port = erlang:open_port({spawn_executable, erlsh:fdlink_executable()},
                            [{args, CmdAndArgs}
                            ,exit_status
                            ]),
    port_loop_no_err(Port, []).

port_loop_no_err(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            port_loop_no_err(Port, [Data|Acc]);
        {Port, {exit_status, _}} ->
            lists:flatten(lists:reverse(Acc))
    end.

port_loop(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            port_loop_no_err(Port, [Data|Acc]);
        {Port, {exit_status, 0}} ->
            lists:flatten(lists:reverse(Acc));
        {Port, {exit_status, Code}} ->
            exit({non_zero_exit, Port, Code})
    end.

pinger_node(Name, EpmdPort, Target) ->
    PingCmd = lists:flatten(io_lib:format("io:format(net_adm:ping('~s'))", [Target])),
    case start_short_lived_node(Name, EpmdPort, ["-eval", PingCmd]) of
        "pang" ->
            "pang";
        "pong" ->
            "pong"
    end.

node_name_as_str(Name) when is_atom(Name) ->
    atom_to_list(Name);
node_name_as_str(Name) when is_list(Name) ->
    Name.
