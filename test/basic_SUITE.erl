-module(basic_SUITE).
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1]).

-export([ started_app_listens_on_port/1
        , no_listener_after_app_is_stopped/1
        , registering_new_name/1
        , list_of_names_is_returned/1
        , pinging_works/1
        , epmd_connection_is_reported/1
        , dist_connection_is_reported/1
        , packet_callback_invoked/1
        , noop/1
        ]).

-export([connection_reporter/2]).

-behaviour(slow_ride).
-export([ connection_established/3
        , node_registered/2
        , packet/4
        , init/1
        , init_global/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [ registering_new_name
    , no_listener_after_app_is_stopped
    , started_app_listens_on_port
    , list_of_names_is_returned
    , pinging_works
    , epmd_connection_is_reported
    , dist_connection_is_reported
    , packet_callback_invoked
    ].

init_per_suite(Config) ->
    ets:new(dist_packet_count, [public, named_table, {heir, whereis(init), undefined}]),
    ets:insert(dist_packet_count, [{{a, b}, 0}, {{a, c}, 0}]),
    Config.

init_per_testcase(_, Config) ->
    catch application:stop(slow_ride),
    {ok, _} = application:ensure_all_started(slow_ride),
    Config.

end_per_testcase(_, Config) ->
    application:stop(slow_ride),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
noop(_Config) ->
    ok.

started_app_listens_on_port(_Config) ->
    application:ensure_all_started(slow_ride),
    Port = slow_ride:get_port(),
    {ok, _Sock} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary]),
    ok.

registering_new_name(_Config) ->
    {ok, _, Port} = start_node(["-s", "init", "stop"]),
    receive
        {Port, {exit_status, 0}} ->
            ok
    after
        10000 ->
            flush(),
            exit(node_failed_to_start)
    end.

list_of_names_is_returned(_Config) ->
    {ok, N1, _P1} = start_waiting_node(),
    {ok, N2, _P2} = start_waiting_node(),
    EpmdCmd = lists:flatten(io_lib:format("ERL_EPMD_PORT=~b epmd -names", [slow_ride:get_port()])),
    {done, 0, NamesOut} = erlsh:run(EpmdCmd, binary, "/tmp"),
    [<<"epmd: up and running", _/binary>>|NameLines] = re:split(NamesOut, <<"\n">>, [{return, binary}, trim]),
    ParsedNames = [ begin
                        {match, [Name, _Port]} = re:run(Line, <<"^name (.+?) at port (\\d+)">>, [{capture, all_but_first, list}]),
                        list_to_atom(Name ++ "@localhost")
                    end || Line <- NameLines ],
    ?assertEqual(lists:sort([N1, N2]), lists:sort(ParsedNames)),
    ok.

no_listener_after_app_is_stopped(_Config) ->
    Port = slow_ride:get_port(),
    application:stop(slow_ride),
    {error, econnrefused} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary]),
    ok.

pinging_works(_Config) ->
    {ok, N1, _} = start_waiting_node(),
    PingCmd = lists:flatten(io_lib:format("io:format(net_adm:ping('~s'))", [N1])),
    "pong" = start_short_lived_node(["-eval", PingCmd]),
    ok.

connection_reporter(ReportPid, NodeName) ->
    ReportPid ! {connection_to_node, NodeName}.

epmd_connection_is_reported(_Config) ->
    ReportRef = make_ref(),
    slow_ride:callback_module(?MODULE, [self(), ReportRef]),
    {ok, N1, _} = start_waiting_node(),
    [NodePart|_] = string:tokens(atom_to_list(N1), "@"),
    NodePartAtom = list_to_atom(NodePart),
    receive
        {node_registered, ReportRef, NodePartAtom} ->
            ok
    after
        10000 -> exit({no_callback, node_registered})
    end,
    ok.

dist_connection_is_reported(_Config) ->
    ReportRef = make_ref(),
    slow_ride:callback_module(?MODULE, [self(), ReportRef]),
    {ok, N1, _} = start_waiting_node(),
    PingCmd = lists:flatten(io_lib:format("io:format(net_adm:ping('~s'))", [N1])),
    "pong" = start_short_lived_node(["-eval", PingCmd]),
    ct:pal("~p", [N1]),
    receive
        {connection_established, ReportRef, _, N1} ->
            ok
    after
        10000 ->
            exit({no_callback, connection_established})
    end,
    ok.

packet_callback_invoked(_Config) ->
    slow_ride:callback_module(?MODULE, [self(), undefined]),
    {ok, N1, _} = start_waiting_node(),
    PingCmd = lists:flatten(io_lib:format("io:format(net_adm:ping('~s'))", [N1])),
    "pong" = start_short_lived_node(["-eval", PingCmd]),
    case ets:match(dist_packet_count, {{'_', N1}, '$1'}) of
        [[Cnt]] when Cnt > 0 ->
            ok;
        [[0]] ->
            ct:fail(packet_callback_not_invoked);
        [] ->
            ct:pal("Node ~p, ets ~p", [N1, ets:match(dist_packet_count, '$1')]),
            ct:fail(no_traces_of_callbacks_at_all)
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% slow_ride callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(State) ->
    {ok, State}.

init_global(State) ->
    {ok, State}.

connection_established(From, To, [ReportTo, ReportRef] = State) ->
    ReportTo ! {connection_established, ReportRef, From, To},
    ets:insert(dist_packet_count, {{From, To}, 0}),
    ets:insert(dist_packet_count, {{To, From}, 0}),
    {ok, State}.

node_registered(NodeName, [ReportTo, ReportRef] = State) ->
    ReportTo ! {node_registered, ReportRef, NodeName},
    {ok, State}.

packet(From, To, _, State) ->
    ets:update_counter(dist_packet_count, {From, To}, 1),
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flush() ->
    receive
        M ->
            ct:pal("~p", [M]),
            flush()
    after
        0 -> ok
    end.

start_node(Args)->
    EpmdPort = slow_ride:get_port(),
    Name = slow_ride_ct:random_node_name(),
    Port = erlang:open_port({spawn_executable, erlsh:fdlink_executable()},
                            [{args, [os:find_executable("erl"), "-noshell", "-sname", Name ++ "@localhost", "-cookie", "test"] ++ Args}
                            ,{env, [{"ERL_EPMD_PORT", integer_to_list(EpmdPort)}]}
                            ,exit_status
                            ]),
    NameAtom = list_to_atom(Name ++ "@localhost"),
    {ok, NameAtom, Port}.

start_short_lived_node(Args) ->
    {ok, _Name, Port} = start_node(Args ++ ["-s", "erlang", "halt"]),
    node_loop(Port, []).

node_loop(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            node_loop(Port, [Data|Acc]);
        {Port, {exit_status, 0}} ->
            lists:flatten(lists:reverse(Acc));
        {Port, {exit_status, Code}} ->
            exit({non_zero_exit, Code})
    after
        10000 ->
            flush(),
            exit(node_didnt_stop)
    end.

start_waiting_node() ->
    {ok, N, P} = start_node(["-eval", "io:format(\"ok\")."]),
    receive {P, {data, "ok"}} -> ok after 10000 -> exit(node_failed_to_start) end,
    {ok, N, P}.
