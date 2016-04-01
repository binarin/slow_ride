-module(basic_SUITE).
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([started_app_listens_on_port/1
        ,no_listener_after_app_is_stopped/1
        ,registering_new_name/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [registering_new_name
    ,no_listener_after_app_is_stopped
    ,started_app_listens_on_port
    ].

init_per_testcase(_, Config) ->
    application:ensure_all_started(slow_ride),
    Config.

end_per_testcase(_, Config) ->
    application:stop(slow_ride),
    Config.

started_app_listens_on_port(_Config) ->
    application:ensure_all_started(slow_ride),
    Port = slow_ride:get_port(),
    {ok, _Sock} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary]),
    ok.

registering_new_name(_Config) ->
    {ok, _, Port} = start_node(),
    receive
        {Port, {exit_status, 0}} ->
            ok
    after
        10000 ->
            flush(),
            exit(node_failed_to_start)
    end.

flush() ->
    receive
        M ->
            ct:pal("~p", [M]),
            flush()
    after
        0 -> ok
    end.

no_listener_after_app_is_stopped(_Config) ->
    Port = slow_ride:get_port(),
    application:stop(slow_ride),
    {error, econnrefused} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary]),
    ok.

random_node_name() ->
    [ $a + rand:uniform(10) - 1 || _ <- lists:seq(1, 26) ].

start_node()->
    EpmdPort = slow_ride:get_port(),
    Name = random_node_name(),
    Port = erlang:open_port({spawn_executable, erlsh:fdlink_executable()},
                            [{args, [os:find_executable("erl"), "-noshell", "-sname", Name, "-cookie", "test", "-eval", "io:format(\"ok\").", "-s", "init", "stop"]}
                            ,{env, [{"ERL_EPMD_PORT", integer_to_list(EpmdPort)}]}
                            ,exit_status
                            ]),
    {ok, Name, Port}.
