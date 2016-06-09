-module(slow_ride_cli).
-export([start/0, ctl/0]).

start() ->
    application:load(slow_ride),
    copy_env_vars_to_app_env(),
    application:ensure_all_started(slow_ride),
    lager:info("Started slow_ride ~s on port ~b", [node(), slow_ride:get_port()]).

ctl() ->
    try
        copy_env_vars_to_ctl_env(),
        dispatch()
    catch
        exit:{nodedown, Node} ->
            err("Node ~s is apparently down", [Node]);
        exit:{unknown_command, Command} ->
            err("Command '~s' is not supported", [Command]);
        C:E ->
            io:format(standard_error, "Uncaught exception ~p:~p at ~p~n", [C, E, erlang:get_stacktrace()]),
            erlang:halt(1)
    end,
    erlang:halt(0).

err(Fmt, Args) ->
    io:format(standard_error, Fmt ++ "~n", Args),
    erlang:halt(1).


dispatch() ->
    [Command|Args] = init:get_plain_arguments(),
    action(Command, Args).

getenv_int(Name, Default) ->
    case os:getenv(Name) of
        false ->
            Default;
        Str ->
            list_to_integer(Str)
    end.

getenv_atom(Name, Default) ->
    case os:getenv(Name) of
        false ->
            Default;
        Str ->
            list_to_atom(Str)
    end.

copy_env_vars_to_app_env() ->
    application:set_env(slow_ride, port, getenv_int("SLOW_RIDE_PORT", 0)).

copy_env_vars_to_ctl_env() ->
    application:set_env(slow_ride, node_name, getenv_atom("SLOW_RIDE_NODENAME", 'slow_ride@localhost')).

rpc(M, F, A) ->
    {ok, Node} = application:get_env(slow_ride, node_name),
    case rpc:call(Node, M, F, A) of
        {badrpc, nodedown} ->
            exit({nodedown, Node});
        {badrpc, _} = E ->
            exit(E);
        Res ->
            Res
    end.

action("stop", _) ->
    rpc(erlang, time, []),
    try
        rpc(erlang, halt, [])
    catch
        exit:{nodedown, _} ->
            ok
    end;
action("port", _) ->
    io:format("~b~n", [rpc(slow_ride, get_port, [])]);
action(Cmd, _) ->
    exit({unknown_command, Cmd}).
