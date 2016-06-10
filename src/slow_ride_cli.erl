-module(slow_ride_cli).
-export([start/0, ctl/0]).
-export([cli_callback_hook/2]).
-export([getenv_int/2, getenv_atom/2]).
-export([action/1]).

start() ->
    application:load(slow_ride),
    application:ensure_all_started(slow_ride),
    slow_ride:callback_module(slow_ride_cli_callback, []),
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

copy_env_vars_to_ctl_env() ->
    application:set_env(slow_ride, node_name, getenv_atom("SLOW_RIDE_NODENAME", 'slow_ride@localhost')).

action([Command|Args]) ->
    action(Command, Args).

action("port", _) ->
    io:format("~b~n", [slow_ride:get_port()]);
action(Cmd, Args) ->
    case cli_callback_hook(Cmd, Args) of
        not_supported ->
            exit({unknown_command, Cmd});
        _ ->
            ok
    end.

cli_callback_hook(Cmd, Args) ->
    {Mod, State} = slow_ride:callback_module(),
    io:format("Looking callback command '~s' in '~s'", [Cmd, Mod]),
    case lists:member({action, 3}, Mod:module_info(exports)) of
        true ->
            Mod:action(Cmd, Args, State);
        _ ->
            exit({unknown_command, Cmd, Mod})
    end.

