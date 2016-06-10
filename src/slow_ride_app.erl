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
    %% lager:set_loglevel(lager_console_backend, debug),
    copy_env_vars_to_app_env(),
    slow_ride_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
copy_env_vars_to_app_env() ->
    application:set_env(slow_ride, port, slow_ride_cli:getenv_int("SLOW_RIDE_PORT", 0)),
    application:set_env(slow_ride, callback_module, slow_ride_cli:getenv_atom("SLOW_RIDE_CALLBACK_MODULE", slow_ride_noop_callback)).
