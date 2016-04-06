%%%-------------------------------------------------------------------
%% @doc slow_ride top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(slow_ride_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SlowRide = #{id => ?SERVER
                ,start => {slow_ride, start_link, []}
                ,restart => permanent
                ,shutdown => 3600000
                ,type => worker
                ,modules => [slow_ride]
                },
    {ok, { {one_for_all, 0, 1}, [SlowRide]} }.

%%====================================================================
%% Internal functions
%%====================================================================
