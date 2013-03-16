
-module(instathread_db_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {PoolArgs, WorkerArgs}} = application:get_env(instathread_db, pool),
    PoolName = instathread_db_client,
    PoolSpec = poolboy:child_spec(
                 PoolName,
                 [{name, {local, PoolName}}] ++ PoolArgs,
                 WorkerArgs
                ),
    {ok, { {one_for_one, 10, 10}, [PoolSpec]} }.

