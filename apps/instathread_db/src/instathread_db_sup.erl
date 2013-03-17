% -*- erlang -*-
-module(instathread_db_sup).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
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
    detect_heroku(), % this sets the instathread_db client arg if we're in heroku
    {ok, {WorkerMod, WorkerArgs}} = application:get_env(instathread_db, client),
    {ok, PoolArgs} = application:get_env(instathread_db, pool),
    PoolName = instathread_db_client,
    PoolProps = [{name, {local, PoolName}}, {worker_module, WorkerMod}] ++ PoolArgs,
    PoolSpec = poolboy:child_spec(PoolName, PoolProps, WorkerArgs),
    {ok, { {one_for_one, 10, 10}, [PoolSpec]} }.

detect_heroku() ->
    case redis_to_go_env(os:getenv("REDISTOGO_URL")) of
        {ok, ClientArgs} ->
            error_logger:info_msg("Heroku Detected: ~p~n", [ClientArgs]),
            application:set_env(instathread_db, client, ClientArgs);
        O ->
            error_logger:info_msg("Heroku Not Detected: ~p~n", [O]),
            pass
    end.

%%%%
%% Internal 
%%%%
-ifdef(TEST).
redis_to_go_env_test() ->
    ?assertEqual(
       {ok, {instathread_db_redis_client, ["host", 12345, 0, "password"]}},
       redis_to_go_env("redis://redistogo:password@host:12345/")
      ).
-endif.

redis_to_go_env(false) ->
    false;
redis_to_go_env(URI) ->
    {ok, {_Scheme, UserInfo, Host, Port, _Path, _Query}} = http_uri:parse(URI),
    [_Username, Password] = string:tokens(UserInfo, ":"),
    {ok, {instathread_db_redis_client, [Host, Port, 0, Password]}}.


