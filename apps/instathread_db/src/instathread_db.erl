-module(instathread_db).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    [Host, Port] = application:get_env(instathread_db, riak),
    instathread_db_client:start_link(Host, Port).

