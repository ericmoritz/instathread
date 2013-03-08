-module(instathread_web_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Port = port(),
    instathread_web_sup:start_link(Port).

stop(_State) ->
    ok.


%%%%
%% Internal
%%%%
port() ->
    port(
      os:getenv("POST"),
      application:get_env(instathread_web, port)
     ).

port(false, undefined) ->
    error("set $PORT or set the instathread_web apps port configuration parameter");
port(OSVar, _) ->
    OSVar;
port(_, AppVar) ->
    AppVar.
