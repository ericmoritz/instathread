-module(instathread_web).

-export([start/0]).


start() ->
    apptools:ensure_started(?MODULE, permanent).
