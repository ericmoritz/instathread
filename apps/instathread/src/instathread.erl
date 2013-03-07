-module(instathread).

-export([start/0]).

start() ->
    apptools:ensure_started(?MODULE, permanent).
