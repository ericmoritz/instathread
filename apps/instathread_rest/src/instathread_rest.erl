%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% 
%%% @end
%%% Created : 27 Feb 2013 by Eric Moritz <eric@eric-acer>

-module(instathread_rest).

-export([start/0]).

start() ->
    apptools:ensure_started(?MODULE, permanent),
    Dispatch = cowboy_router:compile(
		 instathread_rest_urls:dispatch()
		),
    % start the dev server
    {ok, _} = cowboy:start_http(?MODULE, 100,
				[{port, 8000}],
				[{env, [{dispatch, Dispatch}]}]),
    ok.
		     
		     


