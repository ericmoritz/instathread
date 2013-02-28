%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% URLs for the REST service
%%% @end
%%% Created : 27 Feb 2013 by Eric Moritz <eric@eric-acer>

-module(instathread_rest_urls).


-export([routes/0, thread/1]).


routes() ->
    [
     index_route(),
     v1_threads_route(),
     v1_thread_route()
    ].

thread(Entry) ->
    iolist_to_binary([
		      "/v1/threads/",
		      cowboy_http:urlencode(instathread_db_entry:root_key(Entry))
		     ]).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
index_route() ->
    {"/", cowboy_static, 
     [
      {directory, {priv_dir, instathread_rest, [<<"www">>]}},
      {mimetypes, [{<<".html">>, [<<"text/html">>]}]},
      {file, <<"index.html">>}
     ]}.

v1_threads_route() ->
    {"/v1/threads/", instathread_rest_threads_handler, []}.

v1_thread_route() ->
    {"/v1/threads/:root_key", instathread_rest_thread_handler, []}.
