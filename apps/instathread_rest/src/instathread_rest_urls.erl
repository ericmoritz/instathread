%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% URLs for the REST service
%%% @end
%%% Created : 27 Feb 2013 by Eric Moritz <eric@eric-acer>

-module(instathread_rest_urls).
-behaviour(quickdraw_dispatch).

-export([dispatch/0, thread/1, thread_poll/1]).


dispatch() ->
    [{'_', 
     [
      index_route(),
      v1_threads_route(),
      v1_thread_route(),
      v1_thread_poll_route()
     ]
    }].

thread(RootKey) when is_binary(RootKey) ->
    iolist_to_binary([
		      "/service/v1/threads/",
		      cowboy_http:urlencode(RootKey)
		     ]);
thread(Entry) ->
    thread(instathread_db_entry:root_key(Entry)).


thread_poll(RootKey) when is_binary(RootKey) ->
    iolist_to_binary([
		      thread(RootKey),
		      "/poll"
		     ]);
thread_poll(Entry) ->
    SinceKey = instathread_db_entry:key(Entry),
    PollRoot  = thread_poll(instathread_db_entry:root_key(Entry)),
    iolist_to_binary([
		      PollRoot,
		      "/", cowboy_http:urlencode(SinceKey)
		     ]).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
index_route() ->
    {"/service/v1/", cowboy_static, 
     [
      {directory, {priv_dir, instathread_rest, [<<"www">>]}},
      {mimetypes, [{<<".html">>, [<<"text/html">>]}]},
      {file, <<"index.html">>}
     ]}.

v1_threads_route() ->
    {"/service/v1/threads/", instathread_rest_threads_handler, []}.

v1_thread_route() ->
    {"/service/v1/threads/:root_key", instathread_rest_thread_handler, []}.

v1_thread_poll_route() ->
    {"/service/v1/threads/:root_key/poll/:since", instathread_rest_thread_poll_handler, []}.
