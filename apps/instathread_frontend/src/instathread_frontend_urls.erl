%%%-------------------------------------------------------------------
%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% The quickdraw URL module for the single page HTML5 app
%%% @end
%%% Created : 22 Aug 2013 by Eric Moritz <eric@themoritzfamily.com>
%%%-------------------------------------------------------------------
-module(instathread_frontend_urls).

-export([dispatch/0]).

dispatch() ->
    [
     {'_', [
	    {"/", cowboy_static, [
				  {directory, {priv_dir, instathread_frontend, [<<"www">>]}},
				  {mimetypes, [{<<".html">>, [<<"text/html">>]}]},
				  {file, <<"index.html">>}]},
	    {"/static/[...]", cowboy_static, [
					 {directory, {priv_dir, instathread_frontend, [<<"www/static">>]}},
					 {mimetypes, [
						      {<<".js">>, [<<"application/javascript">>]},
						      {<<".css">>, [<<"text/css">>]}
						     ]}
					]}
	   ]}
    ].
