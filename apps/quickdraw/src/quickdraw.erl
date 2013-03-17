% -*- erlang -*-
%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% Simple tools for easy multi-app Cowboy projects
%%% @end
%%% Created :  7 Mar 2013 by Eric Moritz <eric@eric-acer>

-module(quickdraw).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([dispatch/1]).

%%--------------------------------------------------------------------
%% @doc
%% Compile dispatch rules from multiple applications
%% @end
%%--------------------------------------------------------------------
-spec dispatch([module()]) -> cowboy_router:dispatch_rules().
dispatch(Modules) ->
    dispatch(Modules, orddict:new()).

dispatch([], PathsByHost) ->
    orddict:to_list(PathsByHost);
dispatch([Fun|Rest], PathsByHost) when is_function(Fun) ->
    Dispatch = Fun(),
    PathsByHost2 = reduce_paths(
		    PathsByHost,
		    Dispatch),
    dispatch(Rest, PathsByHost2);
dispatch([Module|Rest], PathsByHost) ->
    Dispatch = Module:dispatch(),

    PathsByHost2 = reduce_paths(
		     PathsByHost,
		     Dispatch
		    ),
    dispatch(Rest, PathsByHost2).


reduce_paths(PathsByHost, Dispatch) ->
    lists:foldl(
      fun dispatch_reducer/2,
      PathsByHost,
      Dispatch
     ).

dispatch_reducer({Host, Paths}, PathsByHost) ->			     
    orddict:append_list(Host, Paths, PathsByHost).

-ifdef(TEST).
dispatch_test() ->
    D1 = fun() ->
		 [
		  {'_', [{"/foo", foo_handler, []}]},
		  {"example.com", [{"/", index_handler, []}]}
		 ]
	 end,
    D2 = fun() ->
		 [
		  {'_', [{"/bar", bar_handler, []}]},
		  {"example.com", [{"/story", story_handler, []}]}
		 ]
	 end,
		 
    ?assertEqual([
		  {'_', [
			 {"/foo", foo_handler, []},
			 {"/bar", bar_handler, []}
			]},
		  {"example.com", [
				   {"/", index_handler, []},
				   {"/story", story_handler, []}
				  ]}
		 ],
		 dispatch([D1, D2])
		).

-endif.
