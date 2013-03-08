%%% @author Eric Moritz <eric@eric-acer>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% The quickdraw_dispatch behaviour
%%% @end
%%% Created :  7 Mar 2013 by Eric Moritz <eric@eric-acer>

-module(quickdraw_dispatch).


-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{dispatch, 0}];
behaviour_info(_) ->
    undefined.
