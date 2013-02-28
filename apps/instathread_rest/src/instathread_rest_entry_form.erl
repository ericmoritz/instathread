%%% @author Eric Moritz <eric@eric-acer>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%%
%%% @end
%%% Created : 28 Feb 2013 by Eric Moritz <eric@eric-acer>

-module(instathread_rest_entry_form).

-export([create_entry/1, create_entry/2]).

create_entry(BodyResult) ->
    DataResult = validate(BodyResult),
    EntryResult = form_to_entry(DataResult),
    store_entry(EntryResult).

create_entry(RootKey, BodyResult) ->
    DataResult = validate(BodyResult),
    EntryResult = form_to_entry(RootKey, DataResult),
    store_entry(EntryResult).

store_entry(E={error, _}) ->
    E;
store_entry({ok, Entry}) ->
    case instathread_db_client:put(Entry) of
	ok ->
	    {ok, Entry};
	Error ->
	    Error
    end.

form_to_entry(E={error, _}) ->
    E;
form_to_entry({ok, FormData}) ->
    Author = proplists:get_value(<<"author">>, FormData, <<"anonymous">>),
    Body   = proplists:get_value(<<"body">>, FormData),
    {ok, instathread_db_entry:new(Author, Body)}.

form_to_entry(_, E={error, _}) ->
    E;
form_to_entry(RootKey, {ok, FormData}) ->
    Author = proplists:get_value(<<"author">>, FormData, <<"anonymous">>),
    Body   = proplists:get_value(<<"body">>, FormData),
    {ok, instathread_db_entry:new(RootKey, RootKey, Author, Body)}.


validate(E={error, _}) ->
    E;
validate({ok, FormData, _}) ->
    case proplists:get_value(<<"body">>, FormData) of
	undefined ->
	    {error, no_body};
	<<>> ->
	    {error, no_body};
	_ ->
	    {ok, FormData}
    end.
