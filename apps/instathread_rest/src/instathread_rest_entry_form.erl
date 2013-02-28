%%% @author Eric Moritz <eric@eric-acer>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%%
%%% @end
%%% Created : 28 Feb 2013 by Eric Moritz <eric@eric-acer>

-module(instathread_rest_entry_form).

-export([create_entry/2, create_entry/3]).

create_entry(Client, BodyResult) ->
    DataResult = validate(BodyResult),
    EntryResult = form_to_entry(DataResult),
    store_entry(Client, EntryResult).

create_entry(Client, RootKey, BodyResult) ->
    DataResult = validate(BodyResult),
    EntryResult = form_to_entry(RootKey, DataResult),
    store_entry(Client, EntryResult).

store_entry(_, E={error, _}) ->
    E;
store_entry(Client, {ok, Entry}) ->
    case instathread_db_client:put(Client, Entry) of
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
