%%% @author Eric Moritz <eric@eric-acer>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% The data for a thread template
%%% @end
%%% Created : 28 Feb 2013 by Eric Moritz <eric@eric-acer>

-module(instathread_rest_thread_data).

-export([new/2, new/3]).

-record(state, {since, last, entries=[]}).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
fixture() ->
    [
     instathread_db_entry:entry(<<"100">>, <<"">>, <<"100">>, {1362,105368,68515}, <<"eric">>, <<"first">>),
     instathread_db_entry:entry(<<"100">>, <<"100">>, <<"101">>, {1362,106930,113671}, <<"eric">>, <<"secord">>)
    ].
-endif.


new(RootKey, Entries) ->
    new(RootKey, Entries, "").

-ifdef(TEST).
new_test() ->
    RootKey = <<"100">>,
    Entries = [_,EntryB] = fixture(),
    Since = <<"100">>,
    Since2 = <<"101">>,
    

    ?assertEqual(
       [
	{rootkey, <<"100">>},
	{thread_url, <<"/service/v1/threads/100">>},
	{poll_url, <<"/service/v1/threads/100/poll/101">>},
	{nodes, [entry_data(EntryB)]}
       ],
      new(RootKey, Entries, Since)
      ),

    ?assertEqual(
       [
	{rootkey, <<"100">>},
	{thread_url, <<"/service/v1/threads/100">>},
	{poll_url, <<"/service/v1/threads/100/poll">>},
	{nodes, []}
       ],
      new(RootKey, Entries, Since2)
      ),
    ok.
    
-endif.
new(RootKey, Entries, SinceKey) ->
    State = lists:foldr(fun reducer/2,
		       #state{since=SinceKey},
		       Entries),
    data(RootKey, State).


-ifdef(TEST).
reducer_test() ->
    State = #state{since = <<"100">>},
    [EntryA, EntryB] = fixture(),
    ?assertEqual(
       State#state{last=undefined, entries=[]},
       reducer(EntryA, State)
      ),

    ?assertEqual(
       State#state{last=EntryB, entries=[entry_data(EntryB)]},
       reducer(EntryB, State)
      ),
    
    ?assertEqual(
       State#state{last=EntryB, entries=[entry_data(EntryB)]},
       reducer(EntryB, State#state{last=EntryA})
      ).
    

-endif.
reducer(Entry, State) ->
    case keep_entry(Entry, State#state.since) of
	true ->
	    LastEntry = older_entry(State#state.last, Entry),
	    EntryData = entry_data(Entry),
	    Entries = [EntryData|State#state.entries],
	    State#state{last=LastEntry, entries=Entries};
	false ->
	    State
    end.

key(Entry) ->
    instathread_db_entry:key(Entry).

-ifdef(TEST).
keep_entry_test() ->
    [EntryA, EntryB] = fixture(),

    ?assertEqual(
       false,
       keep_entry(EntryA, <<"100">>)
      ),
    
    ?assertEqual(
       true,
       keep_entry(EntryB, <<"100">>)
      ).

-endif.
keep_entry(Entry, Since) ->
    key(Entry) > Since.

-ifdef(TEST).
older_entry_test() ->
    [EntryA, EntryB] = fixture(),
    ?assertEqual(
       EntryB,
       older_entry(EntryA, EntryB)
      ),

    ?assertEqual(
       EntryB,
       older_entry(EntryB, EntryA)
      ),
    
    ?assertEqual(
       EntryA,
       older_entry(undefined, EntryA)
      ),
    
    ?assertEqual(
       EntryB,
       older_entry(EntryB, undefined)
      ).

    
-endif.
older_entry(undefined, Entry) ->
    Entry;
older_entry(Entry, undefined) ->
    Entry;
older_entry(EntryA, EntryB) ->
    KeyA = key(EntryA),
    KeyB = key(EntryB),
    if KeyA > KeyB ->
	    EntryA;
       true ->
	    EntryB
    end.

-ifdef(TEST).
poll_url_test() ->
    [_|[Entry]] = fixture(),

    ?assertEqual(
       <<"/service/v1/threads/100/poll">>,
       poll_url(<<"100">>, undefined)
      ),
    ?assertEqual(
       <<"/service/v1/threads/100/poll/101">>,
       poll_url('_', Entry)
      ).
-endif.
poll_url(RootKey, undefined) ->
    instathread_rest_urls:thread_poll(RootKey);
poll_url(_, Entry) ->
    instathread_rest_urls:thread_poll(Entry).

thread_url(RootKey) ->
    instathread_rest_urls:thread(RootKey).    

entry_data(Entry) ->
    [
     {<<"author">>, instathread_db_entry:author(Entry)},
     {<<"body">>, instathread_db_entry:body(Entry)}
    ].

     
data(RootKey, #state{last=LastEntry, entries=Entries}) ->
    [
     {rootkey, RootKey},
     {thread_url, thread_url(RootKey)},
     {poll_url, poll_url(RootKey, LastEntry)},
     {nodes, Entries}
    ].

