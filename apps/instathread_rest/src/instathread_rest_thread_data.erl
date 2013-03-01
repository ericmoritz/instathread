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
     instathread_db_entry:entry(<<"foo">>, <<"">>, <<"foo">>, {1362,105368,68515}, <<"eric">>, <<"first">>),
     instathread_db_entry:entry(<<"foo">>, <<"foo">>, <<"bar">>, {1362,106930,113671}, <<"eric">>, <<"secord">>)
    ].
-endif.


new(RootKey, Entries) ->
    new(RootKey, Entries, "").

-ifdef(TEST).
new_test() ->
    RootKey = <<"foo">>,
    Entries = [_,Entry2] = fixture(),
    Since = <<"2013-03-01T02:36:09Z">>,
    Since2 = <<"2013-03-02T02:36:09Z">>,
    

    ?assertEqual(
       [
	{poll_url, <<"/v1/threads/foo/poll/2013-03-01T03%3a02%3a10Z">>},
	{nodes, [entry_data(Entry2)]}
       ],
      new(RootKey, Entries, Since)
      ),

    ?assertEqual(
       [
	{poll_url, <<"/v1/threads/foo/poll">>},
	{nodes, []}
       ],
      new(RootKey, Entries, Since2)
      ),
    ok.
    
-endif.
new(RootKey, Entries, SinceISO) ->
    State = lists:foldr(fun reducer/2,
		       #state{since=SinceISO},
		       Entries),
    data(RootKey, State).


-ifdef(TEST).
reducer_test() ->
    State = #state{since = <<"2013-03-01T02:36:09Z">>},
    [Entry1, Entry2] = fixture(),
    ?assertEqual(
       State#state{last=undefined, entries=[]},
       reducer(Entry1, State)
      ),

    ?assertEqual(
       State#state{last=Entry2, entries=[entry_data(Entry2)]},
       reducer(Entry2, State)
      ),
    
    ?assertEqual(
       State#state{last=Entry2, entries=[entry_data(Entry2)]},
       reducer(Entry2, State#state{last=Entry1})
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

creation_date(Entry) ->
    instathread_db_entry:creation_date(Entry).

timestamp(Entry) ->
    instathread_db_entry:timestamp(Entry).

-ifdef(TEST).
keep_entry_test() ->
    [Entry1, Entry2] = fixture(),

    ?assertEqual(
       false,
       keep_entry(Entry1, <<"2013-03-01T02:36:09Z">>)
      ),
    
    ?assertEqual(
       true,
       keep_entry(Entry2, <<"2013-03-01T02:36:09Z">>)
      ).

-endif.
keep_entry(Entry, Since) ->
    creation_date(Entry) >= Since.

-ifdef(TEST).
older_entry_test() ->
    [Entry1, Entry2] = fixture(),
    ?assertEqual(
       Entry2,
       older_entry(Entry1, Entry2)
      ),

    ?assertEqual(
       Entry2,
       older_entry(Entry2, Entry1)
      ),
    
    ?assertEqual(
       Entry1,
       older_entry(undefined, Entry1)
      ),
    
    ?assertEqual(
       Entry2,
       older_entry(Entry2, undefined)
      ).

    
-endif.
older_entry(undefined, Entry) ->
    Entry;
older_entry(Entry, undefined) ->
    Entry;
older_entry(Entry1, Entry2) ->
    TS1 = timestamp(Entry1),
    TS2 = timestamp(Entry2),
    if TS1 > TS2 ->
	    Entry1;
       true ->
	    Entry2
    end.
-ifdef(TEST).
poll_url_test() ->
    [Entry|_] = fixture(),

    ?assertEqual(
       <<"/v1/threads/foo/poll">>,
       poll_url(<<"foo">>, undefined)
      ),
    ?assertEqual(
       <<"/v1/threads/foo/poll/2013-03-01T02%3a36%3a08Z">>,
       poll_url(<<"foo">>, Entry)
      ),
    ?assertEqual(
       <<"/v1/threads/foo/poll">>,
       poll_url(<<"foo">>, undefined)
      ).    
-endif.
poll_url(RootKey, undefined) ->
    instathread_rest_urls:thread_poll(RootKey);
poll_url(_, Entry) ->
    instathread_rest_urls:thread_poll(Entry).

entry_data(Entry) ->
    [
     {<<"author">>, instathread_db_entry:author(Entry)},
     {<<"body">>, instathread_db_entry:body(Entry)}
    ].

     
data(RootKey, #state{last=LastEntry, entries=Entries}) ->
    [
     {poll_url, poll_url(RootKey, LastEntry)},
     {nodes, Entries}
    ].

