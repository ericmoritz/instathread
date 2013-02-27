#! /usr/bin/env escript
%% -*- erlang -*s
%%! -smp enable -pa ebin deps/riakc/ebin deps/protobuffs/ebin deps/iso8601/ebin deps/riak_pb/ebin

main([]) ->
    {ok, Client} = instathread_db_client:start_link("127.0.0.1", 10017),

    Entry = instathread_db_entry:new(<<"eric">>, <<"first">>),
    RootKey = instathread_db_entry:root_key(Entry),
    EntryKey = instathread_db_entry:key(Entry),

    ok = instathread_db_client:put(
	   Client,
	   Entry
	  ),

    [instathread_db_client:put(Client, instathread_db_entry:new(RootKey, EntryKey, <<"eric">>, <<"second">>)) || _ <- lists:seq(1, 2000)],

    {TS, {ok, Nodes}} = timer:tc(fun() -> 
					 instathread_db_client:nodes(
					   Client,
					   RootKey
					  )
				 end),
    io:format("~p ms~n", [TS / 1000]).
	
