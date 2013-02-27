-module(instathread_db_entry).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-record(entry_vsn1, {properties}).
-type entry() :: #entry_vsn1{}.
-type timestamp() :: {integer(), integer(), integer()}.
-export([
    % constructor
    new/2, new/4,

    % loader
    load/1,

    % setters
    set_author/2, set_body/2,

    % getters
    root_key/1, key/1, parent_key/1, creation_date/1, author/1, body/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Creates a root entry
%%--------------------------------------------------------------------
-spec new(binary(), binary()) -> entry().
new(Author, Body) ->
    new(undefined, <<"">>, Author, Body).

%%--------------------------------------------------------------------
%% @doc Creates a child entry
%%--------------------------------------------------------------------
-spec new(binary(), binary(), binary(), binary()) -> entry().
new(RootKey, ParentKey, Author, Body) ->
    Timestamp = erlang:now(),

    EntryKey = entry_key(
        Timestamp,		 
	erlang:make_ref()
    ),

    entry(
      RootKey,
      ParentKey,
      EntryKey,
      Timestamp,
      Author,
      Body
    ).
      

%%--------------------------------------------------------------------
%% @doc Update a given entry to the current version
%% This should be called after an entry is deserialized to ensure
%% that the schema has been migrated
%% @end
%%--------------------------------------------------------------------
-spec load(term()) -> entry().
load(Entry=#entry_vsn1{}) ->
    Entry.

%%--------------------------------------------------------------------
%% @doc Return the entry's key
%%--------------------------------------------------------------------
-spec key(entry()) -> binary().
key(#entry_vsn1{properties=Props}) ->
    proplists:get_value(key, Props).

%%--------------------------------------------------------------------
%% @doc Return the entry's root key
%%--------------------------------------------------------------------
-spec root_key(entry()) -> binary().
root_key(#entry_vsn1{properties=Props}) ->
    proplists:get_value(root, Props).

%%--------------------------------------------------------------------
%% @doc Return the entry's parent key
%%--------------------------------------------------------------------
-spec parent_key(entry()) -> binary() | undefined.
parent_key(#entry_vsn1{properties=Props}) ->
    proplists:get_value(parent, Props).

%%--------------------------------------------------------------------
%% @doc Return the entry's creation date
%% The creation data is an ISO8601 formatted string
%% @end
%%--------------------------------------------------------------------
-spec creation_date(entry()) -> binary().
creation_date(#entry_vsn1{properties=Props}) ->
    iso8601:format(
      proplists:get_value(timestamp, Props)
    ).

%%--------------------------------------------------------------------
%% @doc Return the entry's author
%%--------------------------------------------------------------------
-spec author(entry()) -> binary().
author(#entry_vsn1{properties=Props}) ->
    proplists:get_value(author, Props).

%%--------------------------------------------------------------------
%% @doc Return the entry's body
%%--------------------------------------------------------------------
-spec body(entry()) -> binary().
body(#entry_vsn1{properties=Props}) ->
    proplists:get_value(body, Props).

%%--------------------------------------------------------------------
%% @doc Set the author
%%--------------------------------------------------------------------
-spec set_author(entry(), binary()) -> entry().
set_author(Author, Entry) ->
    update(Entry, author, Author).

%%--------------------------------------------------------------------
%% @doc Set the body
%%--------------------------------------------------------------------
-spec set_body(entry(), binary()) -> entry().
set_body(Body, Entry) ->
    update(Entry, body, Body).


%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------
-spec update(entry(), any(), any()) -> entry().
update(Entry=#entry_vsn1{properties=Props}, Key, Value) ->
    Props1 = [{Key, Value}|proplists:delete(Key, Props)],
    Entry#entry_vsn1{properties=Props1}.

-spec entry_key(timestamp(), term()) -> binary().
entry_key(Timestamp, Ref) ->
    iolist_to_binary([
		      iso8601:format(Timestamp),
		      ":",
		      base64:encode(term_to_binary(Ref))
		     ]).

-spec entry(binary(), binary(), timestamp(), term(), binary(), binary()) -> entry().
entry(undefined, ParentKey, Key, Timestamp, Author, Body) ->
    % entries with undefined roots are roots so the root key is set to the entry's key
    entry(Key, ParentKey, Key, Timestamp, Author, Body);
entry(RootKey, ParentKey, Key, Timestamp, Author, Body) ->
    #entry_vsn1{
        properties=[
	    {root, RootKey},
            {parent, ParentKey},
            {key, Key},
            {timestamp, Timestamp},
	    {author, Author},
            {body, Body}
       ]
   }.


-ifdef(TEST).

fixture() ->
    entry(
      <<"root-key">>,
      <<"parent-key">>,
      <<"key">>,
      {1360,123602,691827},
      <<"author">>,
      <<"body">>
    ).


root_key_test() ->
    ?assertEqual(
       <<"root-key">>,
       root_key(fixture())
    ).

key_test() ->
    ?assertEqual(
       <<"key">>,
       key(fixture())
    ).

parent_key_test() ->
    ?assertEqual(
       <<"parent-key">>,
       parent_key(fixture())
    ).

creation_date_test() ->
    ?assertEqual(
       <<"2013-02-06T04:06:42Z">>,
       creation_date(fixture())
    ).

author_test() ->
    ?assertEqual(
       <<"author">>,
       author(fixture())
    ).

body_test() ->
    ?assertEqual(
       <<"body">>,
       body(fixture())
    ).


set_author_test() ->
    Entry1 = set_author(
	       <<"author-1">>,
	       fixture()),

    ?assertEqual(
       <<"author-1">>,
       author(Entry1)).
    
set_body_test() ->
    Entry1 = set_body(
	       <<"body-1">>,
	       fixture()),

    ?assertEqual(
       <<"body-1">>,
       body(Entry1)).
    
entry_test() ->
    ?assertEqual(
       entry(undefined, <<"">>, <<"foo">>, {0,0,0},  <<"eric">>, <<"test">>),
       #entry_vsn1{
	      properties=[
			  {root, <<"foo">>},
			  {parent, <<"">>},
			  {key, <<"foo">>},
			  {timestamp, {0,0,0}},
			  {author, <<"eric">>},
			  {body, <<"test">>}
			  ]
	     }
      ),
    ?assertEqual(
       entry(<<"root-key">>, <<"">>, <<"foo">>, {0,0,0},  <<"eric">>, <<"test">>),
       #entry_vsn1{
	      properties=[
			  {root, <<"root-key">>},
			  {parent, <<"">>},
			  {key, <<"foo">>},
			  {timestamp, {0,0,0}},
			  {author, <<"eric">>},
			  {body, <<"test">>}
			  ]
	     }
       ).    

-endif.
