-module(instathread_db_client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-record(state, {client}).

-export([
	 start_link/2,
	 nodes/2,
	 put/2
	]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


start_link(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).


nodes(Pid, RootKey) ->
    gen_server:call(Pid, {nodes, RootKey}).


put(Pid, Entry) ->
    gen_server:call(Pid, {put, Entry}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Host, Port]) ->
    {ok, RiakClient} = riakc_pb_socket:start_link(Host, Port),
    {ok, #state{client=RiakClient}}.

handle_call({nodes, RootKey}, _From, State=#state{client=Client}) ->
    {reply, nodes_internal(Client, RootKey), State};
handle_call({put, Entry}, _From, State=#state{client=Client}) ->
    {reply, put_internal(Client, Entry), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec nodes(pid(), binary()) -> {ok, [term()]} | {error, any()}.
nodes_internal(Client, RootKey) ->
    {ok, [{1, Results}]} = riakc_pb_socket:mapred(
			     Client,
			     {index, <<"entries">>, {binary_index, "root"}, RootKey},
			     [{map, {modfun, riak_kv_mapreduce, map_object_value}, undefined, false},
			      {reduce, {modfun, riak_kv_mapreduce, reduce_set_union}, undefined, true}]
			    ),
    % TODO: Use binary_to_term map phase
    Nodes = [binary_to_term(X) || X <- Results],
    {ok, Nodes}.

-spec put(pid(), term()) -> ok | {error, any()}.
put_internal(Client, Entry) ->
    Obj = riakc_obj:new(
	    <<"entries">>,
	    instathread_db_entry:key(Entry),
	    term_to_binary(Entry)
	   ),
    MD = riakc_obj:set_secondary_index(
	   dict:new(),
	   {{binary_index, "root"}, [instathread_db_entry:root_key(Entry)]}
	  ),
    Obj1 = riakc_obj:update_metadata(Obj, MD),
    riakc_pb_socket:put(Client, Obj1).


