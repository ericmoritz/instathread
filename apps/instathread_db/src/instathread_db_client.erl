-module(instathread_db_client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-record(state, {client}).

-export([
	 start_link/1,
	 start_link/2,
	 nodes/1,
	 put/1
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

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).    

-spec nodes(binary()) -> {ok, [term()]} | {error, notfound} | {error, any()}.
nodes(RootKey) ->
    poolboy:transaction(?MODULE, 
                        fun(Pid) ->
                                gen_server:call(Pid, {nodes, RootKey})
                        end).

put(Entry) ->
    poolboy:transaction(?MODULE, 
                        fun(Pid) ->
                                gen_server:call(Pid, {put, Entry})
                        end).

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
-spec nodes_internal(pid(), binary()) -> {ok, [term()]} | {error, notfound} | {error, any()}.
nodes_internal(Client, RootKey) ->
    case riakc_pb_socket:get(Client, <<"entries">>, RootKey) of
	E={error, _} ->
	    E;
	{ok, Obj} ->
	    NodeSet = node_set(riakc_obj:get_values(Obj)),
	    NodeList = sets:to_list(NodeSet),
	    SortedNodeList = lists:sort(
			       fun(E1, E2) ->
				       instathread_db_entry:key(E1) =< instathread_db_entry:key(E2)
			       end,
			       NodeList
			      ),
	    {ok, SortedNodeList}
    end.

-spec put_internal(pid(), term()) -> ok | {error, any()}.
put_internal(Client, Entry) ->
    RootKey = instathread_db_entry:root_key(Entry),
    {ok, Obj} = get_or_new(Client, RootKey),
    NodeSet = node_set(riakc_obj:get_values(Obj)),
    NodeSet2 = sets:add_element(Entry, NodeSet),
    Obj2 = riakc_obj:update_value(Obj, term_to_binary(NodeSet2)),
    RiakResult = riakc_pb_socket:put(Client, Obj2),
    send_onchange(RootKey, RiakResult).
       
% send the onchange message to subscribers
% if riak's put was not an error
send_onchange(_, E={error, _}) ->
    E;
send_onchange(RootKey, R) ->
    instathread_subscriptions:onchange(RootKey),
    R.

get_or_new(Client, RootKey) ->
    get_or_new(
      <<"entries">>,
      RootKey, 
      riakc_pb_socket:get(Client, <<"entries">>, RootKey)
    ).

get_or_new(_, _, {ok, Obj}) ->
    {ok, Obj};
get_or_new(Bucket, Key, {error, notfound}) ->
    {ok, riakc_obj:new(Bucket, Key)};
get_or_new(_,_, Other) ->
    Other.

node_set(Siblings) ->
    sets:union(
      [binary_to_term(X) || X <- Siblings]
    ).
