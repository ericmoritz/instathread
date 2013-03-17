% -*- erlang -*-
-module(instathread_db_redis_client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-record(state, {client}).

-export([
	 start_link/1,
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

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).    

-spec nodes(pid(), binary()) -> {ok, [term()]} | {error, notfound} | {error, any()}.
nodes(Pid, RootKey) ->
    gen_server:call(Pid, {nodes, RootKey}).

put(Pid, Entry) ->
    gen_server:call(Pid, {put, Entry}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Client} = erlang:apply(eredis, start_link, Args),
    {ok, #state{client=Client}}.

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
    SetKey = redis_set_key(RootKey),
    SetMembers = eredis:q(Client, [<<"ZRANGE">>, SetKey, <<"0">>, <<"-1">>]),
    {ok, deserialze(SetMembers)}.

-spec redis_set_key(binary()) -> binary().
redis_set_key(RootKey) ->
    iolist_to_binary([<<"entries:">>, RootKey]).

-spec deserialze(binary())      -> {ok, instathread_db_entry:entry()};
                (nil)             -> {error, notfound};
                ({error, Reason}) -> {error, Reason}.
deserialze(E={error, _}) -> E;
deserialze({ok, []}) -> {error, notfound};
deserialze({ok, EntryBins}) ->
    lists:map(fun erlang:binary_to_term/1, EntryBins).

-spec serialize(instathread_db_entry:entry()) -> binary().
serialize(Entry) ->
    term_to_binary(Entry).

-spec put_internal(pid(), term()) -> ok | {error, any()}.
put_internal(Client, Entry) ->
    RootKey = instathread_db_entry:root_key(Entry),
    SetKey = redis_set_key(RootKey),
    case eredis:q(Client, [<<"ZADD">>, SetKey, <<"0.0">>, serialize(Entry)]) of
        {ok, _} ->
            ok;
        E={error, _} ->
            E
    end.

