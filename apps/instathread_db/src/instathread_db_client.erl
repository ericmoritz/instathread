% -*- erlang -*-
-module(instathread_db_client).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	 nodes/1,
	 put/1
	]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec nodes(binary()) -> {ok, [term()]} | {error, notfound} | {error, any()}.
nodes(RootKey) ->
    poolboy:transaction(?MODULE, 
                        fun(Pid) ->
                                gen_server:call(Pid, {nodes, RootKey})
                        end).

put(Entry) ->
    R = poolboy:transaction(?MODULE, 
                            fun(Pid) ->
                                    gen_server:call(Pid, {put, Entry})
                            end),
    RootKey = instathread_db_entry:root_key(Entry),
    send_onchange(RootKey, R).

%%%%
%% Internal
%%%%

% send the onchange message to subscribers
% if riak's put was not an error
send_onchange(_, E={error, _}) ->
    E;
send_onchange(RootKey, R) ->
    instathread_subscriptions:onchange(RootKey),
    R.



