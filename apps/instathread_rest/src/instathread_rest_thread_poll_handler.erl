%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% The long poll thread handler
%%% @end
%%% Created : 28 Feb 2013 by Eric Moritz <eric@eric-acer>

-module(instathread_rest_thread_poll_handler).
-behaviour(cowboy_loop_handler).
-record(state, {root_key, since}).
-export([init/3, info/3, terminate/3]).

init({tcp, http}, Req, _Opts) ->
    {RootKey, Req2} = cowboy_req:binding(root_key, Req),
    {Since, Req3} = cowboy_req:binding(since, Req2),
    State = #state{root_key=RootKey, since=Since},

    % Fetch the current thread
    Thread = instathread_db_client:nodes(RootKey),

    % Get the resource data for the thread
    Data = data(RootKey, Thread, Since),

    % Detect if the resource data has comments since the date given
    HasComments = has_comments(Data),

    % Generate the approproate response
    data_response(HasComments, RootKey, Data, Req3, State).
    
info({thread_update, Nodes}, Req, State = #state{root_key=RootKey, since=Since}) ->
    Data = data(RootKey, Nodes, Since),
    data_response(true, RootKey, Data, Req, State).

terminate(_Reason, _Req, _State) ->
    ok.


%%%%
%% Internal
%%%%
%%--------------------------------------------------------------------
%% @doc
%% Detects if the resource data has comments
%% @end
%%--------------------------------------------------------------------
has_comments(E={error,_}) ->
    E;
has_comments(Data) ->
    proplists:get_value(nodes, Data) =/= [].

%%--------------------------------------------------------------------
%% @doc
%% Generate the resource data
%% @end
%%--------------------------------------------------------------------
data(_RootKey, E={error, _}, _) ->
    % pass through any errors, they'll be handled by data_response/5
    E;
data(RootKey, {ok, Nodes}, Since) ->
    % reduce {ok, Nodes} values to just Nodes,
    % {ok, Nodes} comes from init/3 
    data(RootKey, Nodes, Since);
data(RootKey, Nodes, undefined) ->
    instathread_rest_thread_data:new(RootKey, Nodes);
data(RootKey, Nodes, Since) ->
    instathread_rest_thread_data:new(RootKey, Nodes, Since).

%%--------------------------------------------------------------------
%% @doc
%% Generate the appropriate response
%% @end
%%--------------------------------------------------------------------
data_response({error, notfound}, _RootKey, _Data, Req, State) ->
    % return a 404 if has_comments/1 passed through the {error, notfound} result
    {ok, Req2} = cowboy_req:reply(404, [{<<"content-type">>, <<"text/plain">>}], "not found", Req),
    {ok, Req2, State};
data_response(true, _RootKey, Data, Req, State) ->
    HTML = instathread_rest_templates:thread(Data),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], HTML, Req),
    {ok, Req2, State};
data_response(false, RootKey, _Data, Req, State) ->
    subscribe(RootKey),
    {loop, Req, State, 60000, hibernate}.

subscribe(RootKey) ->
    instathread_subscriptions:subscribe(RootKey).



