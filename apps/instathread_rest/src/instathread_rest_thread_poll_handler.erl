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
    subscribe(RootKey),
    {loop, Req3, #state{root_key=RootKey, since=Since}, 60000, hibernate}.

info({thread_update, Nodes}, Req, State = #state{root_key=RootKey, since=Since}) ->
    Data = data(RootKey, Nodes, Since),
    HTML = instathread_rest_templates:thread(Data),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], HTML, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.


%%%%
%% Internal
%%%%
subscribe(RootKey) ->
    instathread_subscriptions:subscribe(RootKey).


data(RootKey, Nodes, undefined) ->
    instathread_rest_thread_data:new(RootKey, Nodes);
data(RootKey, Nodes, Since) ->
    instathread_rest_thread_data:new(RootKey, Nodes, Since).

