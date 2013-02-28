%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% The thread handler
%%% @end
%%% Created : 27 Feb 2013 by Eric Moritz <eric@eric-acer>

-module(instathread_rest_thread_handler).

-record(state, {root_key, nodes, entry}).

% Cowboy handler callbacks
-export([init/3]).

% Cowboy REST callbacks
-export([rest_init/2, allowed_methods/2]). 

% GET callbacks
-export([resource_exists/2, content_types_provided/2, to_html/2]).

% POST callbacks
-export([post_is_create/2, content_types_accepted/2, create_reply/2, created_path/2]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.


rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

resource_exists(Req, State) ->
    {RootKey, Req2} = cowboy_req:binding(root_key, Req),
    case instathread_db_client:nodes(RootKey) of
	{error, notfound} ->
	    {false, Req2, State};
	{ok, Nodes} ->
	    {true, Req2, State#state{root_key=RootKey, nodes=Nodes}}
    end.

content_types_provided(Req, State) ->
    {[
      {{<<"text">>, <<"html">>, []}, to_html}
      ], Req, State}.

to_html(Req, State=#state{root_key=_RootKey, nodes=Nodes}) ->
    Data = [
	    {<<"nodes">>, nodes_data(Nodes)}
	   ],
    HTML = instathread_rest_templates:thread(Data),
    {HTML, Req, State}.


post_is_create(Req, State) ->
    {true, Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_reply}
     ], Req, State}.

create_reply(Req, State=#state{root_key=RootKey}) ->
    BodyResult = cowboy_req:body_qs(Req),
    case instathread_rest_entry_form:create_entry(RootKey, BodyResult) of
	{ok, Entry} ->
	    {ok, _, Req2} = BodyResult,
	    {true, Req2, State#state{entry=Entry}};
	{error, no_body} ->
	    {ok, _, Req2} = BodyResult,
	    {false, Req2, State}
    end.


created_path(Req, State=#state{entry=Entry}) ->
    {instathread_rest_urls:thread(Entry), Req,State}.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
nodes_data(Nodes) ->
    [node_data(N) || N <- Nodes].

node_data(Node) ->
    [
     {<<"author">>, instathread_db_entry:author(Node)},
     {<<"body">>, instathread_db_entry:body(Node)}
    ].
