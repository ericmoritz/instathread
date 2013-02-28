%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% The threads POST handler
%%% @end
%%% Created : 27 Feb 2013 by Eric Moritz <eric@eric-acer>

-module(instathread_rest_threads_handler).

-record(state, {data, entry, client}).

% Cowboy handler callbacks
-export([init/3]).

% Cowboy REST callbacks
-export([rest_init/2, service_available/2, allowed_methods/2, post_is_create/2, content_types_accepted/2,
	 content_types_provided/2, plain_text/2, create_entry/2, created_path/2]).


init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.


rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.


service_available(Req, State) ->
    case instathread_db:start_link() of
	{ok, Client} ->
	    {true, Req, State#state{client=Client}};
	{error, _} ->
	    {false, Req, State}
    end.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"text">>, <<"plain">>, []}, plain_text}
      ], Req, State}.

plain_text(Req, State) ->
    {true, Req, State}.

content_types_accepted(Req, State) ->
    {[
     {{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_entry}
    ], Req, State}.


post_is_create(Req, State) ->
    {true, Req, State}.

create_entry(Req, State=#state{client=Client}) ->
    BodyResult = cowboy_req:body_qs(Req),
    case instathread_rest_entry_form:create_entry(Client, BodyResult) of
	{ok, Entry} ->
	    % if create_entry returns ok then BodyResult and
	    % EntryResult are not errors so it is safe to unpack them.
	    {ok, _, Req2} = BodyResult,
	    {true, Req2, State#state{entry=Entry}};
	% no_body is the only error we can handle ourselves,
	% other errors from store_entry are backend errors
	% and should 500
	{error, no_body} ->
	    % {error, no_body} means we got valid form data but it was
	    % missing the body field. It is safe to unpack BodyResult
	    % here.
	    {ok, _, Req2} = BodyResult,
	    {false, Req2, State}
    end.

created_path(Req, State=#state{entry=Entry}) ->
    {instathread_rest_urls:thread(Entry), Req,State}.

