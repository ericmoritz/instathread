%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% Simple tools for OTP application
%%% @end
%%% Created : 27 Feb 2013 by Eric Moritz <eric@eric-acer>

-module(apptools).

-export([ensure_started/2, priv_dir/1]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the app and its dependencies
%% @end
%%--------------------------------------------------------------------
-spec ensure_started(atom(), application:restart_type()) -> ok | {error, term()}.
ensure_started(App, Type) ->
    case application:start(App) of
	{error, {not_started, Dep}} ->
	    ensure_started(Dep, Type),
	    ensure_started(App, Type);
	{error, {already_started,_}} ->
	    ok;
	E={error, _} ->
	    E;
	ok ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Locates the priv dir of an app
%% @end
%%--------------------------------------------------------------------
-spec priv_dir(atom()) -> string().
priv_dir(App) ->
    case code:priv_dir(App) of
	{error, bad_name} -> priv_dir_mod(App);
	Dir -> Dir
    end.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
priv_dir_mod(Mod) ->
    case code:which(Mod) of
	File when not is_list(File) -> "../priv";
	File -> filename:join([filename:dirname(File), "..priv"])
    end.
	    
	    
