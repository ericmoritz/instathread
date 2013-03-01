-module(instathread_subscriptions).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0, start_link/0, subscribe/1, onchange/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start() ->
    apptools:ensure_started(instathread_subscriptions, permanant).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

subscribe(ThreadKey) ->
    % TODO: replace gproc with a multi-node pub/sub mechanism
    gproc:reg({p, l, {?MODULE, ThreadKey}}).

onchange(ThreadKey) ->
    gen_server:call(?SERVER, {onchange, ThreadKey}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({onchange, ThreadKey}, _From, State) ->
    handle_onchange(ThreadKey),
    {reply, ok, State};
handle_call(Msg, _From, State) ->
    {reply, {error, {unknown_msg, Msg}}, State}.

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
handle_onchange(ThreadKey) ->
    % fetch the latest nodes
    case instathread_db_client:nodes(ThreadKey) of 
	{ok, Nodes} ->
	    % broadcast the new nodes to the subscribers
	    gproc:send({p, l, {?MODULE, ThreadKey}},
		       {thread_update, Nodes});
	_ ->
	    pass % throwaway any unknown results
    end,
    ok.
