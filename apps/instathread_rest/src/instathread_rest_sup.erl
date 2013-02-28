
-module(instathread_rest_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% Compile the templates
    erlydtl:compile_dir(template_dir(), instathread_rest_templates),

    Port = 8000,

    Dispatch = cowboy_router:compile([
		{'_', instathread_rest_urls:routes()}
	       ]),
    
    CowboySpec = ranch:child_spec(instathread_rest, 100, 
				  ranch_tcp, [{port, Port}],
				  cowboy_protocol, [{env, [{dispatch, Dispatch}]}]),

    {ok, { {one_for_one, 1000, 3600}, [CowboySpec]} }.


template_dir() ->
    filename:join([
		   apptools:priv_dir(instathread_rest),
		   "templates"
		  ]).
