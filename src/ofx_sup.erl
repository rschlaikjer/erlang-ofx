-module(ofx_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    new_client/5
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_client(Username, Password, Org, Fid, Url) ->
    supervisor:start_child(?SERVER, [Username, Password, Org, Fid, Url]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {simple_one_for_one, 10, 10}, [child_spec()]} }.

%%====================================================================
%% Internal functions
%%====================================================================

child_spec() -> {
    ofx_client,
    {ofx_client, start_link, []},
    transient,
    3000,
    worker,
    [ofx_client]
}.
