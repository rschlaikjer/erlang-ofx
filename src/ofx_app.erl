-module(ofx_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    initialize_leex(),
    ofx_sup:start_link().

% Compile and load the leex lexer for OFX documents
initialize_leex() ->
    ok.

stop(_State) ->
    ok.
