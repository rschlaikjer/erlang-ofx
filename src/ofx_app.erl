-module(ofx_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    initialize_leex(),
    ofx_sup:start_link().

% Compile and load the leex lexer for OFX documents
initialize_leex() ->
    PrivDir = code:priv_dir(ofx),
    {ok, CodeFile} = leex:file(filename:join([PrivDir, "ofx_leex.xrl"])),
    {ok, ofx_leex} = compile:file(CodeFile).


stop(_State) ->
    ok.
