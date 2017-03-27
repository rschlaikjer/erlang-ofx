-module(ofxconnect_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    initialize_leex(),
    ofxconnect_sup:start_link().

% Compile and load the leex lexer for OFX documents
initialize_leex() ->
    PrivDir = code:priv_dir(ofxconnect),
    {ok, CodeFile} = leex:file(filename:join([PrivDir, "ofxconnect_leex.xrl"])),
    {ok, ofxconnect_leex} = compile:file(CodeFile).


stop(_State) ->
    ok.
