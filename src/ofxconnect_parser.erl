-module(ofxconnect_parser).
-include("include/records.hrl").
-compile(export_all).

marshal(#ofx_node{name=Name, children=Children}) ->
    [
     "<", Name, ">",
     [marshal(Child) || Child <- Children],
     "</", Name, ">"
    ];
marshal(#ofx_leaf{name=Name, value=Value}) ->
    ["<", Name, ">", Value].

lex(Ofx) ->
    leex:file('ofx.xrl'),
    compile:file("ofx.erl"),
    {ok, Tags, _} = ofx:string(Ofx),
    Tags.

parse(Tags) ->
    do_parse(Tags).

do_parse([{opentag, Tag}|[{string, Value}|Tags]]) ->
    #ofx_leaf{name=Tag, value=Value};
do_parse([{opentag, Tag}|[{opentag, ChildTag}|Tags]]) ->
    #ofx_node{
       name=Tag,
       children=[do_parse([{opentag, ChildTag}|Tags])]
    }.
