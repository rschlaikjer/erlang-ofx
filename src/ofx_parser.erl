-module(ofx_parser).
-include_lib("ofx/include/records.hrl").
-export([
    marshal/1,
    unmarshal/1
]).

%%
%% Public methods
%%

% Dump an OFX tree out to a deep IO list
marshal(#ofx_node{name=Name, children=Children}) ->
    [
     "<", Name, ">",
     [marshal(Child) || Child <- Children],
     "</", Name, ">"
    ];
marshal(#ofx_leaf{name=Name, value=Value}) ->
    ["<", Name, ">", Value].

% Read a string into an OFX tree
unmarshal(Text) when is_binary(Text) ->
    unmarshal(binary_to_list(Text));
unmarshal(Text) ->
    case lex(Text) of
        {ok, Tags} -> parse(Tags);
        {error, Reason} -> {error, Reason}
    end.


%%
%% Internal methods
%%

% Lexes a string into a list of tags. The tags can be fed to parse_node
% to generate an OFX document
lex(Ofx) ->
    case ofx_leex:string(Ofx) of
        {ok, Tags, _} -> {ok, Tags};
        {error, Details} -> {error, {Details, Ofx}}
    end.

% Parses a list of tags into an OFX data tree.
% Will error out in there are tokens that cannot be parsed as part of the tree.
parse(Tags) ->
    {Tree, Unparsed} = parse_node(Tags),
    [] = Unparsed,
    Tree.

% Parse a single OFX node from tokens.
% Returns the node, and any unused tokens.
parse_node([{opentag, Tag}|[{string, Value}|Tags]]) ->
    {#ofx_leaf{name=Tag, value=Value}, Tags};
parse_node([{opentag, Tag}|Tags]) ->
    {Children, Tags2} = parse_node_list(Tag, Tags),
    {#ofx_node{name=Tag,children=Children}, Tags2}.

% Convenience method for parse_node_list/3.
parse_node_list(EndTag, Tags) ->
    parse_node_list(EndTag, Tags, []).

% Parses a list of child nodes. Stops parsing when a {closetag, } tuple is found
% with a name matching the EndTag.
parse_node_list(_EndTag, [], Nodes) ->
    Nodes;
parse_node_list(EndTag, [Tag|Tags], Nodes) ->
    {Node, Tags2} = parse_node([Tag|Tags]),
    case hd(Tags2) of
        {closetag, EndTag} ->
            {[Node|Nodes], tl(Tags2)};
        _ ->
            parse_node_list(EndTag, Tags2, [Node|Nodes])
    end.
