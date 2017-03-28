-module(ofx_tree).
-include_lib("ofx/include/records.hrl").

-export([
    get_child/2,
    get_children/1,
    get_children/2,
    get_name/1,
    get_value/1
]).

get_child(Name, #ofx_node{children=Children}) ->
    get_child_list(Name, Children).

get_child_list(_Name, []) ->
    not_found;
get_child_list(Name, [Node|Nodes]) ->
    case get_name(Node) of
        Name -> Node;
        _ -> get_child_list(Name, Nodes)
    end.

get_children(#ofx_node{children=Children}) ->
    Children.

get_children(Name, #ofx_node{children=Children}) ->
    lists:filter(
      fun(Node) -> get_name(Node) =:= Name end,
      Children
    ).

get_name(#ofx_node{name=Name}) -> Name;
get_name(#ofx_leaf{name=Name}) -> Name.

get_value(#ofx_leaf{value=Value}) -> Value.
