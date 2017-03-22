%% Records for representing a SGML OFX document as a tree.
%% Nodes may have one or more children, but no value.
%% Leaves can have a string value and no children.
-record(ofx_node, {
          name :: nonempty_string(),
          children :: [{any(), any()}]
         }).
-record(ofx_leaf, {
          name :: nonempty_string(),
          value :: nonempty_string()
         }).
