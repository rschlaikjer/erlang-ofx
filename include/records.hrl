-record(ofx_node, {
          name :: nonempty_string(),
          value :: [{any(), any()}]
         }).
-record(ofx_leaf, {
          name :: nonempty_string(),
          value :: nonempty_string()
         }).
