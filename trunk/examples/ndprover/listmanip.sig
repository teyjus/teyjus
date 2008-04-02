/* File listmanip.sig. Signature file for list manipulating predicates
used in the theorem prover */

sig listmanip.

type    member                  A -> (list A) -> o.
type    member_and_rest         A -> (list A) -> (list A) -> o.
type    nth_item                int -> A -> (list A) -> o.
type    nth_item_and_rest       int -> A -> (list A) -> (list A) -> o.
type    member_move_to_end      A -> (list A) -> (list A) -> o.
type    add_to_end              A -> (list A) -> (list A) -> o.

