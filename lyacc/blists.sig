sig blists.
type append, join    (list A) -> (list A) -> (list A) -> o.
type countup         int -> (list int) -> int -> o.
type delete_member   A -> (list A) -> (list A) -> o.
type flatten         (list (list A)) -> (list A) -> o.
type intersection    (list A) -> (list A) -> (list A) -> o.
type length          (list A) -> int -> o.
type memb, member    A -> (list A) -> o.
type memb_and_rest   A -> (list A) -> (list A) -> o.
type nreverse        (list A) -> (list A) -> o.
type nth_and_replace int -> A -> (list A) -> (list A) -> (list A) -> o.
type nth_and_rest    int -> A -> (list A) -> (list A) -> o.
type nth_item        int -> A -> (list A) -> o.
type nth_remove	     int -> (list A) -> (list A) -> o.
type permutation     (list A) -> (list A) -> o.
type replace_member  A -> (list A) -> (list A) -> (list A) -> o.
type reverse         (list A) -> (list A) -> o.
type split           (list A) -> (list A) -> (list A) -> o.
type filter 	     (A -> o) -> (list A) -> (list A) -> o.