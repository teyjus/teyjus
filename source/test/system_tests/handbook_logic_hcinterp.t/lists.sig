sig lists.

kind  pairty   type -> type -> type.

type  pair     A -> B -> (pairty A B).

type  id       (list A) -> (list A) -> o.
type  memb     A -> (list A) -> o.
type  member   A -> (list A) -> o.
type  append   (list A) -> (list A) -> (list A) -> o.
type  join     (list A) -> (list A) -> (list A) -> o.
type  assoc    A -> B -> (list (pairty A B)) -> o.
type  domain   (list (pairty A B)) -> (list A) -> o.
type  range   (list (pairty A B)) -> (list B) -> o.

