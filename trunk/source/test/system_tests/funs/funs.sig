sig funs.

% import list.

type  mapfun   (A -> B) -> list A -> list B -> o.
type  foldr    (A -> B -> B) -> B -> list A -> B -> o.
type  foldl    (A -> B -> A) -> A -> list B -> A -> o.

% for testing
kind  i  type.
type  g  i -> i -> i.
type  a  i.
type  b  i.
