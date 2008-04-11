sig maps.
accum_sig control.

type forevery	  (A -> o) -> list A -> o.
type forsome	  (A -> o) -> list A -> o.
type mapfun	  (A -> B) -> list A -> list B -> o.
type mappred	  (A -> B -> o) -> list A -> list B -> o.
type reduce	  (A -> B -> B) -> list A -> B -> B -> o.

type  ++          (A -> B -> o) -> (B -> C -> o) -> A -> C -> o.
infixr ++         5.

type all_index    int -> (int -> o) -> o.
type some_index   int -> (int -> o) -> o.

type all_aux      int -> int -> (int -> o) -> o.
type some_aux     int -> int -> (int -> o) -> o.

type subset       (A -> o) -> (A -> o) -> o.
