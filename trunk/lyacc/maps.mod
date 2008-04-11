module maps.

mapfun F nil nil.
mapfun F (X :: L) ((F X) :: K) :- mapfun F L K.

mappred P nil nil.
mappred P (X :: L) (Y :: K) :- P X Y, mappred P L K.

% reduce F (X1::X2::...::Xn) Init (F X1 (F X2 (... (F Xn Init)))).

reduce F nil X X.
reduce F (W :: L) X (F W Y) :-  reduce F L X Y.

forevery P nil.
forevery P (X :: L) :- P X, forevery P L.

forsome P (X :: L) :- P X.
forsome P (X :: L) :- forsome P L.

% Natural join of two binary relations.

(R ++ S) X Z :- R X Y, S Y Z.

% Two predicates to search through some initial indicies.

type all_aux      int -> int -> (int -> o) -> o.

all_index C P :- all_aux 1 C P.
all_aux C D P :- if (C > D) true (P C, C' is C + 1, all_aux C' D P).

type some_aux     int -> int -> (int -> o) -> o.

some_index C P :- some_aux 1 C P.
some_aux C D P :- if (C > D) fail
                     (if (P C) true
                         (C' is C + 1, some_aux C' D P)).

% Compare the *extensions* of two relations using negation-by-failure (Yuck!)

type subset    (A -> o) -> (A -> o) -> o.

subset P Q :- not (P X, not (Q X)).

