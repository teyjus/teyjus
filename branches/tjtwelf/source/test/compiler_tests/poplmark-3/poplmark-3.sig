sig poplmark-3.


%% Types

kind    tp      type.

type    top     tp.
type    arrow   tp -> tp -> tp.
type    forall  tp -> (tp -> tp) -> tp.


%% Terms

kind    term    type.

type    abs     tp -> (term -> term) -> term.
type    app     term -> term -> term.
type    tabs    tp -> (tp -> term) -> term.
type    tapp    term -> tp -> term.


%% Testing predicates

type    filler      term.

type    pred        term -> term -> term -> tp -> o.
type    test        o.
