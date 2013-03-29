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


%% Predicates

type    sub     tp -> tp -> o.
type    value   term -> o.
type    of      term -> tp -> o.
type    step    term -> term -> o.
