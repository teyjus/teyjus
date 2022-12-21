module poplmark-3.

%% Subtyping

sub S top.
sub T T.
sub S T :- sub S Q, sub Q T.
sub (arrow S1 S2) (arrow T1 T2) :- sub T1 S1, sub S2 T2.
sub (forall S1 S2) (forall T1 T2) :- sub T1 S1,
    pi x\ sub x T1 => sub (S2 x) (T2 x).


%% Values

value (abs T E).
value (tabs T E).


%% Typing

of (abs T1 E) (arrow T1 T2) :- pi x\ of x T1 => of (E x) T2.
of (app E1 E2) T12 :- of E1 (arrow T11 T12), of E2 T11.
of (tabs T1 E) (forall T1 T2) :- pi x\ sub x T1 => of (E x) (T2 x).
%% We introduce a T3 in the following rule to prevent an infinite loop
of (tapp E T2) T3 :- of E (forall T11 T12), sub T2 T11, T3 = T12 T2.
of E T :- of E S, sub S T.


%% Small-step evaluation

step (app (abs T E1) E2) (E1 E2).
step (tapp (tabs T1 E) T2) (E T2).
step (app E1 E2) (app E1' E2) :- step E1 E1'.
step (app V1 E2) (app V1 E2') :- value V1, step E2 E2'.
step (tapp E T) (tapp E' T) :- step E E'.
