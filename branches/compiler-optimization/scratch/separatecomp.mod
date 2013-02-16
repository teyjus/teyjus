module separatecomp.

accumulate accumulatemod.

c3 ((x\ x) 1.0).  % Incorrect.
c3 A :- c5 A F, (x\ F) 0. % Incorrect.

c4 F :- c3 0, F, not (F 1).  % Correct.
c4 F :- F, c3 F.  % Incorrect.

c5 0 1.0. % Incorrect.

c7 0 0.   % Correct (gotten from accumulatemod.sig)
