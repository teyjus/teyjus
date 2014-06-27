module control.

if Cond Then Else :- Cond, !, Then.
if Cond Then Else :- Else.

once P :- P, !.

announce G :-
  print ">> ", term_to_string G String, print String, print "\n", fail.

spy G :-
(print ">Entering ", term_to_string G Str,  print Str,  print "\n", G,
   print ">Success  ", term_to_string G Strx, print Strx, print "\n";
   print ">Leaving  ", term_to_string G Str,  print Str,  print "\n", fail).

spyf G :-
  (G; print ">SpyF  ", term_to_string G Str,  print Str,  print "\n", fail).

value X :- term_to_string X StrX, print StrX, print "\n".

printout Msg Var :- print Msg, (printterm std_out Var), print "\n".
