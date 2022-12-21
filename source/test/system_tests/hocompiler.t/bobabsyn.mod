module bobabsyn.

% should use parser to generate copy clauses from signature automatically!

copystr S S.
copyvar (simplevar S) (simplevar T) :- copystr S T.
copyvar (fieldvar V S) (fieldvar W T) :- copyvar V W, copystr S T.
copyvar (subscriptvar V E) (subscriptvar W F) :-
	copyvar V W, copyexp E F.
copyexp nilexp nilexp.
copyexp breakexp breakexp.
copyexp (varexp V) (varexp W) :- copyvar V W.
copyexp (intexp N) (intexp N). 
copyexp (stringexp S) (stringexp S).
copyexp (opexp O A B) (opexp O C D) :-
	copyexp A C, copyexp B D.
copyexp dummyexp dummyexp.
copyexp (callexp S []) (callexp T []) :- copyvar S T.
copyexp (callexp S [A|B]) (callexp T [C|D]) :-
	copyexp A C, copyexp (callexp S B) (callexp T D).
copyexp (recordexp S []) (recordexp T []) :- copystr S T.
copyexp (recordexp S [(eepair U V)|B]) (recordexp T [(eepair X Y)|D]) :- 
	copyexp U X, copyexp V Y, copyexp (recordexp S B) (recordexp T D).
copyexp (seqexp []) (seqexp []).
copyexp (seqexp [A|B]) (seqexp [C|D]) :-
	copyexp A C, copyexp (seqexp B) (seqexp D).
copyexp (assignexp V E) (assignexp W F) :-
	copyvar V W, copyexp E F.
copyexp (ifexp A B C) (ifexp D E F) :-
	copyexp A D, copyexp B E, copyexp C F.
copyexp (whileexp A B) (whileexp C D) :-
	copyexp A C, copyexp B D.
copyexp (forexp D A B) (forexp F C D2) :-
	copydec D F, copyexp A C, copyexp B D2.
copyexp (arrayexp S A B) (arrayexp T C D) :- 
	copystr S T, copyexp A C, copyexp B D.
copyexp (depair D E) (depair G H) :- copydec D G, copyexp E H.
copyty (namety S) (namety T) :- copystr S T.
copyty (recordty []) (recordty []).
copyty (recordty [(etpair E1 T1)|R1]) (recordty [(etpair E2 T2)|R2]) :-
	copyexp E1 E2, !, copyty T1 T2, copyty (recordty R1) (recordty R2).
copyty (arrayty S) (arrayty T) :- copystr S T.
copyty dummytype dummytype.
% The strings should not be copied here!
copydec (typedec S T) (typedec S V) :-  copyty T V.
copydec (vardec S T E) (vardec S B C) :-
	copyty T B, copyexp E C, !, 5 = 5.
copydec (classdec S [] F) (classdec T [] G) :-
  copystr S T, copydec (declist F) (declist G).
copydec (classdec S [etpair E1 T1|R] F) 
	(classdec S2 [etpair E2 T2|R2] G) :-
  copyexp E1 E2, !, copyty T1 T2,
  copydec (classdec S R F) (classdec S2 R2 G).

  
%copyexp (letexp [] A) (letexp [] B) :- copyexp A B.
%copyexp (letexp [C|D] A) (letexp [E|F] B) :- 
%	copydec C E, copyexp (letexp D A) (letexp F B).
copyexp (letexp C A) (letexp E B) :- copydec C E, copyexp A B.
copyexp (coercexp A) (coercexp B) :- copydec A B.
copydec (fixptdec N A [] T) (fixptdec N B [] U) :-
  (pi s\ (copyexp (A s) (B s))), copyty T U, copystr N M.
copydec (fixptdec N A [C|D] T) (fixptdec M B [E|F] U) :-
  copyty C E, copydec (fixptdec N A D T) (fixptdec M B F U).
copyexp (absterm A) (absterm B) :-
  pi v\ (copyexp v v => copyexp (A v) (B v)).
copyexp (nameterm A) (nameterm B) :-
  pi s\ (copyexp (A s) (B s)).		% copystr s s redundant

% lambda abstraction over the declarations parallels those of
% the let body - to allow for mutually recursive definitions.
copydec (decabs A) (decabs B) :- pi s\ (copydec (A s) (B s)).
copydec (declist []) (declist []).
copydec (declist [A|B]) (declist [C|D]) :-
	copydec A C, copydec (declist B) (declist D).


% moved from kitty.mod 8/03
copyexp (let0 M N) (let0 M2 N2) :-
  copyexp M M2, 
  (pi v\(copyexp v v => copyexp (N v) (N2 v))).
copyexp (lamexp A) (lamexp B) :-
  pi v\ (copyexp v v => copyexp (A v) (B v)).
copyexp (let2 A) (let2 B) :- copyexp A B.
copydec (fixdec2 N A [] T) (fixdec2 M B [] U) :-
  copyexp A B, copyty T U, copystr N M.
copydec (fixdec2 N A [C|D] T) (fixdec2 M B [E|F] U) :-
  copystr N M, copyexp A B, copyty T U,
  copyty C E, copydec (fixdec2 N A D T) (fixdec2 M B F U).


% more to come when interpreter/compiler is created.

hastype (intexp I) (namety "integer").

% etc ...
