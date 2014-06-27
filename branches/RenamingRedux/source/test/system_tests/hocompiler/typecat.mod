module typecat.
accumulate bobparser.

copyty (tarr [] A) (tarr [] B) :- copyty A B.
copyty (tarr [H|T] A) (tarr [U|V] B) :-
  copyty H U, copyty (tarr T A) (tarr V B).

%uncomment to trace:
%typeexp A B :- printterm std_out (typeexp A B), print " <-\n", fail.

typeexp dummyexp dummytype.
typeexp nilexp dummytype.
typeexp (intexp N) (namety "int").
typeexp (stringexp S) (namety "string").
typeexp (opexp OP A B) W :- optype OP (tarr [U,V] W), 
  typeexp A U, typeexp B V.
typeexp (callexp FV AT) W :-
  typeexp (varexp FV) (tarr FT W),
  forall2 (x\ (y\ (typeexp x y))) AT FT.
typeexp (seqexp []) dummytype.
typeexp (seqexp [H]) A :- typeexp H A.
typeexp (seqexp [H,I|T]) A :- typeexp H HT, typeexp (seqexp [I|T]) A.
typeexp (assignexp V E) T :- 
    typeexp (varexp V) T, typeexp E T.
typeexp (ifexp A B dummyexp) T :- !, 
    typeexp A (namety "bool"), typeexp B T.
typeexp (ifexp A B C) T :- typeexp A (namety "bool"),
  typeexp B T, typeexp C T.
typeexp (whileexp A B) BT :-
  typeexp A (namety "bool"), typeexp B BT.
typeexp (arrayexp S R I) (arrayty S) :-   % need generalization from string
  typeexp R (namety "int"), typeexp I (namety S).
typeexp (varexp (subscriptvar (simplevar V) E)) (namety T) :-
  typeexp E (namety "int"),
  typeexp (varexp (simplevar V)) (arrayty T).
typeexp (letexp D E) T :- typedecl [] D E T.

local typedecl (list string) -> tdec -> texp -> ttype -> o.
%typedecl A B C D :- printterm std_out (typedecl A B C D), print " <-d\n", fail.
typedecl Ns (decabs D) (nameterm E) T :-
 pi v\ (append Ns [v] (VNS v),
   (typeexp (varexp (simplevar v)) VT => typedecl (VNS v) (D v) (E v) T)).
typedecl [] (declist []) E T :- typeexp E T.
typedecl [N|Ns] (declist [D|Ds]) E T :- 
  D = (vardec VNS VT0 VE), 
  (VT0 = dummytype; VT = VT0), !,    % dummytype hack
  typeexp (varexp (simplevar N)) VT, typeexp VE VT,
  typedecl Ns (declist Ds) E T.
typedecl [N|Ns] (declist [D|Ds]) E T :-
  D = (fixptdec FNS FIX AT RT),
  typeexp (varexp (simplevar N)) (tarr AT RT),
  typefix (FIX N) AT RT,
  typedecl Ns (declist Ds) E T.

% class representation not consistent, can't type without eigenvars!
%typedecl [N|Ns] (declist [D|Ds]) E T :-
%  D = (classdec CNS PRIVATE PUBLIC),
%  typeclass PRIVATE PUBLIC,
%  typedecl Ns (declist Ds) E T.


% may need to use setsubst here to flatten all types! 
%typeclass [] PUB :- 

local typefix texp -> (list ttype) -> ttype -> o.
typefix (nameterm F) [A|As] RT :- !,
  pi u\ (typeexp (varexp (simplevar u)) A => 
 	   typefix (F u) As RT).
typefix F [] RT :- !, typeexp F RT.

%typeexp (recordexp)    % not implemented
% default failure clauses report error:
typedecl [N|Ns] (declist [D|Ds]) E T :- !,
  print "Error type checking declaration ",
  printterm std_out D, print "\n.", stop.
typeexp A dummytype :- !, print "Error type checking expression ",
  printterm std_out A, print "\n.", stop.


local optype string -> ttype -> o.
optype "+" (tarr [namety "int",namety "int"] (namety "int")).
optype "-" (tarr [namety "int",namety "int"] (namety "int")).
optype "*" (tarr [namety "int",namety "int"] (namety "int")).
optype "=" (tarr [namety X,namety X] (namety "bool")).
optype "<" (tarr [namety "int",namety "int"] (namety "bool")).
optype ">" (tarr [namety "int",namety "int"] (namety "bool")).
optype "<=" (tarr [namety "int",namety "int"] (namety "bool")).
optype ">=" (tarr [namety "int",namety "int"] (namety "bool")).
optype "<>" (tarr [namety X,namety X] (namety "bool")).

local forall2 (A -> B -> o) -> (list A) -> (list B) -> o.
forall2 P [] [].
forall2 P [H|T] [A|B] :- (P H A), forall2 P T B.
% optional parameters are easy!
%forall2 P [] [optional_param S|T].
%forall2 P [H|T] [optional_param S|U] :-
%  (P H S), % P willbe 'typeexp'
%  forall2 P T U.

% builtins, like printf:
typeexp (varexp (simplevar "printf")) (tarr [namety "string"|T] dummytype).




