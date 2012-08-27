module kitty.
%accumulate tigerabsyn.
accumulate bobparser.

% cps for tiger.

% do first order first
% no higher order functions, no escaping functions

atomic nilexp.  
atomic breakexp.
atomic (intexp A).
atomic (stringexp B).
atomic dummyexp.
atomic (varexp (simplevar S)).
primitive A :- atomic A, !.
primitive (opexp S A B) :- atomic A, atomic B.
primitive (varexp (subscriptvar A B)) :- atomic (varexp A), atomic B.
primitive (varexp (fieldvar F V)). % :- atomic (varexp F).  % V is string!!!
% objects can't be in array!  severe restrictions on classes!

% uncomment to trace:
%formcps A K J :- printterm std_out (formcps A K J), print " <-\n", fail. 

formcps A K (cps A2 K2) :- primitive A, A = A2, K = K2, !.

formcps (varexp (subscriptvar A B)) K BK :- !, % what to do with A?
  formcps B (kabs b\ (cps (varexp (subscriptvar A b)) K)) BK.

formcps (opexp OP A B) K AK :- atomic B, !, 
  pi v\ ( ((copyexp v A :- true, !) => copyexp (L0 v) (opexp OP A B)),  % dag
          ((copyexp v v, atomic v) => (formcps (L0 v) K (L1 v),  
                                       formcps A (kabs L1) AK))
        ).

formcps (opexp OP A B) K BK :- 
  pi v\ ( ((copyexp v B :- true, !) => copyexp (L0 v) (opexp OP A B)),  % dag
	  ((copyexp v v, atomic v) => (formcps  (L0 v) K (L1 v),  
                                       formcps B (kabs L1) BK))
	).


% The following alternate form needs to be used since Teyjus doesn't allow
% more than 1024 levels of abstractions.
 formcps dummyexp K K.
 formcps (seqexp []) K (kseq [K]).
 formcps (seqexp [H]) K (kseq [HK]) :- !, formcps H K HK.
 formcps (seqexp [H|T]) K (kseq [HK|TK])  :-
    formcps (seqexp T) K (kseq TK), formcps H kret HK.

% simplevar:
formcps (assignexp (simplevar L) R) K LK :-
     formcps R (kabs (rv\ (cps (assignexp (simplevar L) rv) K))) LK.
formcps (assignexp (fieldvar (simplevar F) V) R) K LK2 :-
 formcps R (kabs (rv\ (cps (assignexp (fieldvar (simplevar F) V) rv) K))) LK2.

% subscriptvar:
formcps (assignexp (subscriptvar V I) R) K LK :-
 pi u\ (
     formcps I (kabs (r\ (cps (assignexp (subscriptvar V r) u) K))) (IK u)),
     formcps R (kabs IK) LK.

formcps (ifexp A B C) K (kif AK BK CK K) :-
  formcps A (kabs v\ (kreturn v)) AK,
  formcps B (kabs v\ (kreturn v)) BK, 
  formcps C (kabs v\ (kreturn v)) CK.
 

formcps (whileexp A B) K (kwhile AK BK K) :-
  formcps A (kabs a\ (kreturn a)) AK,
  formcps B kret BK.
 

formcps (callexp F Args) K CALLK :-  kcallaux (varexp F) 0 K Args CALLK.
local kcallaux texp -> int -> kexp -> (list texp) -> kexp -> o.
kcallaux F N K [] (kcall F N K).
kcallaux F N K [A|As] AK :-
  N2 is (N + 1),
  kcallaux F N2 K As ASK,
  formcps A (kabs a\ (karg a N ASK)) AK. 



% use kseq to represent kfix (let) sequence, with body at end of list
formcps (letexp D E) K A :- changelet0 (letexp D E) (let2 B),
     formcps B K A.
formcps (let2 B) K A :- formcps B K A.
formcps (nameterm A) K (kfix B) :- !,
  pi u\ (formcps (A u) K (B u)).

%depair only used in let2 expressions!

formcps (depair (declist []) E) K EK :- formcps E K EK.
formcps (depair (declist [vardec VN VT (arrayexp AT (intexp AL) IE)|Ds]) E) K 
      (kseq [IEK]) :- !,
  pi v\ (
  makenlist AL v (IEL v),   % makes a list of v's
  formcps IE (kabs v\ (kstruct VN (IEL v) Ks)) IEK),
  formcps (depair (declist Ds) E) K Ks.
formcps (depair (declist [vardec VN VT VE|Ds]) E) K (kseq [VEK]) :- 
  ((VT = (namety VTN), !); VTN = "."),  % "." means global scope
  formcps VE (kabs v\ (kvar VN VTN v Ks)) VEK,  
  formcps (depair (declist Ds) E) K Ks.
formcps (depair (declist [classdec CN Prl Pbl|Ds]) E) K 
          (kseq [klass CN Prs Kf]) :- 
  getnamesonly Prl Prs,
  ((inclass CN :- true, !) => formcps (depair (declist Pbl) dummyexp) Ks Kf), 
  formcps (depair (declist Ds) E) K Ks.

formcps (depair (declist [fixdec2 FN B AT RT|Ds]) E) K
          (kseq [(kfunc FN CN BK)|Ks]) :- 
  inclass CN,
  abscps B BK,
  formcps (depair (declist Ds) E) K (kseq Ks).
% The extra k arg will be incorporated into the function call
% abscps is used to process bodies of functions:
abscps (nameterm E) (kname M) :- !,
  pi v\ (abscps (E v) (M v)).  % copystr v v redundant
abscps E M :- formcps E (kabs r\ (kreturn r)) M.


% utilities

local getnamesonly (list tfield) -> (list string) -> o.
local getfnames (list tdec) -> (list string) -> o.
getnamesonly [] [].
getnamesonly [etpair (varexp (simplevar A)) B|C] [A|D] :- getnamesonly C D.
getfnames [] [].
getfnames [fixptdec S B TL TR|Rest] [S|R2] :- getfnames Rest R2.
getfnames [vardec S TT TEXP|Rest] [S|R2] :- getfnames Rest R2.

%type inclass string -> o.
inclass ".".
% change form of let expressions: (streamlines it)

changelet Ds (decabs D) (nameterm E) (nameterm T) :-
  pi n\ (append Ds [n] (Dn n), changelet (Dn n) (D n) (E n) (T n)).
changelet [] (declist []) E (depair (declist []) E).
changelet (D::Ds) (declist [fixptdec FN U AT RT|Fs]) E     
                  (depair (declist [fixdec2 D (U D) AT RT|F2s]) E) :-
  changelet Ds (declist Fs) E (depair (declist F2s) E).
changelet (D::Ds) (declist [vardec VN VT VE|Fs]) E
          (depair (declist [vardec D VT VE|F2s]) E) :-
  changelet Ds (declist Fs) E (depair (declist F2s) E).

changelet (D::Ds) (declist [classdec CN Pr Pb|Fs]) E
	          (depair (declist [classdec D Pr Pc|F2s]) E) :-
  getfnames Pb Pbn,
  changelet Pbn (declist Pb) dummyexp (depair (declist Pc) dummyexp),  
  changelet Ds (declist Fs) E (depair (declist F2s) E).

changelet0 (letexp D E) (let2 E2) :- changelet [] D E E2.

local kmapn int -> (int -> A -> B -> o) -> (list A) -> (list B) -> o.
kmapn N P [] [].
kmapn N P [H|T] [H2|T2] :- (P N H H2), N2 is (N + 1), kmapn N2 P T T2.

local makenlist int -> texp -> (list kexp) -> o.  % only used for arrays
makenlist 0 A [].
makenlist N A [kreturn A|B] :- N > 0, N2 is (N - 1), makenlist N2 A B.

% copy clauses for continuations:

kopy kret kret.
kopy (kreturn A) (kreturn B) :- copyexp A B.
kopy incpc incpc.
kopy (cps E K) (cps E2 K2) :-
  copyexp E E2, !, kopy K K2.
kopy (kfix A) (kfix B) :-
  pi v\ ( kopy (A v) (B v)).
kopy (kname A) (kname B) :-
  pi v\ ( kopy (A v) (B v)).
kopy (kif E A B K) (kif F C D J) :-
  kopy E F, kopy A C, kopy B D, kopy K J.
kopy (kwhile E A K ) (kwhile F B J) :-
  kopy E F, kopy A B, kopy K J.
kopy (kseq []) (kseq []).
kopy (kseq [A|B]) (kseq [C|D]) :-
  kopy A C, kopy (kseq B) (kseq D).
kopy (kcall S I K) (kcall S2 I K2) :-
  copyexp S S2, !, kopy K K2.
kopy (kcall0 S I K) (kcall0 S2 I K2) :-
  copystr S S2, !, kopy K K2.
kopy (karg T I K) (karg S I J) :- copyexp T S, !, kopy K J.
kopy (kabs K) (kabs J) :-
  pi v\ (copyexp v v => kopy (K v) (J v)).
kopy (kfunc S C K) (kfunc T D J) :- copystr S T, copystr C D, kopy K J. 
% Be not B2?
kopy (kvar A C1 B C) (kvar A2 D1 B2 C2) :-
  copystr A A2, copyexp B Be, !, kopy C C2, copystr C1 D1. 
kopy (kstruct S [] K) (kstruct T [] J) :- copystr S T, kopy K J.
kopy (kstruct S [A|B] K ) (kstruct T [C|D] J) :-
  kopy A C,
  kopy (kstruct S B K) (kstruct T D J).
kopy (klass S SL K) (klass T TL J) :-
  copystr S T, map_pred2 copystr SL TL, kopy K J.

%  checks if texp occurs in a continuation:


%notfreeink A K :- 
%  pi b\ (sigma N\ ( (copyexp A b) => (kopy K N, !, N = K) ) ).

% more optimal notfreeink: 

%notfreeink A K :- printterm std_out (notfreeink A K), print "*** ... ", fail.

notfreeink A K :- 
    ((copyexp A A :- true, !, fail) => (kopy K K)).
%  print " YES!!\n".

%notfreeink A K :- 
%  (pi b\ (sigma N1\ ( (copyexp A b) => (kopy K N1, !, N1 = K)))).
%   print " YES!!\n".

%notfreeink A K :- print "NO!\n", fail. 


%notfreeink A K :- pi v\ ((copyexp A v :- true, !, true) => kopy K K), !.

% not used:
okkursin A K :-  
  pi b\ (sigma N\ ( (copyexp A b :- !) => (kopy K N, !, not (N = K))) ).


% optimizations

% number of times a texp occurs in a kexp:
%numoccur 0 E kret.
%numoccur 0 E (kseq []).
%numoccur N E (kabs A) :- pi v\ (numoccur N E (A v)).
%numoccur N E (cps A K) :- oneif J E A, numoccur M E K, N is (M + J).
%numoccur 1 E (kreturn E) :- !.
%numoccur 0 E (kretrun A).
%numoccur N E (kfix A) :- pi v\ (numoccur N E (A v)).
%numoccur N E (kname A) :- pi v\ (numoccur N E (A v)).
%numoccur N E (kif E2 A B K) :- oneif M E E2,
%  numoccur N1 E A, numoccur N2 E B, numoccur N3 E K,
%  N is (M + N1 + N2 + N3).
% INCOMPLETE!

%local oneif int -> A -> A -> o.
%oneif 1 A A :- !,
%oneif 0 A B.

% constant folding:  do it with copy clauses
% copyexp (opexp "+" (intexp A) (intexp B)) (intexp C) :- !, C is (A + B).
% etc ...


% trivial continuations of the form (cps (intexp X) (kabs K))
% specialized copy clauses:

% kopy (cps (intexp X) (kabs K)) T :- !, kopy (K (intexp X)) T.
% kopy (cps A (kabs B)) T :- !, kopy (B A) T.  % inverse cps

% Data flow analysis
% different kinds of terms:
% continuations with no abstraction 
% continuations (kabs x\A).  These continuation can not be trivially
% eliminated (maybe result of some assignment operation).
% maybe should use kreturn instead of kret.

% should data flow analysis occur at the cps level at all?

%local invertcps kexp -> kexp -> o.
%invertcps (cps A (kabs K)) B :- !, invertcps (K A) B.
%invertcps (karg E I (kabs K)) B :- !, invertcps (K E) B.
%invertcps (kvar A B E (kabs K)) B :- !, invercps (K (varexp (simplevar A))) B.
%invertcps A A.   % default

% inlining function calls. (beta reduction)  
% need to count how many times something occurs


% typing and type inference


% common subexpression elimination  (dags?)
% part of this is done when the cps rep is formed.  The kabs abstraction
% covers as much as possible, in conjunction with cexp.

