module mlt.

% ML Type inferencer, with abs, app and let.  Chuck liang, 7/95, revised
% from old version of 4/94.  Adopted to Teyjus 7/2000.
% This is a purely declarative L-lambda program.

monotype integer.
monotype (arr A B) :- monotype A, monotype B.

polytype NV (newvar Mgu) (app M N) (all Ty) :-
pi tv\ (sigma Tn\ (sigma Mg\ (sigma Mg2\ (monotype tv => (
  polytype NV Mt1 M Ar1, fill-out Mt1 Ar1 Mt Artype,
  polytype NV Nt1 N T1,    fill-out Nt1 T1 Nt T,
  merge-type T tv Tn,
  typeunify (tv::NV) Tn Artype Mg,
  append-sub Nt Mt MN, merge-sub MN Mg Mg2,
  resolve-sub (tv::NV) (tv::NV) Mg2 (Mgu tv),
  typesub tv (Mgu tv) (Ty tv)))))).

polytype NV (newvar Mgu) (abs M) (all Typ) :-
  pi x\ (pi a\ (sigma A2\ (sigma B\ (sigma Mg\ (sigma Ty\ (monotype a => (
    ((pi Any\ (polytype Any emp x a))
	=> polytype (a::NV) Mg (M x) B), 
    resolve-sub (a::NV) (a::NV) Mg (Mgu a),
    merge-type a B Ty,
    typesub Ty (Mgu a) (Typ a)))))))).

polytype NV M (let R T) C :-
  polytype NV Mt T Ty,
  pi x\ ((pi A\ (polytype A Mt x Ty)) => polytype NV Mr (R x) B),
  append-sub Mr Mt Mrt, resolve-sub NV NV Mrt M,
  typesub B M C.


merge-type (all A) (all B) (all C) :- 
  pi v\ (monotype v => merge-type (A v) (B v) (C v)).
merge-type (all A) B (all C) :- monotype B,
  pi u\ (monotype u => merge-type (A u) B (C u)).
merge-type A (all B) (all C) :- monotype A,
  pi u\ (monotype u => merge-type A (B u) (C u)).
merge-type A B (arr A B) :- monotype A, monotype B.

merge-sub emp L L.
merge-sub (newvar M) (newvar N) (newvar L) :- 
  pi m\ (merge-sub (M m) (N m) (L m)).
merge-sub (newvar M) N (newvar L) :- (N = emp; N = (sub A B C)),
  pi x\ (copypoly x x => merge-sub (M x) N (L x)).
merge-sub (sub A B C) (newvar N) (newvar L) :- 
  pi x\ (merge-sub (sub A B C) (N x) (L x)).
merge-sub (sub X Y Ss) A (sub X Y B) :- merge-sub Ss A B.

append-sub (newvar M) N (newvar L) :- 
  pi x\ (copypoly x x => append-sub (M x) N (L x)).
append-sub (sub A B C) (newvar N) (newvar L) :- 
  pi z\ (append-sub (sub A B C) (N z) (L z)).
append-sub emp L L.
append-sub (sub X Y Ss) A (sub X Y B) :- append-sub Ss A B.

resolve-for A Vl M N :- (M = emp; M = (sub X Y Z)), 
  ((pi U\ (vrd A U :- member U Vl; rigid U)) => rsub2 Vl A M N).
rsub2 Vl A M (sub A T P) :- collect-for A M AL N, rsub3 (A::Vl) AL T N P.
rsub3 (A::Vl) nil A N N.
rsub3 (A::Vl) (X::nil) X N M :- 
  unify Vl X X U, update-sub N (sub A X U) M.
rsub3 Vl (X::Y::R) Z N M :- 
  unify Vl X Y U, apply_sub X U X2, 
  update-sub N U P, rsub3 Vl (X2::R) Z P M.
collect-for A emp nil emp.
collect-for A (sub A A R) Tr R2 :- collect-for A R Tr R2.
collect-for A (sub A T R) (T::Tr) R2 :- vrd A T, collect-for A R Tr R2.
collect-for A (sub B T R) Tr (sub B T R2) :- (vrd A B; vrd B A),
   collect-for A R Tr R2.

resolve-sub Vs Vl (newvar A) (newvar B) :- 
  pi v\ (append Vs (v::nil) (Vt v),
         append Vl (v::nil) (Vm v),
         resolve-sub (Vt v) (Vm v) (A v) (B v)).
resolve-sub nil Vl A A :- (A = emp; A = (sub X Y Z)).
resolve-sub (V::Vs) Vl A C :- (A = emp; A = (sub X Y Z)),
  membrest V Vl Vn,
  resolve-for V Vn A B, resolve-sub Vs Vl B C.

membrest X (X::Xs) Xs.
membrest X (Y::Xs) (Y::Ys) :- membrest X Xs Ys.

typeunify L (all A) B (newvar Mgu) :- 
  pi t \ (monotype t => typeunify (t::L) (A t) B (Mgu t)).
typeunify L A (all B) (newvar Mgu) :- monotype A,
  pi t \ (monotype t => typeunify (t::L) A (B t) (Mgu t)).
typeunify L A B Mgu :- monotype A, monotype B,  unify L A B Mgu.

typesub (all A) (newvar U) (all B) :- 
  pi v\ (monotype v => typesub (A v) (U v) (B v)).
typesub (all A) S (all B) :- (S = emp; S = (sub X Y Z)),
  pi x\ (monotype x => (copypoly x x => typesub (A x) S (B x))).
typesub A (newvar S) (all B) :- monotype A,
  pi x\ (typesub A (S x) (B x)).
typesub A S B :- monotype A, (S = emp; S = (sub X Y Z)), apply_sub A S B.

apply_sub A (sub X Y Z) B :- copypoly X Y => apply_sub A Z B.
apply_sub A emp B :- copypoly A B.

update-sub emp U emp.
update-sub (sub X Y Z) U (sub X Y2 Z2) :- 
  apply_sub Y U Y2, update-sub Z U Z2.

fill-out N A N A :- monotype A, (N = emp; N = (sub X Y Z)).
fill-out (newvar N) (all A) (newvar M) (all B) :- 
  pi u\ (pi v\ (monotype v => fill-out (N u) (A v) (M u) (B v))).
fill-out (newvar N) A (newvar N) (all B) :- monotype A,
  pi u\ (pi v\ (monotype v => fill-out (N u) A (N u) (B v))).
fill-out N (all A) (newvar M) (all A) :- (N = emp; N = (sub X Y Z)),
  pi u\ (pi v\ (monotype v => fill-out N (A v) (M u) (A v))).


% Unification and utilities: ------

copypoly integer integer.
copypoly real real.
copypoly (arr A B) (arr C D) :- copypoly A C, copypoly B D.
rigid integer.
rigid real.
rigid (arr A B).

transform ((diff X X)::S) S Sub Sub :- var X.
transform ((diff integer integer)::S) S Sub Sub.
transform ((diff real real)::S) S Sub Sub.
transform ((diff (arr A B) (arr C D))::S) 
          ((diff A C)::(diff B D)::S) Sub Sub.
transform ((diff X T)::S) S2 Sub (sub X T Sub2) :-
  var X, 
  occur-check X T,
  diff-subst (sub X T emp) S S2,
  sub-subst (sub X T emp) Sub Sub2.

distinct-vars X Y :- var X, var Y, (vrd X Y; vrd Y X).

occur-check X integer.
occur-check X real.
occur-check X (arr A B) :- occur-check X A, occur-check X B.
occur-check X Y :- distinct-vars X Y.

unify1 O (V::Vs) A B Sub :-
  (var V, (pi Z\ (vrd V Z :- member Z Vs))) => unify1 O Vs A B Sub.
unify1 O nil A B Sub :- 
  unify0 ((diff A B)::nil) emp Sub1,
  add-trivials O Sub1 Sub.

unify0 nil Sub Sub.
unify0 ((diff A B)::R) Sub Sub2 :-
  var B, rigid A, unify0 ((diff B A)::R) Sub Sub2.
unify0 (D::Ds) Sub Sub3 :-
  transform (D::Ds) S Sub Sub2,
  unify0 S Sub2 Sub3.

unify Vs A B U :- unify1 Vs Vs A B U.


add-trivials nil S S.
add-trivials (V::Vs) S S2 :- in-domain V S, add-trivials Vs S S2.
add-trivials (V::Vs) S (sub V V S2) :- 
  not-indomain V S, add-trivials Vs S S2.

in-domain V (sub V X Y).
in-domain V (sub U X Y) :- in-domain V Y.
not-indomain V emp.
not-indomain V (sub U X Y) :- (vrd V U; vrd U V), not-indomain V Y.


sub_apply A emp A.
sub_apply A (sub V T Ss) B :-
  ((copypoly V T, (pi Y\ (copypoly Y Y :- distinct-vars Y V))) =>
    copypoly A C), sub_apply C Ss B.

diff-subst Sub nil nil.
diff-subst Sub ((diff A B)::R) ((diff C D)::R2) :-
  sub_apply A Sub C, sub_apply B Sub D, 
  diff-subst Sub R R2.

sub-subst Sub emp emp.
sub-subst Sub (sub A B R) (sub C D R2) :-
  sub_apply A Sub C, sub_apply B Sub D,
  sub-subst Sub R R2.

%====

update-sig NV G :-
  ((pi Z\ (copypoly Z Z :- member Z NV)) => 
   ((pi Z\ (rigid Z :- member Z NV)) =>
    ((pi X\ (pi Z\ (occur-check X Z :- member Z NV))) =>
     ((pi Sb\ (pi S\ (pi Z\ (transform ((diff Z Z)::S) S Sb Sb 
			:- member Z NV)))) => G)))).


member X [X|T].
member X [Y|T] :- !, member X T.
append [] L L.
append [H|T] L [H|M] :- append T L M.


%=============Examples:============


polytype A emp zero integer.
polytype A emp succ (arr integer integer).
polytype A emp op (all x\ (arr x x)).
polytype A emp op2 (all x\ (all y\ (arr x (arr x x)))).
polytype A emp op3 (all x\ (arr (arr x x) (arr x x))).
polytype A emp op4 (all x\ (all y\ (arr x (arr y y)))).
polytype A emp op5 (all x\ (all y\ (arr (arr x x) (arr y y)))).
polytype A emp op6 (all x\x).  %for separate compilation example

remvac (all x\A) B :- remvac A B.
remvac (all A) (all B) :- 
  pi a\ (monotype a => (appear-in a (A a), remvac (A a) (B a))).
remvac A A :- monotype A.
appear-in A (all B) :- pi x\ (appear-in A (B x)).
appear-in A A :- monotype A.
appear-in A (arr B C) :- appear-in A B; appear-in A C.

infer-type N T :- polytype nil M N S, remvac S T.


go :- example X T, tryonce X T.
%go :- sysprops, stop.
go :- stop.
 

tryonce X T :- !, polytype nil M X T, !, fail.
%tryonce X T :- !, polytype nil M X T, 
%	printterm std_out T, print "\n", !, fail.

tryonce_ans X T :- !, polytype nil M X T, !.

example (abs x\ (app op zero)) T.
example (abs f\ (app f zero)) T.
example (abs f\ (abs x\ (app f x))) T. 
example (abs x\ (abs f\ (app f x))) T.
example (abs f\ (abs g\ (abs x\ (app g (app f x))))) T.
example (abs x\ (abs f\ (app (app f x) (app (app f x) x)))) T.
example (abs f\ (abs x\ (abs y\ (app (app f (app op x)) (app op y))))) T.
example (abs y\ (let (x\ (abs z\x)) (app succ y))) T.
example (let (x\ (app x x)) (abs y\y)) T.
example (let (x\ (let (z\ (app z (app (app x x) (app z z)))) (abs u\u))) (abs y\y)) T. 
example (let (f\ (app (app (abs d1\ (abs (d2 \ zero))) (app f zero)) (app f (abs x \ x)))) (abs x \ x)) T.
example (abs x\ (let (y\x) (app x zero))) T.
example (abs x\ (app op (app op x))) T.
example (abs x\ (let (y\ (app (abs c\c) y)) (app (abs c\c) x))) T.
example (abs x\ (let (u\ (let (v\v) (app succ u))) (app op x))) T.
example (let (u\ (let (v\v) (app op u))) (abs x\x)) T.

% Untypable examples:
example (app (abs x\ (app x x)) (abs x\ (app x x))) T.
example (abs x\ (abs y\ (app (app y x) (app succ y)))) T.
example (abs f\ (let (x\ (app f x)) (app op f))) T.
example (abs x\ (let y\ (app y y)) (app op2 x)) T.
example (abs x\ (let (y\ (app x y)) (app op3 x))) T.
example (abs x\ (let (y\ (app y y)) (app op3 x))) T.
example (abs x\ (let (v\ (app v v)) (abs z\ (app x z)))) T.
example (let (u\ (let (v\v) (app succ u))) (abs x\x)) T.

% some ml equivalents
% fn f => (fn g => (fn x => (g (f x))));
% let val x = (fn y => y) in (let val z = (fn u => u) in (z (x x) (z z)) end) end;
% let val f = (fn x => x) in (((fn d1 => (fn d2 => 0)) (f 0)) (f (fn h => h))) end;










