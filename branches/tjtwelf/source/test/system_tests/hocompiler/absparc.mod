module absparc.
accumulate kitty.

% bug fix for cut added

lexlevel 0.

% permstore is used for lvalues

assoces (intexp A) (conststore A) :- !.
assoces A S :- lexlevel LL, permstore LL A S, !.

%assoces A S :- 
%  permstore L A C,
%  lexlevel LL, !, LD is (LL - L), addoffset C LD S.

assoces dummyexp All.   % default - all are available.
 
freestore (reg "o" 0).   % default accumulator
freestore (reg "o" 1).
freestore (reg "o" 2).
freestore (reg "o" 3).
freestore (reg "o" 4).
%  freestore (reg "o" 5).   % closure parameter
localstore (reg "l" 0).   % should really be "tempstore"
localstore (reg "l" 1).
localstore (reg "l" 2).
localstore (reg "l" 3).
localstore (reg "l" 4).
localstore (reg "l" 5).
localstore (reg "l" 6).
localstore (reg "l" 7).
localstore (reg "i" 0).
localstore (reg "i" 1).
localstore (reg "i" 2).
localstore (reg "i" 3).
localstore (reg "i" 4).
% localstore (reg "i" 5). % closure parameter
freestore X :- localstore X.  % localstore subset of freestore 

% register %g7 used as local swap register.

% store is free (fp-20), etc if it's in shallower level:
local isfree store -> o.
%isfree X :- (assoces E X, !, E = dummyexp);
%  (lexlevel LL, permstore PL E2 X, LL > PL, !).

% this version will forcefully free i registers:
isfree (indirect A B) :- 
  lexlevel LL, !, not (permstore LL V (indirect A B)), 
  assoces E (indirect A B), !, E = dummyexp.
isfree X :- assoces E X, !,
   ((E = dummyexp, !); 
    (getthestore E ES, !, not (ES = X))), !.  % not current

% getstore returns o registers and sp+68, sp+72, etc...
% used to load operands, actual parameters
getstore X :- freestore X, isfree X, !.
getstore (indirect (sreg "sp") (conststore Offset)) :- 
  findoffset "sp" 68 Offset 4.

% gettemp used to return location to temporarily store o register
% prefered is a l register
gettemp X :- localstore X, isfree X, !.
gettemp (indirect (sreg "fp") (conststore Offset)) :- X is (0 - 20),
  Y is (0 - 4), findoffset "fp" X Offset Y.

% getlocal get permenant RAM locations for declared variables.
getlocal (indirect (sreg "fp") (conststore Offset)) :- X is (0 - 20),
  Y is (0 - 4), findoffset "fp" X Offset Y.

local findoffset string -> int -> int -> int -> o.
findoffset SR X0 X0 Inc :- 
  isfree (indirect (sreg SR) (conststore X0)), !.
findoffset SR X0 X Inc :- X1 is (X0 + Inc), findoffset SR X1 X Inc.


% first, second arg to gencode is label counter

% uncomment to trace:
%gencode A B C S D :- printterm std_out (gencode A B C S D), print " <--\n", fail.
%gencode L M (cps E K) S I :- printterm std_out (cps E K), print "<-\n", fail.

% gencode args:
% 1: current label counter
% 2: final label counter
% 3: continuation to be processed
% 4: store location of final result
% 5: instruction list generated

% needs lots more special cases:
gencode L M (cps (opexp "="  A B) K) S I :- !,
  genbool L M "beq" A B K S I.
gencode L M (cps (opexp "<>"  A B) K) S I :- !,
  genbool L M "bne" A B K S I.
gencode L M (cps (opexp ">"  A B) K) S I :- !,
  genbool L M "bg" A B K S I.
gencode L M (cps (opexp "<"  A B) K) S I :- !,
  genbool L M "bl" A B K S I.
gencode L M (cps (opexp "<="  A B) K) S I :- !,
  genbool L M "ble" A B K S I.
gencode L M (cps (opexp ">="  A B) K) S I :- !,
  genbool L M "bge" A B K S I.

gencode L M (cps (opexp "*" A B) K) S I :- !,  % sparc has no multiplication
  gencode L M (cps A (kabs a\ (karg a 0 
		 (cps B (kabs b\ (karg b 1 
		    (kcall (varexp (simplevar ".umul")) 2 K))))))) S I.

gencode L M (cps (opexp F A B) K) S
   [LA, LB, (basicop F AS BS DS)|Rest] :- !,
   genloadop A AS LA, % find AS, generate code ifnecessary
  ((assoces A AS :- true, !) => 
    ( genloadop B BS LB, 
      (assoces B BS :- true, !) =>   
      (  ifimpk (notfreeink A K) AS
         (ifimpk (notfreeink B K) BS
	    (getstore DS, processk DS L M K S Rest))
    ))).

gencode L M (cps (assignexp (subscriptvar A I) B) K) S 
    [LB, LI, movop OFS (reg "g" 7), movop IS TS, 
     basicop "sll" TS (conststore 2) TS,
     basicop "-" (reg "g" 7) TS TS, movop BS (indirect R TS)|Rest] :- 
  permstore VL (varexp A) VS0, lexlevel LL, !, 
  LD is (LL - VL), addoffset VS0 LD (indirect R OFS),
  getstore TS,  % temp
  (pi v\ ((assoces v TS :- true, !) => genloadop I IS LI, 
     ((assoces v IS :- true, !) => genloadop B BS LB))),
  processk BS L M K S Rest.

gencode L M (cps (assignexp A B) K) S [LB, (movop BS AS)|Rest] :- 
  lexlevel LL, 
  permstore PL (varexp A) AS0, !,  % failure means undeclared var
  LD is (LL - PL), addoffset AS0 LD AS,
  genloadop B BS LB,
  processk BS L M K S Rest.  % K=kret? if so, doesn't matter
%  ((assoces (varexp A) AS :- !) =>  % make sure A will be reloaded 
%     (ifimpk (getthestore (varexp A) AS2) AS2 
%       (processk nullstore L M K S Rest))).

gencode L M (cps (assignexp (simplevar A) B) K) S   % in function in class
  [LB,movop BS (indirect (reg "i" 5) (conststore N3))|Rest] :-
  inclass C, classinfo C CM, 
  findmemb1 A CM N, N2 is (N * 4),
  genloadop B BS LB,
  N3 is (0 - N2),
  processk BS L M K S Rest.  % K = kret, so no use to =>

gencode L M (cps dummyexp K) S I :- !, processk nullstore L M K S I.

gencode L M (cps N K) S [LN|Rest] :-
  genloadop N NS LN,  % instantiates NS and LN
  ((assoces N NS :- true, !) => processk NS L M K S Rest).

gencode L M (kfix A) S I :- 
  makelabel "func" L Fnlab, L2 is (L + 1),
  pi f\ ((copystr f Fnlab :- true, !) => gencode L2 M (A f) S I).

% may need to use nop
% at the end of each branch, result (from return) is moved to common store:
gencode L M (kif AK BK CK K) S    
  [instseq LA, basicop "cmp" AS (conststore 1) nullstore,
   branchop "bne" (slabel Flab),nullop |I] :- 
  gencode L L2 AK AS LA,
  makelabel "else" L2 Flab, L3 is (L2 + 1),
  makelabel "endif" L2 Endlab, 
  gencode L3 L4 BK SB BI, gencode L4 L5 CK SC CI, 
  getstore SS, % value for either SB or SC
  append BI 
    [movop SB SS, branchop "ba" (slabel Endlab),nullop,ilabel Flab|CI] I2,
  append I2 [movop SC SS, ilabel Endlab|Rest] I,
  processk SS L5 M K S Rest.

gencode L M (kwhile AK BK K) S  
  [ilabel StartL|I] :-
  makelabel "swhile" L StartL, makelabel "ewhile" L EndL,
  L2 is (L + 1),
  gencode L2 L3 AK AS AI,  % AS now contains location of boolean
  gencode L3 L4 BK BS BI,  % actually BS will be nullstore.
  append AI [basicop "cmp" AS (reg "g" 0) nullstore,
             branchop "be" (slabel EndL), nullop|BI] 
         ABI,
  append ABI [branchop "ba" (slabel StartL), nullop, (ilabel EndL)|Rest] I,
  processk AS L4 M K S Rest.
  
% need to know output label
gencode L L kret nullstore [].
gencode L L (kreturn E) S [] :- genloadop E S LE, !.  % LE not used!
gencode L L (kseq []) nullstore [].
gencode L M (kseq [HK]) S I :- gencode L M HK S I.
gencode L M (kseq [HK,HK2|TK]) S I :- 
  gencode L L2 HK S0 HI,
  gencode L2 M (kseq [HK2|TK]) S TI, append HI TI I.

% called when processing argument list (needs to know where they are!)
% assoces A AS assured since it's from (kabs a\ (karg a N K))
% o0 can't be used right away because of nested calls.
% AS is current location of parameter - needs to be moved to o register
% Old is old contents of oN register, LA is store to move it to.
% ATS is needed if AS is also an o register.
% If LA = (reg "o" N), the movop will be optimized away later.
gencode L M (karg A N K ) S [LA0,movop (reg "o" N) LA, movop AS ATS|Rest] :- 
     N < 5,
%     assoces A AS,  % can't use after cps form is simplified!
     genloadop A AS LA0, % LA0 is redundant if used before simplification
     freereg3 K Old (reg "o" N) LA, !,  % move register contents if nec.
     ((assoces Old LA :- true, !) =>
      (
       (
        (AS = (reg "o" N), ATS = LA); 
        (AS = (reg "o" 0), N > 0, gettemp ATS);  % o0 is ret. val
        (AS = (reg "o" M1), freereg (reg "o" M1) ATS); % critical!
        (ATS = AS)
       ), !,
       argcount AC, AC2 is (AC + 1),
       ((savedarg AC N ATS) =>  ((assoces A ATS :- true, !) =>
         ((argcount AC2 :- true, !) => 
            processk nullstore L M K S Rest))))).

gencode L M (karg A N K) S
            [movop AS (indirect (sreg "sp") (conststore OF))|Rest] :- 
  N > 4, assoces A AS, findoffset "sp" 68 OF 4, !,
  argcount AC, AC2 is (AC + 1),
 ((argcount AC2 :- true, !) =>    
    processk (indirect (sreg "sp") (conststore OF)) L M K S Rest).

% kind of inefficient: redundant saving of %o0 right before call:
% note if FS = %o0, it will be optimized away later:
%gencode L M (kcall0 F N K) S 
%   [movop FS (reg "o" 0), branchop "call" (slabel Fl), nullop|Rest] :-
%  argcount AC, AC2 is (AC - N), % for nested calls
%  savedarg AC2 FS, % location of first argument  (needed for nested calls)
%  copystr F Fl, !,
%  ((argcount AC2 :- !) => 
%    processk (reg "o" 0) L M K S Rest).

gencode L M (kcall (varexp (simplevar F)) 0 K) S 
   [SO,branchop "call" (slabel Fl),nullop|I] :- 
  ((isfree (reg "o" 0), !, CO = dummyexp, SO = killmeop);
   (assoces CO (reg "o" 0), SO = (movop (reg "o" 0) NewO))), !,
  getstore NewO,
  copystr F Fl, !,
  ((assoces CO NewO :- true, !) => 
    processk (reg "o" 0) L M K S I).
gencode L M (kcall (varexp (simplevar F)) N K) S I :- N > 0, 
  argcount AC, AC2 is (AC - N), % for nested calls
  gencallaux 0 N AC2 MOVARGS, 
  copystr F Fl, !,
  append MOVARGS [branchop "call" (slabel Fl), nullop|Rest] I,
  ((argcount AC2 :- true, !) => 
    processk (reg "o" 0) L M K S Rest).

gencode L M (kcall (varexp (fieldvar A F)) 0 K) S 
      [movop AS2 (reg "o" 5),branchop "call" (slabel Fl),nullop|I] :- 
  ofclass (varexp A) AC, copystr AC ACL,
  copystr F F0, Fl is (ACL ^ F0), lexlevel LL, !, 
  permstore LX (varexp A) AS, LD is (LL - LX), addoffset AS LD AS2,
  processk (reg "o" 0) L M K S I.
gencode L M (kcall (varexp (fieldvar A F)) N K) S I :- N > 0, 
  ofclass (varexp A) AClass, copystr AClass ACL,
  copystr F F0, Fl is (ACL ^ F0),  
  argcount AC, AC2 is (AC - N), % for nested calls
  gencallaux 0 N AC2 MOVARGS, 
  lexlevel LL, !, 
  permstore LX (varexp A) AS, LD is (LL - LX), addoffset AS LD AS2,
  append MOVARGS 
      [movop AS2 (reg "o" 5),branchop "call" (slabel Fl), nullop|Rest] I,
  ((argcount AC2 :- true, !) => 
    processk (reg "o" 0) L M K S Rest).

% local variable definition:
gencode L M (kvar VN "." V K) S [movop VS LS|Rest] :-  % not object
  assoces V VS,    % where texp V is stored,
  getlocal LS,     % permanent local store for VN
  lexlevel LL,  !,  % for permstore
  ((assoces dummyexp VS :- true, !) =>   % free VS (moved)
    ((permstore LL (varexp (simplevar VN)) LS :- true, !) =>  % new assoc
     ((ofclass (varexp (simplevar VN)) ".") =>
     processk LS L M K S Rest))).
% the above code will generate redundant assocs v LS =>

gencode L M (kvar VN VC V K) S 
    [LI1, movop (conststore CS) (reg "o" 0),
     branchop "call" (slabel "malloc"), nullop,
     movop (reg "o" 0) LS, LI2|Rest] :-  % is object
  classinfo VC CM, !, length CM CML, CS is (CML * 4),  % size of class
  ((isfree (reg "o" 0), !, LI1 = killmeop, LI2 = killmeop);
   (LI1 = movop (reg "o" 0) (reg "g" 7), LI2 = movop (reg "g" 7) (reg "o" 0))), !,
  getlocal LS,     % permanent local store for VN
  lexlevel LL,  !,  % for permstore
    ((permstore LL (varexp (simplevar VN)) LS :- true, !) =>  % new assoc
     ((ofclass (varexp (simplevar VN)) VC) =>   % type info!
        processk LS L M K S Rest)).

% fixpt definitions:
gencode L M (kfunc FN FC F) (slabel Fnl) 
   [branchop "ba" (slabel Endlab),nullop,
    ilabel Fnl, 
    basicop "save" (sreg "sp") (conststore X10) (sreg "sp")|I] :-
  copystr FN Fn0, 
  ((FC = ".", !, Fcl = ""); (copystr FC Fcl)), !,
  Fnl is (Fcl ^ Fn0),
  X10 is (0 - 200),   % need to change!
  makelabel "endfunc" L Endlab, L2 is (L + 1),
  lexlevel LL, LL2 is (LL + 200),   % 200 for now - need change
  ((lexlevel LL2 :- true, !) =>  % window shift, 
   ((pi R\ (pi N\ (assoces dummyexp (reg R N) :- true, !))) =>  %free registers
    ((pi N\ (assoces dummyexp (indirect (sreg "fp") (conststore N)))) =>
     ((inclass FC :- true, !) =>
     (genfunc 0 L2 M F I2))))),
  append I2 [ilabel Endlab] I.

gencode L M (kstruct SN MK K) S I :-
  length MK ML, % size of struct
  lexlevel LL, !,
  getlocal LS, % address for start of struct
  MLM is (0 - ML),
  addoffset LS MLM (indirect R (conststore OF)),   %structs stored upside down
  OF3 is (OF + ML),
  pi v\ ((permstore LL (varexp (simplevar SN)) LS :- true, !) =>   
   (pi OF2\ (assoces v (indirect R (conststore OF2)) :- 
		OF2 >= OF, OF2 =< OF3, !, true)) =>
      genstruct L M (kstruct SN MK K) S I 0 OF3).

% gencode, genloadop, should be parametrized by the class in an oop language!
gencode L M (klass CN Prs K) S I :- 
  makelabel "class" L CL, L2 is (L + 1),
  ((copystr CN CL :- true, !) =>    % string prefix for class
    (classinfo CN Prs => gencode L2 M K S I)).


% clauses used by gencode:

local genbool int -> int -> string -> texp -> texp -> kexp -> store -> (list instruction) -> o.
genbool L M OP A B K S
   [LA, LB, (basicop "cmp" AS BS nullstore),
    movop (conststore 1) DS,   
    branchop OP (slabel Truelab), nullop,
    movop (reg "g" 0) DS,  % g0 is always 0
    ilabel Truelab|Rest] :-
   makelabel "bool" L Truelab, L2 is (L + 1),
   genloadop A AS LA, % find AS, generate code ifnecessary
  ((assoces A AS :- true, !) => 
    ( genloadop B BS LB, 
      (assoces B BS :- true, !) =>   
      (  ifimpk (notfreeink A K) AS
         (ifimpk (notfreeink B K) BS
	    (getstore DS, processk DS L2 M K S Rest))
    ))).
  
local classinfo string -> (list string) -> o.
local genstruct int -> int -> kexp -> store -> (list instruction) -> int -> int -> o.
genstruct L M (kstruct SN [HK|TK] K) S I N OF3 :-
   gencode L L2 HK HS HI,
   N4 is (4 * N), OF4 is (OF3 - N4), N2 is (N + 1),
   append HI [movop HS (indirect (sreg "fp") (conststore OF4))] HI2,
   genstruct L2 M (kstruct SN TK K) S I2 N2 OF3,
   append HI2 I2 I.
genstruct L M (kstruct SN [] K) S I N OF3 :-
   gencode L M K S I.


local genfunc int -> int -> int -> kexp -> (list instruction) -> o.
% note: type info needed if functions can take object parameters.
% otherwise, won't know whose member functions to call!
genfunc N L M (kname F) [movop (reg "i" N) VS | I] :- 
  N < 5, lexlevel LL, N2 is (N + 1), !,
  getlocal VS,
  pi v\ ((permstore LL (varexp (simplevar v)) VS :- true, !) =>
          ((assoces (varexp (simplevar v)) (reg "i" N) :- true, !) =>
          genfunc N2 L M (F v) I)).   
genfunc N L M (kname F)  
   [movop (indirect (sreg "fp") (conststore OF)) VS | I] :-
  N > 4, !, lexlevel LL, N2 is (N + 1), !,
  getlocal VS,
  NA is (N - 6), NB is (4 * NA), OF is (NB + 68), 
  pi v\ ((permstore LL (varexp (simplevar v)) VS :- true, !) =>
          ((assoces (varexp (simplevar v)) (reg "i" N) :- true, !) =>
          genfunc N2 L M (F v) I)).   
genfunc N L M FB I :- 
  gencode L M FB S I0, 
  append I0 [movop S (reg "i" 0),branchop "retstore" nullstore] I.

local savedarg int -> int -> store -> o.
local argcount int -> o.
% savedarg 1st arg is "argument counter", 2nd is argument number
% 3rd is where that argument is saved.
savedarg L N (reg "o" N).  % default, no nested calls.
argcount 0.
% CPS fails for sparc!- need to expose structure

% various auxilliaries:

local gencallaux int -> int -> int -> (list instruction) -> o.
gencallaux M N AC2 [movop MS (reg "o" M)|I] :- M < N, N < 5,
  AC3 is (AC2 + M),
  savedarg AC3 M MS, !,  % location of Mth argument 
  M2 is (M + 1), gencallaux M2 N AC2 I.
gencallaux M N AC2 [].

% make sure store is recent:  E is input S is ouput
local getthestore texp -> store -> o.
getthestore E S :- assoces E S, assoces F S, !, E = F.

% loads texp into a register, killmeop indicates not necessary:
genloadop (varexp (subscriptvar V I)) TS 
     (instseq [IL, movop IS TS, basicop "sll" TS (conststore 2) TS,
         movop OFS (reg "g" 7),
         basicop "sub" (reg "g" 7) TS TS, movop (indirect R TS) TS]) :- !,
  permstore VL (varexp V) VS0, lexlevel LL,
  LD is (LL - VL), addoffset VS0 LD (indirect R OFS),
  genloadop I IS IL, 
  %  getthestore I IS,  % won't load permenents
  getstore TS. % must be a register!  need to ensure
genloadop E (reg R N) killmeop :- 
   getthestore E (reg R N), !. % already in reg
genloadop E (sreg R) killmeop :- 
   getthestore E (sreg R), !. % already in reg
genloadop (intexp N) S (movop (conststore N) S) :-  !, getstore S.
genloadop (stringexp Str) S (basicop "set" (slabel Str) nullstore S) :- !,
  getstore S.
genloadop V VS (movop S VS) :- 
  assoces V S, !, getstore VS. 
genloadop V VS (movop (indirect FP (conststore N2)) VS) :-
  permstore VL V (indirect FP (conststore N)), lexlevel LL,
  LD is (LL - VL), N2 is (N + LD), !, getstore VS.  
genloadop (varexp (simplevar V)) VS 
   (movop (indirect (reg "i" 5) (conststore N3)) VS) :-
  inclass C, 
  classinfo C Membs,  findmemb1 V Membs N, N2 is (N * 4),
  getstore VS, N3 is (0 - N2).

  

% getstore != getthestore!!!
local findmemb1 string -> (list string) -> int -> o.
findmemb1 V [V|T] 0 :- !.
findmemb1 V [H|T] N :- findmemb1 V T M, N is (M + 1).

local ofclass texp -> string -> o.
ofclass (varexp (simplevar ("this"))) C :- inclass C.  % important!

% process continuation predicate:
local processk store -> int -> int -> kexp -> store -> (list instruction) -> o.
processk DS L L kret nullstore [] :- !.
processk DS L2 M (kabs K) S Rest :-  !, % needed for uniform proof restriction
 pi v\ (((assoces v DS :- true, !), copyexp v v) => gencode L2 M (K v) S Rest).
processk nullstore L M K S R :- !, gencode L M K S R.
processk DS L M K S R :- 
 pi v\ ((assoces v DS :- true, !) => gencode L M K S R).
% pigoal still needed above so that DS is "filled with something".

%ifelse A B C :- (A, !, B); C.
ifimpk A BS G :- (A, !, (assoces dummyexp BS :- true, !) => G) ; G.
% better version: 7/11/03  (ifimpk causes error) 
ifelse A B C :- ((A, B); C), !.
%ifimpk A BS G :- ((A, (assoces dummyexp BS :- true, !) => G) ; G), !.


local addoffset store -> int -> store -> o.
addoffset (indirect R (conststore A)) D (indirect R (conststore B)) :-  
  B is (A + D).
addoffset (reg A B) D (reg A B).  % may need to remove!

local makelabel string -> int -> string -> o.
makelabel Prefix N L :-
  NS is (int_to_string N), L is (Prefix ^ NS).

% frees o register for function call: finds local register.
local freereg store -> store -> o.
freereg R R :- isfree R, !.  % no need to free
freereg (reg "o" N) LS :- N < 5,
  assoces A (reg "o" N), not (A = dummyexp), !, gettemp LS. 
local freereg3 kexp -> texp -> store -> store -> o.
% freereg3 also returns old contents.
% checks if register is needed in the continuation before freeing
freereg3 K dummyexp R R :- isfree R, !.  % no need to free
freereg3 K dummyexp R R :-
  assoces RC R, notfreeink RC K, !.    % still no need to free 
freereg3 K A (reg "o" N) LS :- N < 6,
  assoces A (reg "o" N), not (A = dummyexp), !, gettemp LS. 
% need N>=6 case!

%%%%% tests
% formcps (opexp "+" (opexp "*" (intexp 2) (intexp 4)) (intexp 6)) kret K, gencode 0 L K I.

%parsefile "test3.bob" (program A), formcps A kret B, gencode 0 M B I.
%parsefile "test3.bob" (program A), formcps A kret B, gencode 0 M B S I, optimizes1 I I2.
% notes

% introduce a getlocalstore predicate
% use o-registers and sp as freestore

% l0-l5 registers are needed to temporarily store o registers, which
% will be used for function call.  
% Then can they be used for local variables?  (let - vardec's).
% % then we need to use [fp]?  Yes.  that's how gcc does it.

% extracting string constants:
% arg 1 is label counter, arg 2 is input insts, arg 3 is output insts, 
% and arg 4 is set of asciz instructions extracted.
extractstr L [] [] [].
extractstr L [basicop "set" (slabel S) nullstore R|I] 
             [basicop "set" (slabel SL) nullstore R|I2]
             [asciz (slabel SL) S|M] :- !,
  makelabel ".str" L SL,  L2 is (L + 1),
  extractstr L2 I I2 M.
extractstr L [H|T] [H|T2] M :- extractstr L T T2 M.

%  optimization:
% memory-memory moves are transformed using %g7
optimizes1 [] [].
optimizes1 [killmeop|A] B :- !, optimizes1 A B.
optimizes1 [movop A A|B] C :- !, optimizes1 B C.
optimizes1 [movop nullstore A|B] C :- !, optimizes1 B C.
optimizes1 [movop A nullstore|B] C :- !, optimizes1 B C.
optimizes1 [ilabel A, ilabel B|C] [ilabel A,nullop|D] :- !,
	optimizes1 [ilabel B|C] D.
optimizes1 [nullop,nullop|B] C :- optimizes1 [nullop|B] C.
optimizes1 [movop A B, movop B A|C] D :- !, 
        optimizes1 [movop A B|C] D.
optimizes1 [movop A B, movop A B|C] D :- !, 
        optimizes1 [movop A B|C] D.
optimizes1 [movop (indirect A B) (indirect C D)|E] F :- !,
  optimizes1 [movop (indirect A B) (reg "g" 7),
              movop (reg "g" 7) (indirect C D)|E] F.
optimizes1 [instseq A|B] C :- !, append A B AB, optimizes1 AB C.
optimizes1 [A|H] [A|T] :- optimizes1 H T.

% notes:

% gencode probably should take a continuation argument - not anoter
% kexp but a lambday prolog goal.  This will probably in effect make
% the gencode definite clauses itself the CPS representation.


