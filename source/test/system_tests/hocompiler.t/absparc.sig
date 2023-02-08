sig absparc.
accum_sig kitty.
% abstract sparc assembly language

kind instruction type.

kind store type.

type reg string -> int -> store. % e.g. reg "l" 0   %l0
type sreg string -> store.  % special register like fp.
type basicop string -> store -> store -> store -> instruction.
type movop store -> store -> instruction.
type branchop string -> store -> instruction.
type indirect store -> store -> store.   % second store is offset.
type instseq (list instruction) -> instruction.
type ilabel string -> instruction.
type slabel string -> store.
type conststore int -> store.
type nullstore store.
type killmeop instruction.  % to be deleted later.
type nullop instruction.
type permstore int -> texp -> store -> o.
% in basicop, third store is always destination.
% nullstore used to fill in unused slots.
type lexlevel int -> o.
type asciz store -> string -> instruction.
type extractstr int -> (list instruction) -> (list instruction) -> (list instruction) -> o.
type optimizes1 (list instruction) -> (list instruction) -> o.

% set macro used in place of sethi, or lo
% sethi, orlo are basic ops in this language


% for the code generator:

type assoces texp -> store -> o.

% associates storage with expression.  Use "dummyexp" to indicate freeness.
%assoc A dummyexp.   % default.
% string associations are (assoc  (slabel "string1") (stringexp "hello")).

type gettemp, getstore, getlocal store -> o.  % find empty store.
%getstore could return either a (reg t n) or a 

type freestore, localstore store -> o.   % static list.

type gencode int -> int -> kexp -> store -> (list instruction) -> o.
type ifelse o -> o -> o -> o.

type genloadop texp -> store -> instruction -> o.
type ifimpk o -> store -> o -> o.





