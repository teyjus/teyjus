sig kitty.
%accum_sig tigerabsyn.
accum_sig bobparser.

kind kexp type.  % continuation passing expression

type kret kexp.   % stop
type kreturn  texp -> kexp.  % at end of functions.
type incpc kexp.  % increment program counter
type kseq (list kexp) -> kexp.
type kabs (texp -> kexp) -> kexp.  % "administrative lambda"
type kfix (string -> kexp) -> kexp.  % simpler form for now 
type klet (string -> kexp) -> kexp.  % simpler form for now 
type kname (string -> kexp) -> kexp.  % simpler form for now 
% strings are for function and class names
type kfunc string -> string -> kexp -> kexp.
type karg texp -> int -> kexp -> kexp.  %used to represent a func. argument
type kfree texp -> kexp. % used to indicate value nolonger needed.
type kvar string -> string -> texp -> kexp -> kexp.

% last kexp is the K continuation:
type kcall texp -> int -> kexp -> kexp. 
type kcall0 string -> int -> kexp -> kexp.
type kif kexp -> kexp -> kexp -> kexp -> kexp. 
type kwhile kexp -> kexp -> kexp -> kexp.
type kstruct string -> (list kexp) -> kexp -> kexp.
type cps texp -> kexp -> kexp.
type abscps texp -> kexp -> o.
type klass string -> (list string) -> kexp -> kexp.
type inclass string -> o.

% formcps is always relative to some "current continuation"
%   - moved to second parameter position to improve indexing 6/03
type formcps texp -> kexp -> kexp -> o.

type atomic texp -> o.
type primitive texp -> o.

% a cps term is a computation AND an continuation.
% should be different than a stand-alone kabs function.

type kopy kexp -> kexp -> o.
type notfreeink, okkursin texp -> kexp -> o.

% manipulate the source absyn:

type numoccur int -> texp -> kexp -> o.




