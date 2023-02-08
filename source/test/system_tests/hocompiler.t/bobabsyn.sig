% Signature file for abstract syntax of Bobcat language.

sig bobabsyn.

kind texp type.  % basic type of all tiger expressions.
kind tvar type.  
kind ttype type. % meta-level type of tiger types
kind tdec type.
kind tfieldexp, tfield type.

% Based on "Modern Compiler Implementation in Java" page 103.

type simplevar string -> tvar.  	% x
type fieldvar tvar -> string -> tvar.	% x.y
type subscriptvar tvar -> texp -> tvar.	% x[i+j]

type varexp tvar -> texp.
type nilexp, breakexp 	texp.
type intexp	int -> texp.
type stringexp	string -> texp.
type callexp	tvar -> (list texp) -> texp.  %string changed to tvar (bob)
type opexp	string -> texp -> texp -> texp.  % using strings.
type dummyexp	texp.			% dummy expression (null)
type recordexp	string -> (list tfieldexp) -> texp.
type seqexp  (list texp) -> texp.
type assignexp tvar -> texp -> texp.
type ifexp texp -> texp -> texp -> texp.	% use dummy
type whileexp texp -> texp -> texp.
type forexp tdec -> texp -> texp -> texp.
type arrayexp string -> texp -> texp -> texp.
type fletexp (list tdec) -> texp -> texp.	 % foas version

type namety string -> ttype.
type recordty (list tfield) -> ttype.
type arrayty string -> ttype.
type dummytype ttype.

type typedec string -> ttype -> tdec.
type vardec string -> ttype -> texp -> tdec.
type functiondec string -> (list tfield) -> ttype -> texp -> tdec.
type classdec string -> (list tfield) -> (list tdec) -> tdec.

% dependent types would be convenient.

% higher-order version records arity and target type.
% must implement Currying here
type hfuncdec string -> int -> ttype -> (texp -> texp) -> tdec.

type etpair texp -> ttype -> tfield.   % exp - type pair
type eepair texp -> texp -> tfieldexp. % exp - exp pair
type depair tdec -> texp -> texp.

% Higher order abstract syntax version.
% The extra "absterm" is necessary for Currying.
% All declarations are for strings, which will be abstracted over.
% Function declaration must include argument and return types.
% letexp expects nameterms. fixptdec expects absterms.
% The callexp form will be retained in abstract syntax - it will be
% curried during interpretation/compilation.


type letexp tdec -> texp -> texp.     
%type letexp tdec -> (string -> texp) -> texp.
type fixptdec string -> (string -> texp) -> (list ttype) -> ttype -> tdec.
type absterm (texp -> texp) -> texp.
type nameterm (string -> texp) -> texp.

% needed for allowing mutually recursive declarations:
type decabs (string -> tdec) -> tdec.
type declist (list tdec) -> tdec.   % type coersion 

type coercexp tdec -> texp.

% meta-level predicates for reasoning with abstract syntax:

type copyexp texp -> texp -> o.
type copyty ttype -> ttype -> o.
type copyvar tvar -> tvar -> o.    % don't really need to be this complicated
type copystr string -> string -> o.
type copydec tdec -> tdec -> o.

type hastype texp -> ttype -> o.
% etc ...

% moved from kitty.sig 8/03
type fixdec2 string -> texp -> (list ttype) -> ttype -> tdec.
type let2 texp -> texp. 
type changelet (list string) -> tdec -> texp -> texp -> o.
type changelet0 texp -> texp -> o.


% simple let expression for testing
type let0 texp -> (texp -> texp) -> texp.
type lamexp (texp -> texp) -> texp.
