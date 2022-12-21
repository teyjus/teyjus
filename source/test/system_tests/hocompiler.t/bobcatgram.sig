sig bobcatgram.
accum_sig lambdayacc, bobabsyn.

% kind gs type - type of "grammar symbols"
% terminal symbols
% iconst, id, and sconst are already provided in lambdayacc.

type comma, colon, semicolon, lparen, rparen, lbrack, rbrack  gs.
type lbrace, rbrace, dott, plust, minust, timest, dividet, eqt, neqt  gs.
type ltt, let, gtt, get, andt, ort, assignt, arrayt, ift, thent, elset  gs.
type whilet, fort, tot, dot, lett, intok, endt, oft, breakt, nilt  gs.
type classt, functiont, vart, typet, errort, ldbrace, rdbrace gs.
type publict, privatet gs.

% non terminals with attribute as parameters.
% initial version does not use hoas, based on a Java_cup version:

type program texp -> gs.   % start symbol
type te texp -> gs.
type dec tdec -> gs.
type decs (list tdec) -> gs.
type tydec_gs tdec -> gs.
type ty_gs ttype -> gs.
type tfields (list tfield) -> gs.
type vdec_gs tdec -> gs.	% vardec 
type fdec_gs tdec -> gs.	% functiondec
type cdec_gs tdec -> gs.	% classdec
type lvalue tvar -> gs.
type args (list texp) -> gs.
type expseq (list texp) -> gs.
type moreinits (list tfieldexp) -> gs.
type index tvar -> gs.


% Added because of use of SBRC grammars:

type tyfcomma, acomma gs.  % , in tfields
type fundech string -> gs.  % function header "function f("
%type funcallh stringg -> gs. % function call f(...
type funcallh tvar -> gs. % MODIFIED FOR BOBCAT
type teqt gs.	   % = in type declarations
type tydechd	string -> gs.
type createrec 	 string -> gs.	% recordexp header id{
type eqtinit texp -> gs.
type ty_gshd	gs.


% additional semantic action predicates:
type mkarrayexp tvar -> texp -> texp -> texp -> o.
type formlet (list tdec) -> texp -> texp -> o.
%type formnabs (list tdec) -> texp -> texp -> o.
type formnabs (list tdec) -> (list tdec) -> texp -> tdec -> texp -> o.
type formfix string -> (list tfield) -> ttype -> texp -> tdec -> o.
type formfix2 texp -> texp -> (list tfield) -> (list ttype) -> o.


% Modifications because of non-implemenation of nullibility check:

% tfields, args must end in rparen
