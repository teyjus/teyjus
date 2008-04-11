sig lyaccshares.

% Predicates required by grammar files
kind gs type.     % grammar symbol type.
type iconst int -> gs.
type sconst string -> gs.
type id string -> gs.    % universal identifier
type rule decl -> o -> decl.  % rule clause.  The first param. is the
type ==> gs -> (list gs) -> decl.   % The |- symbol for grammars
infixl ==> 10.
type binaryop gs -> gs -> gs -> string -> int -> o.  % precedence and type
type implicitop gs -> gs -> string -> int -> o.      % juxtaposition
type unaryop gs -> gs -> int -> o.
type terminal gs -> o.
type non_terminal gs -> o.
type start_symbol gs -> o.
type cfg (list decl) -> o.   % grammar rules
type test int -> gs -> o.
type ntnum int -> o.
type first gs -> gs -> o.
type letter, letnum, alltyp string -> o.
type terminal_list (list gs) -> o.
type nonterm_list (list gs) -> o.
type comment_list (list string) -> o.
type ary0 gs -> gs.
type ary1 (A -> gs) -> gs.
type ary2 (A -> B -> gs) -> gs.
type ary3 (A -> B -> C -> gs) -> gs.
type ary4 (A -> B -> C -> D -> gs) -> gs.
type freshcopy gs -> gs -> o.   % avoids instantiating nonterminals!
type comment string -> o.

% Predicates required for internal parser generating
type bofs, eofs, sprime,bols,eols   gs.   % beginning and end of file, beginning and end of line, extra start symbol.
type eof_symbol string -> o.
type linenum int -> gs.  
kind decl type.	  % declaration type for grammar rules.
kind ch type.   % candidate handle type
type printname A -> string -> gs -> o.
type finderrline (list gs) -> (list gs) -> o.
type print_tokens int -> (list gs) -> o.
type tokchar string -> o.

% Predicates needed at the top level
type genparser A -> B -> o.
type parseline gs -> A -> o.
type parsefile string -> A -> o.
type parse (list gs) -> (list gs) -> gs -> string -> o.
type startparse gs -> (list gs) -> (list gs) -> gs -> string -> o.
type generate_fc A -> o.
type output_fcs out_stream -> (list gs) -> o.