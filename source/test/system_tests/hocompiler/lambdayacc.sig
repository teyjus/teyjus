sig lambdayacc.


kind gs type.     % grammar symbol type.
kind decl type.	  % declaration type for grammar rules.
type rule decl -> o -> decl.  % rule clause.  The first param. is the
		      % CFG rule, the second determines attributes.
type ==> gs -> (list gs) -> decl.   % The |- symbol for grammars
infixl ==> 10.

type terminal gs -> o.
type non_terminal gs -> o.
type start_symbol gs -> o.
type cfg (list decl) -> o.   % grammar rules
type getrule decl -> o.

type bofs, eofs, sprime   gs.   % begin and end of file, extra start symbol.
type id string -> gs.    % universal identifier
type iconst int -> gs.
type sconst string -> gs.
type test int -> gs -> o.
type ntnum int -> o.
type first gs -> gs -> o.
type find_first int -> gs -> gs -> o.
type before, follow gs -> (list gs) -> o.
type epsilon gs.
type first_stage int -> gs -> gs -> o.
type gen_first out_stream -> int -> o.
type head (list gs) -> gs -> o.
type last (list gs) -> gs -> o.
type look gs -> (list gs) -> gs -> o.

kind ch type.   % candidate handle type
type handle gs -> decl -> gs -> ch.
type gen_handles (list ch) -> (list ch) -> (list ch) -> o.
type context_search gs -> gs -> (list gs) -> (list gs) -> (list gs) -> o.
type make_handles (list gs) -> (list ch) -> o.
type rule_to_h gs -> gs -> decl -> ch -> o.
type getprods gs -> (list decl) -> o.
type freshcopy gs -> gs -> o.   % avoids instantiating nonterminals!
type brcvh (list ch) -> o.
type convert_to_sets ch -> ch -> o.

% added for fbrc:
type lcontext_search gs -> (list gs) -> (list gs) -> (list gs) -> o.
type rcontext_search gs -> (list gs) -> (list gs) -> (list gs) -> o.
type fsbrc (list ch) -> o.
type fprepare (list decl) -> (list ch) -> o.
type lgen_shandles (list ch) -> (list ch) -> o.
type rgen_shandles (list ch) -> (list ch) -> o.


%-------------------------  Simple BRC    ---------

type shandle (list gs) -> decl -> (list gs) -> o.
type shand (list gs) -> decl -> (list gs) -> ch.
type sbrc (list ch) -> o.
type sprepare (list decl) -> (list ch) -> o.
type gen_shandles (list ch) -> (list ch) -> o.
type make_shandle decl -> ch -> o.

type rrdeterministic (list ch) -> o.
type srdeterministic (list ch) -> (list ch) -> o.
type rr_conflict ch -> ch -> o.
type sr_conflict (list gs) -> (list gs) -> gs -> ch -> o.
type srprocess (list gs) -> (list gs) -> (list gs) -> (list ch) -> o.
type genparser_sbrc string -> o.
type genparser string -> string -> string -> o.
type gen_reduce out_stream -> ch -> o.
type gen_shift out_stream -> (list gs) -> (list gs) -> gs -> o.
type srdecision out_stream -> (list gs) -> gs -> o.
type decresolve (list gs) -> gs -> o.
type stream_name out_stream -> o.


% signature for generated parser:
type parse (list gs) -> (list gs) -> gs -> string -> o.
type binaryop gs -> gs -> gs -> string -> int -> o.  % precedence and type
type implicitop gs -> gs -> string -> int -> o.      % juxtaposition
type unaryop gs -> gs -> int -> o.


% utilities
type append (list A) -> (list A) -> (list A) -> o.
type member A -> (list A) -> o.
type oncememb A -> (list A) -> o.
type nth int -> A -> (list A) -> o.
type length (list A) -> int -> o.
type reverse (list A) -> (list A) -> o.
type map1 (A -> B) -> (list A) -> (list B) -> o.
type map_pred2 (A -> B -> o) -> (list A) -> (list B) -> o.
type allare (A -> o) -> (list A) -> o.
type filter (A -> o) -> (list A) -> (list A) -> o.
type preplace (A -> B -> o) -> A -> B -> (list A) -> (list B) -> o.

% the open-list utility:
type openlist (list A) -> o.
type close_list (list A ) -> o.
type openadd A -> (list A) -> o.
type openmemb A -> (list A) -> o. 
type opennil A -> o.
type openmap2 (A -> B -> o) -> (list A) -> (list B) -> o.



%%%% Tokenizer:
type printname A -> string -> gs -> o.
type yyinitial int -> in_stream -> (list gs) -> o.
type yyalphanum, yyspecial    string -> in_stream -> gs -> o.
type yycomment in_stream -> o.
type yystring string -> in_stream -> gs -> o.
type decdigit, alphab, alphanum, white_space  string -> o.
type parseline A -> o.
type parsefile string -> A -> o.
type yynat string -> in_stream -> gs -> o.
type natconst int -> gs.  % grammar symbol for natural numbers.
type parseint string -> int -> o.
type parseint_aux string -> int -> int -> int -> int -> o.
type eof_symbol string -> o.

% special grammar symbol to record line numbers.
% used only to report current line number.
type linenum int -> gs.  
type finderrline (list gs) -> (list gs) -> o.
type print_tokens int -> (list gs) -> o.


