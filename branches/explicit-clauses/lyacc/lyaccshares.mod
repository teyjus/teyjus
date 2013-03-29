module lyaccshares.
accumulate blists.

% Built-in sets of characters to be considered parts of tokens
% All letters, capital and lowercase, and the underscore
letter A :- X is (string_to_int A), ((X > 64, X < 91); (X > 96, X < 123); X = 95).

% All letters, all numbers, and the underscore
letnum A :- X is (string_to_int A), ((X > 64, X < 91); (X > 96, X < 123); (X > 47, X < 58); X = 95).

% All visible characters
alltyp A :- X is (string_to_int A), (X > 32, X < 127).


% Clauses to interpret grammar information on comments, terminals, and non-terminals
comment X :- comment_list L, member X L.
terminal X :- terminal_list A, member X A.
non_terminal X :- nonterm_list A, member (ary0 X) A,!.
non_terminal (X _) :- nonterm_list L, member (ary1 X) L,!.
non_terminal (X _ _) :- nonterm_list L, member (ary2 X) L,!.
non_terminal (X _ _ _) :- nonterm_list L, member (ary3 X) L,!.
non_terminal (X _ _ _ _) :- nonterm_list L, member (ary4 X) L,!.

% Length of the nonterminal list
ntnum X :- nonterm_list A, length A N, X is N + 1.

% freshcopy clauses for assumed built in grammar symbols
freshcopy (iconst A) (iconst B) :- !.
freshcopy (sconst A) (sconst B) :- !.
freshcopy (id A) (id B) :- !.


% added 11/99: report line number parse failed:
finderrline S [] :- !, print "Error parsing last line\n",
		    print "\nTop of Stack =  ", print_tokens 1 S.
finderrline S [(linenum N)|T] :- !, print "** Error parsing line ",
				 NF is (N + 1),    % offset (hack)
				 printterm std_out NF, print "\nTop of Stack =  ",
				 print_tokens 1 S.
finderrline S [H|T] :- finderrline S T.