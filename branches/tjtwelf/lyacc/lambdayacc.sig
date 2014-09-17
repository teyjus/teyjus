%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LambdaYacc
%
% Changes made by Kamal Aboul-Hosn:
% 
% 1/2002 : Split LambdaYacc into multiple files, described below
% 1/2002 : Used module files for lists and filtering
% 1/2002 : A second genparser clause to handle command-line program compilation (lambdayacc.mod)
% 1/2002 : Added support for comments of the form /* */ (tokenizer.mod)
% 2/2002 : No longer need to end a file with a "$".  (tokenizer.mod)
% 2/2002 : An underscore can be used to begin a variable name (tokenizer.mod)
% 2/2002 : Special symbols can be longer than two characters (tokenizer.mod)
% 2/2002 : Special symbols without a printname clause are treated as ids (tokenizer.mod)
%		- The special symbol can begin only with non alphanumeric characters
%		- In the file being parsed, any symbols parsed as ids must have a space after them
% 2/2002 : Modified gen_first predicate to reduce nondeterminism.  Required new predicates gather_tlist
%	   and print_tlist (parser.mod)
% 2/2002 : Modified fsbrc predicate to reduce nondeterminism.  fsbrc now calls the new predicate mgen_handles, which
%	   uses two new predicates "findlbs" and "findlas" (parser.mod)
% 3/2002 : Modified shift-reduce conflict detection.  Redefined sr_conflict clause (parser.mod)
% 3/2002 : Modified gen_first clause to print out each first declaration once using new predicate
%	   remove_copies (parser.mod)
% 3/2002 : Added the ability to have different and multiple single-line comment characters (tokenizer.mod)
%	   Grammars must now include a declaration of the form "comment X :- member X A.", where A is the
%	   list of characters used to recognize lines as comments.  They can each be up to two characters long	
% 3/2002 : Improved token recognition.  One can now specify characters that can be in tokens.  This is done through
%	   the "tokchar A" predicate in the grammar.  If no predicate is specified, the program will assume that 
%	   letters and numbers and the underscore are valid.  Three options are available for easy use:
%		letter A : All letters, capital and lowercase, and the underscore
%		letnum A : Like letter A, but with the addition of numbers
%		alltyp A : All characters a la Lambda Prolog
%	   One can add specific characters to ignore or include beyond these.  For instace, the tokchar predicate for
%	   the Lambda Prolog grammar is:
%		tokchar S :- alltyp S, C is string_to_int S, 
%			  not (C = 46; C = 44; C = 41; C = 40; C = 59; C = 91; C = 93; C = 124; C = 58; C = 92; C = 34; C = 126).
%	   (tokenizer.mod, lyaccshares.mod)
% 3/2002 : Removed printing of "special" rules in the case that a shift is chosen.  The only time the special case is needed is
%	   when it is necessary to jump over a "shift" rule to get to a "reduce" rule, which appears later (parser.mod)
% 3/2002 : Passed the "shifting" rule being checked in srdeterministic to srprocess and then to sr_conflict so it could be printed
%	   when an error is detected (parser.mod)
% 3/2002 : Added support for null productions (parser.mod)
% 3/2002 : Fixed error in unary operator "parse" clause mentioned by Dr. Liang (lambdayacc.mod)
% 4/2002 : Changed reduce-reduce conflict detection to use first clauses in same way as shift-reduce conflict detection (parser.mod)
% 4/2002 : Automatic generation of freshcopy clauses with commandline argument to shell script (lambdayacc.mod)
% 4/2002 : Fixed problems in Shift-Reduce conflict detection whereby it would not detect all of the conflicts (parser.mod)
% 4/2002 : Changed parseline predicate so that one may specify the starting symbol and parse a line of any grammar symbol (lambdayacc.mod)
% 4/2002 : Made predicates local that did not need to be called outside the specific module (all files)
% 4/2002 : Removed predicates that are no longer used (parser.mod)
% 4/2002 : Improved the efficiency of the decresolve predicate by passing it the list of first clauses, similar to sr_conflict,etc.
% 4/2002 : Removed utilities modules, since its predicates are no longer used
% 4/2002 : Passed output stream as an argument to predicates instead of using "stream_name OS" for sake of consistency
%
%
%%%%%%%%%%%%%%%%%

sig lambdayacc.
accum_sig lyaccshares, blists.

type genparser A -> B -> o.
type parseline gs -> A -> o.
type parsefile string -> A -> o.
type parse (list gs) -> (list gs) -> gs -> string -> o.
type startparse gs -> (list gs) -> (list gs) -> gs -> string -> o.
type generate_fc A -> o.