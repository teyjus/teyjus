module parser.

% A handle itself
type shand (list gs) -> decl -> (list gs) -> ch.


type gen_first out_stream -> int -> (list o) -> o.
type gather_terms (list decl) -> (list decl) -> (list o) -> (list o) -> o.
type gather_tlist gs -> (list decl) -> (list decl) -> (list gs) -> (list gs) -> o.
type remove_copies (list o) -> (list o) -> o.
type print_tlist gs -> (list gs) -> (list o) -> (list o) -> o.
type printterms (list o) -> out_stream -> o.

type fsbrc (list ch) -> o.
type mgen_handles gs -> (list decl) -> (list decl) -> (list ch) -> o.
type findlbs int -> gs -> gs -> (list decl) -> (list decl) -> (list gs) -> o.
type findlas int -> gs -> gs -> (list decl) -> (list decl) -> (list gs) -> o.
type index_of gs -> (list gs) -> int -> o.
type find_item gs -> (list gs) -> int -> int -> o.
type printhandles (list ch) -> o.
type rrdeterministic (list ch) -> (list o) -> o.
type rr_conflict ch -> ch -> (list o) -> o.
type srdeterministic (list ch) -> (list ch) -> (list o) -> out_stream -> o.
type sr_conflict decl -> (list gs) -> (list gs) -> gs -> ch -> (list o) -> out_stream -> o.
type srprocess decl -> (list gs) -> (list gs) -> (list gs) -> (list ch) -> (list o) -> out_stream -> o.
type decresolve (list gs) -> gs -> (list o) -> o.
type srdecision out_stream -> (list gs) -> gs -> (list gs) -> o.
type gen_reduce out_stream -> ch -> o.
type gen_shift out_stream -> (list gs) -> (list gs) -> gs -> o.



%--------------------------------------------------------------------------------------

non_terminal sprime.      % Internal start symbol of grammar.
terminal bofs.            % Internal beginning of file symbol   
terminal eofs.            % Internal end of file symbol
terminal bols.		  % Internal beginning of line symbol
terminal eols.		  % Internal end of line symbol

% Find the "first" clauses, removes duplicates  and print them out
gen_first OS M Printed2 :- cfg Gs, gather_terms Gs Gs nil Printed, remove_copies Printed Printed2, 
			   printterms Printed2 OS, output OS "first X X :- terminal X.\n\n".

% Gather the first grammar symbol from each rule in the grammar
% If it is a terminal, just add it.  If it is a non-terminal,
% add it and then see what the first symbols are for that non-terminal
gather_terms nil _ _ nil.
gather_terms ((rule (N ==> nil) A)::B) Gs Printed Printfnl2 :- gather_terms B Gs Printed Printfnl2.
gather_terms ((rule (N ==> [T|Ts]) A)::B) Gs Printed Printfnl2 :- 
							  ((terminal T, not (member (first N T) Printed), 
						  	  Print2 = ((first N T)::Printed),PPrinted = [(first N T)],!);
							  (not (N = T), gather_tlist T Gs Gs nil Terms,
							  print_tlist N Terms Printed PPrinted,Print2 = Printed);
							  (N = T, Print2 = Printed,Pprinted = nil)),
							  ((not (N = T), member (rule (T ==> nil) _) Gs,
							  gather_terms ((rule (N ==> Ts) A)::B) Gs Print2 Printlst);
							  gather_terms B Gs Print2 Printlst),
							  append Printlst PPrinted Printfnl2.

% Gather a list of terms for "first" clauses from non-terminals
gather_tlist B nil L M nil :- !.
gather_tlist N1 ((rule (N ==> nil) A)::B) Gs AVNT L :- gather_tlist N1 B Gs AVNT L,!.
gather_tlist N1 ((rule (N1 ==> [T|Ts]) A)::B) Gs AVNT L :- terminal T, gather_tlist N1 B Gs AVNT L3, 
						           L = (T::L3),!.
gather_tlist N1 ((rule (N1 ==> [T|Ts]) A)::B) Gs AVNT L3 :- not (N1 = T), 
							    not (member T AVNT), gather_tlist T Gs Gs (N1::AVNT) L1,
							    gather_tlist N1 B Gs AVNT L2,!,append L1 L2 L3.
gather_tlist N1 ((rule (N1 ==> [T|Ts]) A)::B) Gs AVNT L3 :- gather_tlist N1 B Gs AVNT L3,!.
gather_tlist N ((rule (N1 ==> [T|Ts]) A)::B) Gs AVNT L :-   gather_tlist N B Gs AVNT L.

% Remove all duplicate "first" clauses
remove_copies nil nil.
remove_copies [(first N T)|Rest] Rest2  :- freshcopy N Np, (member (first Np T) Rest2), remove_copies Rest Rest2,!.
remove_copies [(first N T)|Rest] [(first N T)|Rest2] :- remove_copies Rest Rest2,!.

% Print all the terms found in gather_tlist to a list
print_tlist _ nil _ nil.
print_tlist N (A::B) Printed Printlst2 :- ((not (member (first N A) Printed), 
				   Print2 = ((first N A)::Printed), Printlst2 = [(first N A)|Printlst]);
				   (Print2 = Printed, Printlst2 = Printlst)),print_tlist N B Print2 Printlst.

% Print all first clauses (without copies) to the .mod file
printterms nil _.
printterms [H|T] OS :- printterm OS H, output OS ".\n", printterms T OS.


%%%%%%%%%%%%%%% Create the parser handles
fsbrc Shands :- cfg Grammar, cfg Grammar2, start_symbol A, 
		mgen_handles A Grammar [(rule (sprime ==> [bofs,A,eofs]) (true))|Grammar2] Shands.

% Generate all handles
mgen_handles _ nil _ nil :- !.
mgen_handles LHS  [H|T] Grammar [NewHand|Handles] :- H = (rule (LHS1 ==> RHS) _), 
						     LHS1 = LHS, findlbs 0 LHS LHS Grammar Grammar LH,  % Check to see if the last LHS and this one are the same
						     findlas 0 LHS LHS Grammar Grammar RH, 		% If so, proceed, otherwise we need to note the new one
					             !, mgen_handles LHS T Grammar Handles,
						     NewHand = (shand LH H RH),!.
mgen_handles LHS1 [H|T] Grammar [NewHand|Handles] :- H = (rule (LHS ==> RHS) _), not (LHS = LHS1),	% A new LHS
						     findlbs 0 LHS LHS Grammar Grammar LH,  
						     findlas 0 LHS LHS Grammar Grammar RH, 
					             !, mgen_handles LHS T Grammar Handles,
						     NewHand = (shand LH H RH).

% Find the lookbacks for a rule
findlbs _ _ _ nil _ nil.
findlbs I LHSS LHS [H|T] FG FList :- H = (rule (LHSSS ==> RHS) _), index_of LHS RHS N, 	 % Find LHS in the RHS of each rule
				 not (N = 1), N >= I, length RHS L,			 % Is it the first item?  A second occurence?
				 Nnext is N - 1, nth_item Nnext X RHS,			 %  if the first, can be no lookback in rule
				 findlbs 1 LHSS LHS T FG List1, 			 % Find lookbacks in rest of rules
				 ((member (rule (X ==> nil) _) FG,nth_remove Nnext RHS RHS2, 
				 findlbs I LHSS LHS [rule (LHSSS ==> RHS2) _] FG Nulllist,!);Nulllist = nil),
				 ((freshcopy LHS LHSNew, index_of LHSNew RHS N1, N1 > N,     % Does the LHS appear a second time? 
				 findlbs N1 LHSS LHSNew [H|T] FG List2, join List1 List2 List3);List3 = List1),  % If so, do search again with new starting index
				 ((not (member X List3), freshcopy X X2, List = [X2|List3]);List = List3),join List Nulllist FList,!.     % Generate list
findlbs I LHSS LHS [H|T] FG List :- H = (rule (LHS1 ==> RHS) _), index_of LHS RHS N, N = 1,  % If LHS is the first item, must check lookback of this rule
				 not (freshcopy LHS LHS2, freshcopy LHS1 LHS2),		     % Make sure the LHSes aren't the same
				 not (freshcopy LHS1 LHS3,freshcopy LHSS LHS3),		     % Make sure we aren't getting into a loop of checking
				 findlbs 1 LHSS LHS1 FG FG List1, findlbs 1 LHSS LHS T FG List2,
				 join List1 List2 List, !.
findlbs I LHSS LHS [H|T] FG List :- findlbs I LHSS LHS T FG List.			     % No occurences of LHS

% Find the lookaheads for a rule
findlas _ _ _ nil _ nil.
findlas I LHSS LHS [H|T] FG FList :-  
				   H = (rule (LHSSS ==> RHS) _), index_of LHS RHS N, 			     % Find LHS in the RHS of each rule
				 length RHS L, not (N = L), N >= I,					     % Is it the last item?  A second occurence?
				 Nnext is N + 1, nth_item Nnext X RHS,	  	    			     %  if last, can be no lookahead
				 findlas 1 LHSS LHS T FG List1,						     % Find lookaheads in rest of rules
				 ((member (rule (X ==> nil) _) FG,nth_remove Nnext RHS RHS2, 
				 findlas I LHSS LHS [rule (LHSSS ==> RHS2) _] FG Nulllist,!);Nulllist = nil),
				 ((index_of LHS RHS N1, N1 > N, %not (N1 = L),				     % Is there a second occurence of LHS?
				 findlas N1 LHSS LHS [H|T] FG List2, join List1 List2 List3);List3 = List1), % If so, do search again with new starting index
				 ((not (member X List3), freshcopy X X2,List = [X2|List3]);List = List3),
				 join List Nulllist FList,!.  % Generate lists
findlas I LHSS LHS [H|T] FG List :- H = (rule (LHS1 ==> RHS) _), 
				 index_of LHS RHS N, length RHS L, N = L, 	% If LHS is the last item, must check lookahead of this rule
				 not (freshcopy LHS LHS2, freshcopy LHS1 LHS2), 	% Make sure the LHSes aren't the same
				 not (freshcopy LHS1 LHS3,freshcopy LHSS LHS3),		% Make sure we aren't getting into a loop of checking
				 findlas 1 LHSS LHS1 FG FG List1, findlas 1 LHSS LHS T FG List2,
				 join List1 List2 List, !.
findlas I LHSS LHS [H|T] FG List :- findlas I LHSS LHS T FG List.		% No occurences of LHS

index_of A List N :- find_item A List N 1.

find_item _ nil _ _ :- !, fail.
find_item LH (LH2::Rest) N M :- freshcopy LH LH3, LH3 = LH2, N is M.
find_item A (B::Rest) M N :- N1 is N + 1, find_item A Rest M N1.


% Prints out handles for debugging
printhandles nil.
printhandles [(shand A B C)|T] :- printterm std_out A,print "\n",printterm std_out B,
				  print "\n",printterm std_out C,print "\n\n",printhandles T.


%---- R-R and S-R conflict detection:  (rememb no epislons yet!)
rrdeterministic nil _.
rrdeterministic [H|T] Firsts :- forevery (X\ (not (rr_conflict H X Firsts))) T, rrdeterministic T Firsts.

rr_conflict (shand LB1 (rule (LHS ==> RHS) Act) LA1)
            (shand LB2 (rule (LH2 ==> RH2) Ac2) LA2) Firsts :- 
					reverse RHS RRH, reverse RH2 RR2,
					append RRH [B1|_] VP1,        % first viable prefix
					append RR2 [B2|_] VP1,       % second viable prefix
					memb B1 LB1, memb B2 LB2,         % pick two lookbacks
					memb A1 LA1, memb A2 LA2,     % even after lookahead consideration?    
					((terminal A1,terminal A2, A1 = A2, A = A1);(freshcopy A1 Anew, (member (first A1 A) Firsts, 
					freshcopy A2 Anew2, member (first Anew2 A) Firsts))),
					print "Reduce-Reduce conflict exists between\n",
					printterm std_out (LHS ==> RHS), print "  and  ",
					printterm std_out (LH2 ==> RH2), 
					print "\n when top of stack is of form  ", printterm std_out VP1,
					print "\n and lookahead symbol is ", printterm std_out A. 

% usage: srdeterministic out_stream H H, where H are the shandles
srdeterministic nil _ _ _.
srdeterministic [H|T] Handles Firsts OS :- H = (shand LB (rule (LHS ==> RHS) Act) LA), 
				        srprocess (LHS ==> RHS) LB nil RHS Handles Firsts OS, srdeterministic T Handles Firsts OS.

srprocess Rule LB H nil Handles Firsts OS.
srprocess Rule LB nil [H] Handles Firsts OS.   % no shift needed
srprocess Rule LB nil [H1,H2|T] Handles Firsts OS :- srprocess Rule LB [H1] [H2|T] Handles Firsts OS.
srprocess Rule LB [G|Gamma] [Next|Rest] Handles Firsts OS :-  
					(forevery (X\ (not (sr_conflict Rule LB [G|Gamma] Next X Firsts OS))) Handles),
					gen_shift OS LB [G|Gamma] Next,
					srprocess Rule LB [Next,G|Gamma] Rest Handles Firsts OS.


sr_conflict Rule LB Gamma Next (shand LB1 (rule (LHS1 ==> RHS1) _) LA1) Firsts OS :-
				freshcopy Next Nextf, memb Nextf LA1, 		  % Check lookahead symbols
				reverse RHS1 RHSR, !,				  % Reverse the RHS only once	
				((append RHSR [Sym|_] Gamma, freshcopy Sym Symf,  % Is the reversed righthand side
				member Symf LB1, memb LBs LB, VP2 = Gamma);			  % and its LB symbol in Gamma?
				(member LBf LB1, freshcopy LBf LBs,		  % Is the RHS Gamma itself?
				RHSR = Gamma, memb LBs LB, VP2 = Gamma); 
				(append Gamma [LBs2|_] RHSR, freshcopy LBs2 LBs, memb LBs LB, member Symf LB1,append RHSR [Symf|_] VP1, VP2 = RHSR)
				),
				((terminal Nextf, freshcopy Nextf LAC);
				(freshcopy Nextf Nextfst, memb (first Nextfst LAC) Firsts)), % LA is non-terminal; find in firsts lists
				not (decresolve RHS1 LAC Firsts),				     % Try to resolve; negation
				append Gamma [LBs|_] VP1,				     %   to find all conflicts
				print "**Shift-Reduce conflict with  ", printterm std_out Rule, print " and ", printterm std_out (LHS1 ==> RHS1),
				print "  exists \n when top of stack is of form  ", printterm std_out VP1,
				print "\n and lookahead symbol is  ", printterm std_out LAC,
				srdecision OS VP2 Next LB1,
				print "\n", fail.    					     % the failure will allow it to 
											     % detect all sr conflicts.

decresolve RH2 A Firsts :-  append U [Ea,OP1,Eb] RH2,       % operator may be at end of production 
			    binaryop OP1 Ea Eb Assoc1 Prec1, 
			    (binaryop A Eb Ed Assoc2 Prec2; 
			    (implicitop Eb Ed2 Assoc3 Prec3, member (first Ed2 A) Firsts);
			    unaryop A Ed Prec4).
			    %  print "\n *Conflict resolved by operator precedence/associativity.\n".
			    % if this is the case, know the default special rules will work.
decresolve [Ea,Eb] A Firsts :- implicitop Ea Eb Assoc1 Prec1,   % try to resovle by implicit operator assoc and prec
			       (member (first Eb A) Firsts; binaryop A Eb Ed Assoc2 Prec2; unaryop A Ed Prec3).
decresolve RHS A Firsts :- reverse RHS RRS, RRS = [Ea,Op|TT],   % try to resovle by unary operator assoc and prec
			   unaryop Op Ea Prec1,
			   ((binaryop A Ea Ed Assoc2 Prec2); 
			   (implicitop Ea Ed2 Assoc3 Prec3, member (first Ed2 A) Firsts)).

srdecision OS VP A LB :- memb LB1 LB, append VP [LB1|_] VP1, print "\nWhen the stack is of the form\n\t", printterm std_out VP1, 
			 print "\ndo you want to (s)hift or (r)educe (default is shift): ",
			 input_line std_in SR,(((SR = "r\n";SR = "reduce\n"),(
			 output OS "parse (", printterm OS VP1,
			 output OS ") [", printterm OS A, 
			 output OS "|Beta] Result \"special\" :- !, ",
			 output OS "\n\tparse (", printterm OS VP1,
			 output OS ") [", printterm OS A, 
			 output OS "|Beta] Result ",             % will it be shift or reduce?
			 (((SR = "r\n"; SR = "reduce\n"),      % try parse again with reduce
			 output OS "\"reduce\".\n"))));
			 ((SR = "\n"; SR = "s\n"; SR = "shift\n"))).
  

% write reduce and shift clauses to output stream:
gen_reduce OS (shand LB (rule (LHS ==> RHS) Act) LA) :-   % make rule clause
					reverse RHS RRH, 
					output OS "parse [", 
					forevery (X\ (printterm OS X, output OS ",")) RRH,
					output OS "B|Alpha] [A|Beta] Result \"reduce\" :- \n",
					output OS "\tmemb B (bols :: ", printterm OS LB, 
					output OS "), memb Sym (eols :: ", printterm OS LA, 
					output OS "), first Sym A, !, \n\t(",       % ! cuts redundant memb calls
					printterm OS Act, output OS "), parse [",   % semantic action call
					printterm OS LHS, output OS ",B|Alpha] [A|Beta] Result Str.\n".
 
gen_shift OS LB Gamma Next :- output OS "parse [",   % Gamma already reversed  % make shift rule
			      forevery (X\ (printterm OS X, output OS ",")) Gamma,
			      output OS "B|Alpha] [A|Beta] Result \"shift\" :- \n",
			      output OS "\t memb B (bols :: ", printterm OS LB, 
			      output OS "), first (", 
			      printterm OS Next, output OS ") A, !, parse [A,", 
			      forevery (X\ (printterm OS X, output OS ",")) Gamma,
			      output OS "B|Alpha] Beta Result Str.\n".
