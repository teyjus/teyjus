module lambdayacc.   

% Bounded Right Context Parser Generator for Lambda Prolog (Teyjus)
% Includes semi-universal tokenizer


% computation of the first set  (ignoring nullability for now)

non_terminal sprime.      % internal start symbol of grammar.
terminal bofs.		  % internal begin of file symbol   
terminal eofs.	          % internal end of file symbol

getrule R :- cfg Gs, member R Gs.

first_stage 0 N T :- getrule (rule (N ==> [T|Ts]) A) , terminal T.
first_stage M N T :- M > 0, getrule (rule (N ==> [H|Hs]) A), 
   non_terminal H, not (H = N), Mp is (M - 1), first_stage Mp H T.

gen_first OS M :- 
   ntnum N, M is (N - 1), output OS "first X X :- terminal X.\n\n".
gen_first OS M :- M2 is (M + 1), 
   ( ( first_stage M N T, 
       printterm OS (first N T), output OS ".\n",
       fail ); 
     gen_first OS M2 ).
% usage: gen_first OUTPUTSTREAM 0.

% regular first predicate:
find_first 0 T T :- terminal T, !.
find_first M N T :- ntnum Max, M =< Max,
  (first_stage M N T; (M2 is (M + 1), find_first M2 N T)).


%  BRC(1,1) handle generation (no merging)
kind ch type.   % candidate handle type
type handle gs -> decl -> gs -> ch.
type gen_handles (list ch) -> (list ch) -> (list ch) -> o.
type context_search gs -> gs -> (list gs) -> (list gs) -> (list gs) -> o.
type make_handles (list gs) -> (list ch) -> o.
type rule_to_h gs -> gs -> decl -> ch -> o.

% usage: gen_handles nil [handle [bofs] (rule (sprime ==> [s]) true) [eofs]] H.
gen_handles H nil H.
gen_handles Seen [H|T] R :-
 not (member H Seen; member H T), !,
 H = (handle Lb (rule (LHS ==> RHS) Act) La),
 context_search Lb La nil RHS Contexts,
 make_handles Contexts NewHandles,
 append T NewHandles NewT,   % BFS
 gen_handles [H|Seen] NewT R.
gen_handles Seen [H|T] R :- gen_handles Seen T R.


context_search Lb La Back nil nil.		% currently used by sbrc
context_search Lb La Back [H|T] Cs :- 
  terminal H, context_search Lb La [H|Back] T Cs.
context_search Lb La Back [H|T] [LC,H,RC|Cs] :- non_terminal H, 
  look Lb Back LC, look La T RC,  % find contexts, with defaults.
  context_search Lb La [H|T] T Cs.

make_handles nil nil.				% currently used by sbrc
make_handles [Lb,N,La|Rest] Hs :-
  getprods N Nrules,
  map_pred2 (rule_to_h Lb La) Nrules Nhandles,
  make_handles Rest RestH,
  append Nhandles RestH Hs.

% contexts as lists:
%scontext_search Lb La Back nil nil.
%scontext_search Lb La Back [H|T] Cs :- 
%  terminal H, scontext_search Lb La [H|Back] T Cs.
%scontext_search Lb La Back [H|T] [LC,H,RC|Cs] :- non_terminal H, 
%  map1 (X\ (look X Back)) Lb LC, map1 (X\ (look X T)) La RC,
%  scontext_search Lb La [H|T] T Cs.
% make handles where Lb, La are lists
%smake_handles nil nil.
%smake_handles [Lb,N,La|Rest] Hs :-
%  getprods N Nrules,
%  map_pred2 (srule_to_h Lb La) Nrules Nhandles,
%  smake_handles Rest RestH,
%  append Nhandles RestH Hs.
%srule_to_h Lb La (rule (L ==> R) A) (shand Lb (rule (L ==> R) A) La).

rule_to_h Lb La (rule (L ==> R) A) (handle Lb (rule (L ==> R) A) La).

% get all the productions of a given non_terminal:
getprods N L :- cfg Grammar, 
  pi inr\ ( (pi Rhs\ (pi A\ (inr (rule (N2 ==> Rhs) A) :- freshcopy N N2))) =>
      filter inr Grammar L).

% full BRC(1,1) valid handles:
brcvh Hands :- cfg Grammar, start_symbol S,   
  gen_handles nil [handle bofs (rule (sprime ==> [S]) true) eofs] H,
  map_pred2 convert_to_sets H Hands.
  
convert_to_sets (handle LB Decl LA) (shand [LB] Decl [LA]).  % for uniformity


%-------------------------  Simple BRC    ---------

type shandle (list gs) -> decl -> (list gs) -> o.
type shand (list gs) -> decl -> (list gs) -> ch.
type sbrc (list ch) -> o.
type sprepare (list decl) -> (list ch) -> o.
type gen_shandles (list ch) -> (list ch) -> o.
type make_shandle decl -> ch -> o.

sbrc Shands :- cfg Grammar, sprepare Grammar Shands.

% sprepare sets up the valid handles with empty openlists.
sprepare nil Shands :- 
  start_symbol Ex,
  ((shandle LB (rule (sprime ==> [Ex]) true) LA) =>
    gen_shandles [handle bofs (rule (sprime ==> [Ex]) true) eofs] Shands).
sprepare [R|Rs] S :- shandle LB R LA => sprepare Rs S.  % create openlists.

gen_shandles [H|T] R :- not (openmemb H T), 
 H = (handle Lb (rule (LHS ==> RHS) Act) La),
 shandle LB (rule (LHS ==> RHS) Act) LA,      % current status.
 not (openmemb Lb LB, openmemb La LA), !,     % new context found.
 context_search Lb La nil RHS Contexts,
 make_handles Contexts NewHandles,	      % spawn new handles from H
% append T NewHandles NewT,   % BFS
 append NewHandles T NewT,   % DFS
 openadd Lb LB, openadd La LA,                % add context to openlists
 gen_shandles NewT R.
gen_shandles [H|T] R :- gen_shandles T R.     % not new handle, skip,

gen_shandles nil H :- 
  cfg Grammar, map_pred2 make_shandle Grammar H.

make_shandle Decl (shand LB Decl LA) :-       % close open lists
  shandle LB Decl LA, close_list LB, close_list LA.

%--------  SBRC with before and follow:   ---

% left context search
lcontext_search Lb Back nil nil.
lcontext_search Lb Back [H|T] Cs :- 
  terminal H, lcontext_search Lb [H|Back] T Cs.
lcontext_search Lb Back [H|T] [LC,H,epsilon|Cs] :- non_terminal H, 
  look Lb Back LC,   % find contexts, with defaults.
  lcontext_search Lb [H|T] T Cs.

% right context search
rcontext_search La Back nil nil.
rcontext_search La Back [H|T] Cs :- 
  terminal H, rcontext_search La [H|Back] T Cs.
rcontext_search La Back [H|T] [epsilon,H,RC|Cs] :- non_terminal H, 
  look La T RC,   % find contexts, with defaults.
  rcontext_search La [H|T] T Cs.

lgen_shandles [H|T] R :- not (openmemb H T),  
 H = (handle Lb (rule (LHS ==> RHS) Act) La), % La = epsilon here
 shandle LB (rule (LHS ==> RHS) Act) LA,      % current status.
 not (openmemb Lb LB), !,	     	      % new context found.
 lcontext_search Lb nil RHS Contexts,
 make_handles Contexts NewHandles,	      % spawn new handles from H
% append T NewHandles NewT,   % BFS
 append NewHandles T NewT,   % DFS
 openadd Lb LB, 		              % add context to openlists
 lgen_shandles NewT R.
lgen_shandles [H|T] R :- lgen_shandles T R.     % not new handle, skip,
lgen_shandles nil H.

rgen_shandles [H|T] R :- not (openmemb H T), 
 H = (handle Lb (rule (LHS ==> RHS) Act) La), % Lb = epsilon here
 shandle LB (rule (LHS ==> RHS) Act) LA,      % current status.
 not (openmemb La LA), !,	     	      % new context found.
 rcontext_search La nil RHS Contexts,
 make_handles Contexts NewHandles,	      % spawn new handles from H
% append T NewHandles NewT,   % BFS
 append NewHandles T NewT,   % DFS
 openadd La LA, 		              % add context to openlists
 rgen_shandles NewT R.
rgen_shandles [H|T] R :- rgen_shandles T R.     % not new handle, skip,
rgen_shandles nil H :- 
  cfg Grammar, map_pred2 make_shandle Grammar H.

fsbrc Shands :- cfg Grammar, fprepare Grammar Shands.

% sprepare sets up the valid handles with empty openlists.
fprepare nil Shands :- 
  start_symbol Ex,
  ((shandle LB (rule (sprime ==> [Ex]) true) LA) =>
    ((lgen_shandles [handle bofs (rule (sprime ==> [Ex]) true) epsilon] S1),
     (rgen_shandles [handle epsilon (rule (sprime ==> [Ex]) true) eofs] Shands))).
fprepare [R|Rs] S :- shandle LB R LA => fprepare Rs S.  % create openlists.




%---- R-R and S-R conflict detection:  (remember no epislons yet!)
type rrdeterministic (list ch) -> o.
type srdeterministic (list ch) -> (list ch) -> o.
type rr_conflict ch -> ch -> o.
type sr_conflict (list gs) -> (list gs) -> gs -> ch -> o.
type srprocess (list gs) -> (list gs) -> (list gs) -> (list ch) -> o.

rrdeterministic nil.
rrdeterministic [H|T] :-
  allare (X\ (not (rr_conflict H X))) T, rrdeterministic T.

rr_conflict (shand LB1 (rule (LHS ==> RHS) Act) LA1)
	    (shand LB2 (rule (LH2 ==> RH2) Ac2) LA2) :-
  reverse RHS RRH, reverse RH2 RR2,
  append RRH [B1|Alpha] VP1,        % first viable prefix
  append RR2 [B2|Alpha2] VP1,       % second viable prefix
  member B1 LB1, member B2 LB2,     % pick two lookbacks
  member A1 LA1, member A2 LA2,     % even after lookahead consideration?    
  find_first 0 A1 A, find_first 0 A2 A, 
  print "Reduce-Reduce conflict exists between\n",
  printterm std_out (LHS ==> RHS), print "  and  ",
  printterm std_out (LH2 ==> RH2), 
  print "\n when top of stack is of form  ", printterm std_out VP1,
  print "\n and lookahead symbol is ", printterm std_out A. 


% usage: srdeterministic out_stream H H, where H are the shandles
srdeterministic nil H.
srdeterministic [H|T] Handles :- 
   H = (shand LB (rule (LHS ==> RHS) Act) LA),
   srprocess LB nil RHS Handles, srdeterministic T Handles.

srprocess LB H nil Handles.
srprocess LB nil [H] Handles.   % no shift needed
srprocess LB nil [H1,H2|T] Handles :- srprocess LB [H1] [H2|T] Handles.
srprocess LB [G|Gamma] [Next|Rest] Handles :-
  allare (X\ (not (sr_conflict LB [G|Gamma] Next X))) Handles,
  stream_name OS,
  gen_shift OS LB [G|Gamma] Next,  % this will be after "special" rules
  srprocess LB [Next,G|Gamma] Rest Handles.


% sr_conflict lookback, Gamma is "semi-handle", Next is shift symbol
% sr_conflict will eventually fail because all must be resolved.
sr_conflict LB1 Gamma Next 
            (shand LB2 (rule (LH2 ==> RH2) Ac2) LA2) :-
  reverse RH2 RR2,
  member B1 LB1, member B2 LB2,      % pick two lookbacks
  freshcopy B1 Bf1, freshcopy B2 Bf2,  % avoid any binding
  append Gamma [Bf1|Alpha] VP1,      % (Gamma already reversed)
  append RR2 [Bf2|Alpha2] VP1,       % second viable prefix      
  member A2 LA2, 
  find_first 0 A2 A, find_first 0 Next Aa2,	    % lookahead resolution
  freshcopy A Aa2,				    % avoid any binding
  not (decresolve RH2 A),   % not because we want it to fail!
  (freshcopy A Ab, openmap2 freshcopy VP1 VP2),             % DCG problem
  map_pred2 freshcopy RH2 RH3,
  print "**Shift-Reduce conflict with  ", printterm std_out (LH2 ==> RH3),
  print "  exists \n when top of stack is of form  ", printterm std_out VP2,
  print "\n and lookahead symbol is  ", printterm std_out Ab, 
  stream_name OS, srdecision OS VP2 Ab,
  print "\n", fail.    % the failure will allow it to detect all sr conflicts.

decresolve RH2 A :-    % try to resovle by (binary) operator assoc and prec
%  RH2 = [Ea,OP1,Eb], 
  append U [Ea,OP1,Eb] RH2,	  % operator may be at end of production 
  binaryop OP1 Ea Eb Assoc1 Prec1, 
  (binaryop A Eb Ed Assoc2 Prec2; 
   (implicitop Eb Ed2 Assoc3 Prec3, find_first 0 Ed2 A);
   unaryop A Ed Prec4).
%  print "\n *Conflict resolved by operator precedence/associativity.\n".
  % if this is the case, know the default special rules will work.
 
decresolve [Ea,Eb] A :-    % try to resovle by implicit operator assoc and prec
  implicitop Ea Eb Assoc1 Prec1,
  (find_first 0 Eb A; binaryop A Eb Ed Assoc2 Prec2; unaryop A Ed Prec3).

decresolve RHS A :-    % try to resovle by unary operator assoc and prec
  reverse RHS RRS, RRS = [Ea,Op|TT],
  unaryop Op Ea Prec1,
  ((binaryop A Ea Ed Assoc2 Prec2); 
   (implicitop Ea Ed2 Assoc3 Prec3, find_first 0 Ed2 A)).


srdecision OS VP A :- 
  print "\nDo you want to (s)hift or (r)educe (default is shift): ",
  input_line std_in SR,
  output OS "parse (", printterm OS VP,
  output OS ") [", printterm OS A, 
  output OS "|Beta] Result \"special\" :- !, ",
  output OS "\n\tparse (", printterm OS VP,
  output OS ") [", printterm OS A, 
  output OS "|Beta] Result ",             % will it be shift or reduce?
  ( ( (SR = "r\n"; SR = "reduce\n"), !,   %try parse again with reduce
      output OS "\"reduce\".\n");
    ( (SR = "\n"; SR = "s\n"; SR = "shift\n"), output OS "\"shift\".\n")), !.	
  

% write clauses to output stream:
gen_reduce OS (shand LB (rule (LHS ==> RHS) Act) LA) :-   % make rule clause
  reverse RHS RRH, 
  output OS "parse [", 
  allare (X\ (printterm OS X, output OS ",")) RRH,
  output OS "B|Alpha] [A|Beta] Result \"reduce\" :- \n",
  output OS "\tmember B (", printterm OS LB, 
  output OS "), member Sym (", printterm OS LA, 
  output OS "), first Sym A, !, \n\t(",       % ! cuts redundant member calls
  printterm OS Act, output OS "), parse [",   % semantic action call
  printterm OS LHS, output OS ",B|Alpha] [A|Beta] Result Str.\n".
  
gen_shift OS LB Gamma Next :-   % Gamma already reversed  % make shift rule
  output OS "parse [", 
  allare (X\ (printterm OS X, output OS ",")) Gamma,
  output OS "B|Alpha] [A|Beta] Result \"shift\" :- \n",
  output OS "\t member B (", printterm OS LB, 
  output OS "), first (", 
  printterm OS Next, output OS ") A, !, parse [A,", 
  allare (X\ (printterm OS X, output OS ",")) Gamma,
  output OS "B|Alpha] Beta Result Str.\n".
 
% main predicate:
% usage: genparser "sbrc" grammar_module parser_module
% e.g., genparser "sbrc" "calcgramar" "calcparser" will generate
%  "calcparser.mod" and "calcparser.sig".  
% genparser "brc" option is for experimentation only for now.
genparser VHType Gfile File :- 
  Sigfile is (File ^ ".sig"),
  Modfile is (File ^ ".mod"), 
  open_out Sigfile SS,			% create signature file
  output SS "\nsig ", output SS File, output SS ".\n",
  output SS "accum_sig ", 
  output SS Gfile, output SS ", lambdayacc.\n",
  close_out SS,
		        % open output stream for mod file
  open_out Modfile OS,  
  output OS "module ", output OS File, 
  output OS ".\naccumulate ",
  output OS Gfile, output OS ".\n\n",   % don't reaccumulate lambdayacc!
  gen_first OS 0,	        % generate FIRST set as atomic clauses
  print "Computing Valid Handles...\n",
  !, fsbrc SH, !,
%  (( VHType = "sbrc", !, sbrc SH, !);
%   ( VHType = "brc",  brcvh SH)),  !, % find valid handles, simple or full
  rrdeterministic SH, 		% no reduce-reduce conflicts
  print "No reduce-reduce conflicts.\n",
  				% check and resolve sr-conflicts, gen rules:
  (stream_name OS => srdeterministic SH SH),
  print "No remaining shift-reduce conflicts; generating rules... \n",
  % initial shift:
  start_symbol Start,
  output OS "parse [bofs] [A|B] R \"shift\" :- \n", output OS "\tfirst (",
  printterm OS Start, output OS ") A, parse [A,bofs] B R Str.\n",
  % accept rule:
  output OS "parse [Sx,bofs] [eofs] Sx \"accept\" :- start_symbol Sx.\n",
  allare (X\ (gen_reduce OS X)) SH,    % generate reduce rules
  % print failure rule:
  output OS "\nparse V I R \"error\" :- !, finderrline V I, print \"Remaining Input = \", print_tokens 10 I, !, fail.\n",
  print "Parser written to file.\n",
  close_out OS.


%%%% these rules should take precedence over other parse clauses:
% example: binaryop plust (ex E) (ex E2) "left" 4.

% uncomment to trace:
%parse A B C D :- printterm std_out (parse A B C D), print " <-- trace\n", fail.

parse A [(linenum N)|Beta] R S :- !, parse A Beta R S.  % ignore newlines

parse [Ea,OP,Eb|Alpha] [OP|Beta] Result "special" :-
  binaryop OP Eb Ea Assoc Prec1, !,
  ( (Assoc = "left", parse [Ea,OP,Eb|Alpha] [OP|Beta] Result "reduce");
    (Assoc = "right", parse [Ea,OP,Eb|Alpha] [OP|Beta] Result "shift")).

parse [Ea,OP1,Eb|Alpha] [OP2|Beta] Result "special" :- not (OP1 = OP2),
  binaryop OP1 Eb Ea Assoc1 Prec1,
  (binaryop OP2 Ea Ed Assoc2 Prec2; unaryop OP2 Ed Prec2), !,
  ( (Prec1 =< Prec2, parse [Ea,OP1,Eb|Alpha] [OP2|Beta] Result "reduce");
    (Prec1 > Prec2, parse [Ea,OP1,Eb|Alpha] [OP2|Beta] Result "shift")).

parse [Ea,Eb|Alpha] [A|Beta] Result "special" :-
  implicitop Eb Ea Assoc Prec, implicitop Ea Ec Assoc Prec, first Ec A, !,
  ( (Assoc = "left", parse [Ea,Eb|Alpha] [A|Beta] Result "reduce");
    (Assoc = "right", parse [Ea,Eb|Alpha] [A|Beta] Result "shift")).

parse [Ea,OP1,Eb|Alpha] [A|Beta] Result "special" :-
  binaryop OP1 Eb Ea Assoc1 Prec1,
  implicitop Ea Ec Assoc2 Prec2, first Ec A, !,
  ( (Prec1 =< Prec2, parse [Ea,OP1,Eb|Alpha] [A|Beta] Result "reduce");
    (Prec1 > Prec2, parse [Ea,OP1,Eb|Alpha] [A|Beta] Result "shift")).

parse [Ea,Eb|Alpha] [OP1|Beta] Result "special" :-
  implicitop Eb Ea Assoc1 Prec1, 
  (binaryop OP1 Ea Ec Assoc2 Prec2; unaryop OP1 Ec Prec2), !,
  ( (Prec1 =< Prec2, parse [Ea,Eb|Alpha] [OP1|Beta] Result "reduce");
    (Prec1 > Prec2, parse [Ea,Eb|Alpha] [OP1|Beta] Result "shift")).

parse [Ea,OP1|Alpha] [S2|Beta] Result "special" :-
  unaryop OP1 Ea Prec1,
  ((binaryop S2 Ea Eb Assoc2 Prec2, !); 
   (implicitop Ea Eb Assoc2 Prec2, first Eb S2)), !,
  ( (Prec1 =< Prec2, parse [OP1,Ea|Alpha] [S2|Beta] Result "reduce");
    (Prec1 > Prec2, parse [Op1,Ea|Alpha] [S2|Beta] Result "shift")).


%-----------------------------

% utilities:

look Lab nil Lab.
look Lab [H|T] H.
head [] epsilon.
head [H|T] H.
last [] epsilon.
last [T] T.
last [A,B|C] D :- last [B|C] D.

append nil L L.
append [H|T] L [H|Z] :- append T L Z.

member X [X|T].
member X [H|T] :- member X T.
oncememb X [X|T] :- !.
oncememb X [H|T] :- oncememb X T.

nth 1 X [X|T].
nth N X [H|T] :- not(X = H), N2 is (N - 1), nth N2 X T.

length [] 0.
length [H|T] N :- length T N2, N is (N2 + 1).

reverse nil nil.
reverse [H|T] M :- reverse T L, append L [H] M.

filter P nil nil.
filter P [H|T] [H|R] :- (P H), !, filter P T R.
filter P [H|T] R :- filter P T R.

preplace Pred A B [A|T] [B2|T] :- Pred B B2, !.
preplace Pred A B [H|T] [H|M] :- preplace Pred A B T M.

map1 F nil nil.
map1 F [H|T] [(F H)|Z] :- map1 F T Z.

map_pred2 P nil nil.
map_pred2 P [H|T] [L|M] :- (P H L), map_pred2 P T M.

allare P nil.
allare P [H|T] :- (P H), allare P T.


% the open-list utility:

type openlist (list A) -> o.
type close_list (list A ) -> o.
type openadd A -> (list A) -> o.
type openmemb A -> (list A) -> o. 

% add element A to end of openlist L
%openadd A [A|L] :- !.
openadd A [A2|L] :- freshcopy A A2, !.
openadd A [B|L] :- openadd A L.

close_list nil :- !.
close_list [A|T] :- close_list T.

% double negation looses instantiation, detects free variable:
openmemb A L :- not (not (L = nil)), not (not (L = [H|T])), !, fail.
%openmemb A [A|L] :- !.
openmemb A [A2|L] :- not (not (A = A2)), !.  % don't instantiate free vars!
openmemb A [B|L] :- !, openmemb A L.
% openmemb can be used  in place of freshcopy

opennil L :- not (not (L = nil)), not (not (L = [H|T])), !.
openmap2 P A A :- opennil A, !.
openmap2 P [H|T] [L|M] :- P H L, openmap2 P T M.

%%%%%

%%%%%%%%%%%%%%
% Semi-Universal Tokenizer:

%  -------------    Tokenizer:  -----------------------

% added 11/99: report line number parse failed at:
finderrline S [] :- !, print "Error parsing last line\n",
	print "\nTop of Stack =  ", print_tokens 1 S.
finderrline S [(linenum N)|T] :- !, print "** Error parsing line ",
        NF is (N + 1),    % offset (hack)
	printterm std_out NF, print "\nTop of Stack =  ",
        print_tokens 1 S.
finderrline S [H|T] :- finderrline S T.

% assume printname associates abstract syntax, string, and grammar symbols:
% assumes iconst, sconst, and id as grammar symbols:
freshcopy (iconst A) (iconst B) :- !.
freshcopy (sconst A) (sconst B) :- !.
freshcopy (id A) (id B) :- !.
% other freshcopy clauses will be added by grammar files

% pseudo finite automaton:
% yyinitial is the initial state, yyalphanum tries to read an alphanumeric
% string and convert it to either a keyword or identifier, and yyspecial
% recognizes special characters.  yycomment skips comments.

% added 11/99: first arg records line number via special linenum token:

% eof doesn't appear to work  - using $ as special eof character.
%yyinitial LN In nil :- eof In, !.
%yyinitial LN In [eofs] :- lookahead In LA, eof_symbol LA, !.
yyinitial LN In TL :- input In 1 Nextchar, NLN is (LN + 1),
  ((eof_symbol Nextchar, !, fail);
  ( (white_space Nextchar, !, yyinitial LN In TL);
    (TL = [Tk|Rs], alphab Nextchar, 
		yyalphanum Nextchar In Tk, yyinitial LN In Rs);
    (TL = [Tk|Rs], decdigit Nextchar, 
		yynat Nextchar In Tk, yyinitial LN In Rs);
    (Nextchar = "%", !, TL = [(linenum LN)|Rs], 
		yycomment In, yyinitial NLN In Rs);
    (Nextchar = "\n", !, TL = [(linenum LN)|Rs], yyinitial NLN In Rs);
    (TL = [Tk|Rs], Nextchar = "\"", yystring "" In Tk, yyinitial LN In Rs);
    (not (alphanum Nextchar),
     yyspecial Nextchar In Tok, yyinitial LN In Rest, TL = [Tok|Rest]);
    (!, print "Tokenizer stopped after line ", printterm std_out LN,
     print "\n", fail))), !.
yyinitial LN In [eofs].   % possible bug!

%yyalphanum Str In Tok :- eof In, !, (printname XX Str Tok; Tok = (id Str)).
yyalphanum Str In Tok :- lookahead In LA, eof_symbol LA, !,
   (printname XX Str Tok; Tok = (id Str)).
%yyalphanum A I T :- print A, print " alphanum\n", fail.
yyalphanum Str In Tok :- lookahead In LA1,   
  LA is (substring LA1 0 1), % bug fix
  ( (alphanum LA, !,   % read until non alpha-num is reached
     input In 1 Nextchar, Str2 is (Str ^ Nextchar), yyalphanum Str2 In Tok);
    ( (printname XX Str Tok, !); (Tok = (id Str)) )).

%yyspecial Str In Tok :- eof In, !, printname XX Str Tok.
%yyspecial Str In Tok :- lookahead In LA, 
%	eof_symbol LA, !, printname XX Str Tok.

yyspecial Str In Tok :- lookahead In LA1,    % try two-symbol token first
 (( eof_symbol LA1, !, printname XZ Str Tok);
  ( LA is (substring LA1 0 1),    % bug fix
    Str2 is (Str ^ LA), 
    ( (printname XX Str2 Tok, !, input In 1 Nextchar);
      (printname XY Str Tok, !);
      (not (alphanum LA), not (white_space LA), not (LA = "\n"), !,
       input In 1 Nextc, Str3 is (Str ^ Nextc), yyspecial Str3 In Tok) 
    )
  )).

%yyspecial Str In Tok :- printname XX Str Tok.  
%yyspecial Str In Tok :- lookahead In LA1,
%  LA is (substring LA1 0 1),    % bug fix
%  ( (not (alphanum LA), not (white_space LA), not (LA = "\n"), !,
%     input In 1 Nextchar, Str2 is (Str ^ Nextchar), yyspecial Str2 In Tok);
%    ( (printname XX Str Tok, !) )).

yystring Str In Tok :- lookahead In LA, 
  eof_symbol LA, !,  print "file ends before string\n", fail.
yystring Str In Tok :- lookahead In LA1,
  LA is (substring LA1 0 1),    % bug fix
  ( (LA = "\\", !, input In 2 Ns, Str2 is (Str ^ Ns), yystring Str2 In Tok);
    (LA = "\"", !, input In 1 Skip, Tok = (sconst Str));
    (input In 1 Next, Str2 is (Str ^ Next), yystring Str2 In Tok) ).

% comments always terminate in new line.
yycomment In :- input In 1 Nextchar, 
 (((Nextchar = "\n"; Nextchar is (chr 10); Nextchar is (chr 12)), !);
 yycomment In).


yynat Str In Tok :- lookahead In LA, eof_symbol LA, !,
   (parseint Str Value, Tok = (iconst Value)).
yynat Str In Tok :- lookahead In LA1,   
  LA is (substring LA1 0 1), % bug fix
  ( ( decdigit LA, !,   % read until non decimal digit is reached
      input In 1 Nextchar, Str2 is (Str ^ Nextchar), yynat Str2 In Tok);
    ( parseint Str Value, Tok = (iconst Value) )).

white_space X :- X is (chr 9).          	 % tab
white_space X :- X is (chr 12).          	 % form feed
white_space X :- X is (chr 32).          	 % space 
%white_space "\n".  % superceded by linenum   % chr 10

alphab S :- C is (string_to_int S),
    ((C > 64, C < 91); (C > 96, C < 123)).

alphanum S :- C is (string_to_int S),    % 95 is underscore
    ((C > 64, C < 91); (C > 96, C < 123); (C > 47, C < 58); (C = 95)).

decdigit S :- C is (string_to_int S), C > 47, C < 58.  

% "32" to 32:  
% parseint_aux string position accumulator exponent result
% usage: parseint "326" N.
parseint Str Value :- Length is (size Str), Lastpos is (Length - 1),
 	parseint_aux Str Lastpos 0 1 Value. 
parseint_aux S Pos AX Exp AX :- Pos < 0, !.
parseint_aux S Pos AX Exp N :- Digit is (substring S Pos 1),
  M is ((string_to_int Digit) - 48),
  Digitval is (M * Exp),
  NewAx is (AX + Digitval),
  NewExp is (Exp * 10),
  NewPos is (Pos - 1),
  parseint_aux S NewPos NewAx NewExp N.
  
% pretty printing - used in error reportage:
% print_tokens N L prints up to N tokens of L
print_tokens 0 [H|T] :- !, print " ...\n".
print_tokens N [] :- !, print "\n".
print_tokens N [(id A)|T] :- N > 0, !, print A,
  print " ", N2 is (N - 1), print_tokens N2 T.
print_tokens N [(iconst S)|T] :- N > 0, !, printterm std_out S,
  print " ", N2 is (N - 1), print_tokens N2 T.
print_tokens N [(sconst S)|T] :- N > 0, !, print "\"", print S,
  print "\" ", N2 is (N - 1), print_tokens N2 T.
print_tokens N [(linenum K)|T] :- N > 0, !, 
  print "\n", N2 is (N - 1), print_tokens N2 T.
print_tokens N [H|T] :- N > 0, 
  ( (printname XX SH H, !, print SH);
    (printterm std_out H)  ),   % defaults to itself
  print " ", N2 is (N - 1), print_tokens N2 T.
  


% ----------------------------- Main Predicates:  ---------------
parseline Absyn :-
  print "\nEnter Expression: ",
  input_line std_in Line,  % get string
  open_string Line In,     % create in_stream from string
  ((pi It\ (pi LA\ (pi (LN\ (yyinitial LN It [eofs] :- 
			lookahead It LA, eof_symbol LA, !)))))
     => ( eof_symbol "\n" => yyinitial 0 In Tokens )),
  close_in In, !,
  parse [bofs] Tokens Absyn Str, !, 5 = 5.   % bug fix

parsefile File Absyn :- 
  open_in File In,
  print "Scanning for tokens...\n",
  ((eof_symbol "$") => yyinitial 0 In Tokens),
  close_in In, !, 
% printterm std_out Tokens, print "<- tokens\n",
  print "Parsing...\n",
  parse [bofs] Tokens Absyn Str, !, 5 = 5.	     % bug fix

