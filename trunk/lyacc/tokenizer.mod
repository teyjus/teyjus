module tokenizer.

type yyinitial int -> in_stream -> (list gs) -> o.
type yyalphanum, yyspecial    string -> in_stream -> gs -> o.
type yycomment in_stream -> o.
type yylcomment in_stream -> o.
type yystring string -> in_stream -> gs -> o.
type yynat string -> in_stream -> gs -> o.
type decdigit, alphab, alphanum, white_space  string -> o.
type parseint string -> int -> o.
type parseint_aux string -> int -> int -> int -> int -> o.
type tokchar string -> o.


%%%%%%%%%%%%%%
% Semi-Universal Tokenizer:

%  -------------    Tokenizer:  -----------------------
% added 11/99: first arg records line number via special linenum token:

% Start tokenizing the file
yyinitial LN In TL :- not (eof In), input In 1 Nextchar, NLN is (LN + 1),
		      % Continue until the end
		      ((eof_symbol Nextchar, !, fail);
		      % Ignore whitespace
		      ((white_space Nextchar, !, yyinitial LN In TL);
		      % Skip over multi-line comments surrounded by /* */
		      (Nextchar = "/", lookahead In Nextchar1, Nextchar1 = "*", input In 1 Nextchar2, !, TL = [(linenum LN)|Rs], 
		      yylcomment In, yyinitial NLN In Rs);
		      % Process alpha-numeric characters
		      (TL = [Tk|Rs], alphab Nextchar, 
		      yyalphanum Nextchar In Tk, yyinitial LN In Rs);
		      % Process integers
		      (TL = [Tk|Rs], decdigit Nextchar, 
		      yynat Nextchar In Tk, yyinitial LN In Rs);
		      % Process single-line comments
		      ((comment Nextchar;(not (eof In),lookahead In LA,Str2 is (Nextchar ^ LA),comment Str2)), !, TL = [(linenum LN)|Rs], 
		      yycomment In, yyinitial NLN In Rs);
		      (Nextchar = "\n", !, TL = [(linenum LN)|Rs], yyinitial NLN In Rs);
		      % Process strings in quotation marks
		      (TL = [Tk|Rs], Nextchar = "\"", yystring "" In Tk, yyinitial LN In Rs);
		      % Process non-alphanumeric characters as special symbols
		      (not (alphanum Nextchar), !,
		      yyspecial Nextchar In Tok, yyinitial LN In Rest, TL = [Tok|Rest]);
		      % An error in parsing
		      (!, print "Tokenizer stopped after line ", printterm std_out LN,
		      print "\n", fail))), !.
yyinitial LN In [eofs].   % possible bug!

% Process alphanumeric characters and return as token if exists, or id otherwise
yyalphanum Str In Tok :- eof In, !, (printname XX Str Tok; Tok = (id Str)).
yyalphanum Str In Tok :- lookahead In LA1, 
			 LA is (substring LA1 0 1), % bug fix
			 ((alphanum LA, !,   % read until non alpha-num is reached
			 input In 1 Nextchar, Str2 is (Str ^ Nextchar), yyalphanum Str2 In Tok);
			 ((printname XX Str Tok, !); (Tok = (id Str)) )). %, printterm std_out Tok, print "\n".

% Process special symbols and look for token represented by characters
yyspecial Str In Tok :- eof In, !, printname XX Str Tok.
yyspecial Str In Tok :- lookahead In LA1,    % try two-symbol token first
			(( eof_symbol LA1, !, printname XZ Str Tok);
			( LA is (substring LA1 0 1),    % bug fix
			Str2 is (Str ^ LA), 
			((printname XX Str2 Tok, !, input In 1 Nextchar);
			(printname XY Str Tok, !);
			(not (white_space LA), not (LA = "\n"), !,
			input In 1 Nextc, Str3 is (Str ^ Nextc), yyspecial Str3 In Tok); Tok = (id Str)))).

%  Process string (between quotation marks)
yystring Str In Tok :- lookahead In LA, 
		       eof_symbol LA, !,  print "file ends before string\n", fail.
yystring Str In Tok :- lookahead In LA1,
		       LA is (substring LA1 0 1),    % bug fix
		       ((LA = "\\", !, input In 2 Ns, Str2 is (Str ^ Ns), yystring Str2 In Tok);
		       (LA = "\"", !, input In 1 Skip, Tok = (sconst Str));
		       (input In 1 Next, Str2 is (Str ^ Next), yystring Str2 In Tok) ).

% comments always terminate in new line.
yycomment In :- eof In, !.
yycomment In :- input In 1 Nextchar, 
	        (((Nextchar = "\n"; Nextchar is (chr 10); Nextchar is (chr 12)), !);
		yycomment In),!.

% Long comments -- read until the */ is reached
yylcomment In :- input In 1 Nextchar, not(Nextchar = "*"), yylcomment In, !.
yylcomment In :- (lookahead In Nextchar, Nextchar = "/", input In 1 Nextchar1, !); yylcomment In.

% Read decimal digits
yynat Str In Tok :- eof In, !, (parseint Str Value, Tok = (iconst Value)).
yynat Str In Tok :- lookahead In LA, eof_symbol LA, !,
		    (parseint Str Value, Tok = (iconst Value)).
yynat Str In Tok :- lookahead In LA1,   
		    LA is (substring LA1 0 1), % bug fix
		    ( ( decdigit LA, !,   % read until non decimal digit is reached
		    input In 1 Nextchar, Str2 is (Str ^ Nextchar), yynat Str2 In Tok);
		    ( parseint Str Value, Tok = (iconst Value) )).

% White space (assuming one's background color is white)
white_space X :- X is (chr 9).          	 % tab
white_space X :- X is (chr 12).          	 % form feed
white_space X :- X is (chr 32).          	 % space 

% The beginning of an alphanumeric pattern - all characters user has specified except numbers and comments
alphab S :- ((tokchar S,!);(letnum S)), C is (string_to_int S), not (C > 47, C < 58), not (comment S).

% Alpha-numeric characters are specified by user, or default letters and numbers are used
alphanum S :- ((tokchar S,!);(letnum S)).

% Decimal digits
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