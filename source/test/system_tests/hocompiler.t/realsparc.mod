module realsparc.
accumulate absparc.

% compiles filein to fileout:
tigcompile FI FO  :-
  parsefile FI (program A),
%sysprops,
%  print "\n forming CPS intermediate representation ...\n",
  formcps A kret AK, !, 
%sysprops,
%  print "\n generating code ...\n",
  gencode 0 M AK AS AI,
%sysprops,
  optimizes1 AI I0,
  extractstr M I0 I6 Strs, !,
%sysprops,
  open_out FO OS,
  %weirdthing I6 I,
  output OS "\n\t.global main \n\t.align 4",
  genout Strs OS,  % produce .asciz lines
  output OS "\n\n\t.align 4",
  output OS "\nmain:\n",
  output OS "\n\tsave %sp,-200,%sp",
  genout I6 OS,
  output OS "\n\n\tmov 1, %g1\n\tta 0\n",
  print "\nAssembly code written to file.\n",
%sysprops,
  close_out OS.

%local weirdthing (list instruction) -> (list instruction) -> o.
%weirdthing [] [].
%weirdthing [(movop (reg I N) B)|C] [(movop (reg I M) B)|D] :- !,
%  M is (N + 0), weirdthing C D.
%weirdthing [ilabel S|A] [ilabel T|B] :- !, T is (S ^ ""),
%  weirdthing A B.
%weirdthing [branchop "call" (slabel S)|A] 
%           [branchop "call" (slabel T)|B] :- !, T is (S ^ ""),
%  weirdthing A B.
%weirdthing [A|B] [A|C] :- weirdthing B C.
 
 
% uncomment to trace:
% genout A B :- printterm std_out A, print " <--\n", fail.

genout [] OS.
genout [nullop|I] OS :-
  output OS "\n\tnop", genout I OS.
genout [ilabel S|I] OS :-
  S2 is (S ^ ":"),   % required by sparc
  output OS "\n", output OS S2, genout I OS.
genout [movop (conststore N) (reg A M)|I] OS :- !,
  transtore (conststore N) AS, transtore (reg A M) BS,
  output OS "\n\tmov ", output OS AS,
  output OS ",", output OS BS,
  genout I OS.
genout [movop (reg A N) (reg B M)|I] OS :- !,  % register - register
  transtore (reg A N) AS, transtore (reg B M) BS,
  output OS "\n\tmov ", output OS AS,
  output OS ",", output OS BS,
  genout I OS.
genout [movop A (reg B N)|I] OS :- !,  		% load
  transtore A AS, transtore (reg B N) BS,
  output OS "\n\tld ", output OS AS,
  output OS ",", output OS BS,
  genout I OS.
genout [movop (reg B N) A|I] OS :- !,  		% store
  transtore A AS, transtore (reg B N) BS, 
  output OS "\n\tst ", output OS BS,
  output OS ",", output OS AS,
  genout I OS.
genout [branchop "retstore" N|I] OS :- !,
  output OS "\n\tret\n\trestore", genout I OS.
genout [branchop BOP L|I] OS :-
  transop BOP BS, transtore L LS, output OS "\n\t",
  output OS BS, output OS " ", output OS LS,
  genout I OS.
genout [basicop "cmp" A B C|I] OS :- !, 
  transtore A AS, transtore B BS, 
  output OS "\n\tcmp ", output OS AS, output OS ",",
  output OS BS, genout I OS.
genout [basicop "set" (slabel A) nullstore B|I] OS :- !, 
  transtore (slabel A) AS, transtore B BS, 
  output OS "\n\tset ", output OS AS, output OS ",",
  output OS BS, genout I OS.
genout [basicop OP A B C|I] OS :-
  transtore A AS, transtore B BS, transtore C CS,
  transop OP PS, 
  output OS "\n\t", output OS PS, output OS " ",
  output OS AS, output OS ",", output OS BS, 
  output OS ",", output OS CS, genout I OS.
genout [asciz (slabel SL) S|I] OS :-
  output OS "\n", output OS SL, output OS ":",
  output OS "\n\t.asciz ", printterm OS S,
  genout I OS.


% convert abstract store into string
local transtore store -> string -> o.
local transop string -> string -> o.

%transtore A B :- printterm std_out A, print " <- transtore\n", fail.

transtore nullstore "".  
transtore (slabel S) S.
transtore (reg R N) S :-
  S1 is ("%" ^ R), SN is (int_to_string N),
  S is (S1 ^ SN).

transtore (sreg R) S :- S is ("%" ^ R).
transtore (conststore N) SN :- SN is (int_to_string N).
transtore (indirect A (conststore B)) SI :-  B > 0,
  transtore A SA, SB is (int_to_string B),
  S1 is ("[" ^ SA), S2 is (S1 ^ "+"),
  S3 is (S2 ^ SB), SI is (S3 ^ "]").
transtore (indirect A (conststore B)) SI :-  B < 0,
  transtore A SA, SB is (int_to_string B),
  S1 is ("[" ^ SA), 
  S3 is (S1 ^ SB), SI is (S3 ^ "]").
transtore (indirect A (conststore 0)) SI :-
  transtore A SA,
  S1 is ("[" ^ SA), SI is (S1 ^ "]").
transtore (indirect A B) SI :- not (B = (conststore BB)),
  transtore A SA, transtore B SB, 
  S1 is ("[" ^ SA), S2 is (S1 ^ "+"), S3 is (S2 ^ SB),
  SI is (S3 ^ "]").

transop "+" "add" :- !.
transop "-" "sub" :- !.
transop A A.


%---- 
%for testing only:
comptest 1 :- parsefile "merge.tig" A.
comptest 2 :- parsefile "test3d.bob" (program A), formcps A kret B.
comptest 3 :- tigcompile "test3.bob" "test3.s".
comptest 4 :- tigcompile "test4.bob" "test4.s".
comptest 5 :- tigcompile "test5.bob" "test5.s".
comptest 6 :- tigcompile "test6.bob" "test6.s".
comptest 7 :- tigcompile "test7.bob" "test7.s".
comptest 8 :- tigcompile "test8.bob" "test8.s".


