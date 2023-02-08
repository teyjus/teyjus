module bobparser.
accumulate bobcatgram.

first (vdec_gs _556) vart.
first (vdec_gs _614) vart.
first (tydechd _739) typet.
first (cdec_gs _782) classt.
first (fundech _1115) functiont.
first (ty_gs _1158) (id _1167).
first (ty_gs _1200) lbrace.
first (ty_gs _1248) lbrace.
first ty_gshd arrayt.
first (tfields _1356) (id _1364).
first (tfields _1431) (id _1439).
first (lvalue _1516) (id _1525).
first (te _1669) nilt.
first (te _1743) (iconst _1752).
first (te _1785) (sconst _1794).
first (te _1968) minust.
first (te _2796) whilet.
first (te _2853) fort.
first (te _2940) breakt.
first (te _2972) lett.
first (te _3030) ift.
first (te _3099) ift.
first (te _3157) lparen.
first (createrec _3259) (id _3267).
first (moreinits _3299) (id _3307).
first (moreinits _3369) (id _3377).
first acomma comma.
first tyfcomma comma.
first (program _287) nilt.
first (program _287) (iconst _5359).
first (program _287) (sconst _5401).
first (program _287) minust.
first (program _287) whilet.
first (program _287) fort.
first (program _287) breakt.
first (program _287) lett.
first (program _287) ift.
first (program _287) ift.
first (program _287) lparen.
first (dec _361) vart.
first (dec _361) vart.
first (dec _435) classt.
first (tydec_gs _697) typet.
first (fdec_gs _866) functiont.
first (fdec_gs _926) functiont.
first (fdec_gs _1002) functiont.
first (fdec_gs _1055) functiont.
first (ty_gs _1298) arrayt.
first (te _1710) (id _5132).
first (funcallh _1937) (id _5132).
first (te _2689) (id _5132).
first (te _2743) (id _5132).
first (te _3214) (id _6874).
first (args _3496) nilt.
first (args _3496) (iconst _5359).
first (args _3496) (sconst _5401).
first (args _3496) minust.
first (args _3496) whilet.
first (args _3496) fort.
first (args _3496) breakt.
first (args _3496) lett.
first (args _3496) ift.
first (args _3496) ift.
first (args _3496) lparen.
first (expseq _3592) nilt.
first (expseq _3592) (iconst _5359).
first (expseq _3592) (sconst _5401).
first (expseq _3592) minust.
first (expseq _3592) whilet.
first (expseq _3592) fort.
first (expseq _3592) breakt.
first (expseq _3592) lett.
first (expseq _3592) ift.
first (expseq _3592) ift.
first (expseq _3592) lparen.
first (expseq _3636) nilt.
first (expseq _3636) (iconst _5359).
first (expseq _3636) (sconst _5401).
first (expseq _3636) minust.
first (expseq _3636) whilet.
first (expseq _3636) fort.
first (expseq _3636) breakt.
first (expseq _3636) lett.
first (expseq _3636) ift.
first (expseq _3636) ift.
first (expseq _3636) lparen.
first (program _296) (id _8739).
first (program _296) (id _8739).
first (program _296) (id _8739).
first (program _296) (id _10481).
first (dec _333) typet.
first (dec _407) functiont.
first (dec _407) functiont.
first (dec _407) functiont.
first (dec _407) functiont.
first (decs _481) vart.
first (decs _481) vart.
first (decs _481) classt.
first (decs _524) vart.
first (decs _524) vart.
first (decs _524) classt.
first (te _1845) (id _8739).
first (te _1899) (id _8739).
first (args _3505) (id _8739).
first (args _3505) (id _8739).
first (args _3505) (id _8739).
first (args _3505) (id _10481).
first (expseq _3601) (id _8739).
first (expseq _3601) (id _8739).
first (expseq _3601) (id _8739).
first (expseq _3601) (id _10481).
first (expseq _3645) (id _8739).
first (expseq _3645) (id _8739).
first (expseq _3645) (id _8739).
first (expseq _3645) (id _10481).
first (program _305) (id _12346).
first (program _305) (id _12346).
first (decs _490) typet.
first (decs _490) functiont.
first (decs _490) functiont.
first (decs _490) functiont.
first (decs _490) functiont.
first (decs _533) typet.
first (decs _533) functiont.
first (decs _533) functiont.
first (decs _533) functiont.
first (decs _533) functiont.
first (args _3514) (id _12346).
first (args _3514) (id _12346).
first (expseq _3610) (id _12346).
first (expseq _3610) (id _12346).
first (expseq _3654) (id _12346).
first (expseq _3654) (id _12346).
first X Y :- terminal X, X = Y.

parse [dec _587,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (publict :: dec _3002939 :: lett :: nil), first (decs _596) A, !, parse [A,dec _587,B|Alpha] Beta Result Str.
parse [vart,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2922479 :: nil), first (id _640) A, !, parse [A,vart,B|Alpha] Beta Result Str.
parse [id _640,vart,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2922479 :: nil), first (assignt) A, !, parse [A,id _640,vart,B|Alpha] Beta Result Str.
parse [assignt,id _640,vart,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2922479 :: nil), first (te _652) A, !, parse [A,assignt,id _640,vart,B|Alpha] Beta Result Str.
parse [vart,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2929460 :: nil), first (id _698) A, !, parse [A,vart,B|Alpha] Beta Result Str.
parse [id _698,vart,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2929460 :: nil), first (colon) A, !, parse [A,id _698,vart,B|Alpha] Beta Result Str.
parse [colon,id _698,vart,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2929460 :: nil), first (id _709) A, !, parse [A,colon,id _698,vart,B|Alpha] Beta Result Str.
parse [id _709,colon,id _698,vart,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2929460 :: nil), first (assignt) A, !, parse [A,id _709,colon,id _698,vart,B|Alpha] Beta Result Str.
parse [assignt,id _709,colon,id _698,vart,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2929460 :: nil), first (te _721) A, !, parse [A,assignt,id _709,colon,id _698,vart,B|Alpha] Beta Result Str.
parse [tydechd _769,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2911449 :: nil), first (ty_gs _778) A, !, parse [A,tydechd _769,B|Alpha] Beta Result Str.
parse [typet,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2912009 :: nil), first (id _823) A, !, parse [A,typet,B|Alpha] Beta Result Str.
parse [id _823,typet,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2912009 :: nil), first (eqt) A, !, parse [A,id _823,typet,B|Alpha] Beta Result Str.
parse [classt,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2995178 :: nil), first (id _866) A, !, parse [A,classt,B|Alpha] Beta Result Str.
parse [id _866,classt,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2995178 :: nil), first (ldbrace) A, !, parse [A,id _866,classt,B|Alpha] Beta Result Str.
parse [ldbrace,id _866,classt,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2995178 :: nil), first (privatet) A, !, parse [A,ldbrace,id _866,classt,B|Alpha] Beta Result Str.
parse [privatet,ldbrace,id _866,classt,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2995178 :: nil), first (tfields _880) A, !, parse [A,privatet,ldbrace,id _866,classt,B|Alpha] Beta Result Str.
parse [tfields _880,privatet,ldbrace,id _866,classt,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2995178 :: nil), first (publict) A, !, parse [A,tfields _880,privatet,ldbrace,id _866,classt,B|Alpha] Beta Result Str.
parse [publict,tfields _880,privatet,ldbrace,id _866,classt,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2995178 :: nil), first (decs _891) A, !, parse [A,publict,tfields _880,privatet,ldbrace,id _866,classt,B|Alpha] Beta Result Str.
parse [decs _891,publict,tfields _880,privatet,ldbrace,id _866,classt,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2995178 :: nil), first (rdbrace) A, !, parse [A,decs _891,publict,tfields _880,privatet,ldbrace,id _866,classt,B|Alpha] Beta Result Str.
parse [fundech _938,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2947380 :: nil), first (tfields _946) A, !, parse [A,fundech _938,B|Alpha] Beta Result Str.
parse [tfields _946,fundech _938,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2947380 :: nil), first (rparen) A, !, parse [A,tfields _946,fundech _938,B|Alpha] Beta Result Str.
parse [rparen,tfields _946,fundech _938,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2947380 :: nil), first (eqt) A, !, parse [A,rparen,tfields _946,fundech _938,B|Alpha] Beta Result Str.
parse [eqt,rparen,tfields _946,fundech _938,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2947380 :: nil), first (te _961) A, !, parse [A,eqt,rparen,tfields _946,fundech _938,B|Alpha] Beta Result Str.
parse [fundech _998,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2962197 :: nil), first (tfields _1006) A, !, parse [A,fundech _998,B|Alpha] Beta Result Str.
parse [tfields _1006,fundech _998,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2962197 :: nil), first (rparen) A, !, parse [A,tfields _1006,fundech _998,B|Alpha] Beta Result Str.
parse [rparen,tfields _1006,fundech _998,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2962197 :: nil), first (colon) A, !, parse [A,rparen,tfields _1006,fundech _998,B|Alpha] Beta Result Str.
parse [colon,rparen,tfields _1006,fundech _998,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2962197 :: nil), first (id _1020) A, !, parse [A,colon,rparen,tfields _1006,fundech _998,B|Alpha] Beta Result Str.
parse [id _1020,colon,rparen,tfields _1006,fundech _998,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2962197 :: nil), first (eqt) A, !, parse [A,id _1020,colon,rparen,tfields _1006,fundech _998,B|Alpha] Beta Result Str.
parse [eqt,id _1020,colon,rparen,tfields _1006,fundech _998,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2962197 :: nil), first (te _1032) A, !, parse [A,eqt,id _1020,colon,rparen,tfields _1006,fundech _998,B|Alpha] Beta Result Str.
parse [fundech _1074,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2972356 :: nil), first (rparen) A, !, parse [A,fundech _1074,B|Alpha] Beta Result Str.
parse [rparen,fundech _1074,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2972356 :: nil), first (eqt) A, !, parse [A,rparen,fundech _1074,B|Alpha] Beta Result Str.
parse [eqt,rparen,fundech _1074,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2972356 :: nil), first (te _1089) A, !, parse [A,eqt,rparen,fundech _1074,B|Alpha] Beta Result Str.
parse [fundech _1127,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2982849 :: nil), first (rparen) A, !, parse [A,fundech _1127,B|Alpha] Beta Result Str.
parse [rparen,fundech _1127,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2982849 :: nil), first (colon) A, !, parse [A,rparen,fundech _1127,B|Alpha] Beta Result Str.
parse [colon,rparen,fundech _1127,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2982849 :: nil), first (id _1141) A, !, parse [A,colon,rparen,fundech _1127,B|Alpha] Beta Result Str.
parse [id _1141,colon,rparen,fundech _1127,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2982849 :: nil), first (eqt) A, !, parse [A,id _1141,colon,rparen,fundech _1127,B|Alpha] Beta Result Str.
parse [eqt,id _1141,colon,rparen,fundech _1127,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2982849 :: nil), first (te _1153) A, !, parse [A,eqt,id _1141,colon,rparen,fundech _1127,B|Alpha] Beta Result Str.
parse [functiont,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2947940 :: nil), first (id _1199) A, !, parse [A,functiont,B|Alpha] Beta Result Str.
parse [id _1199,functiont,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lett :: publict :: dec _2947940 :: nil), first (lparen) A, !, parse [A,id _1199,functiont,B|Alpha] Beta Result Str.
parse [lbrace,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (tydechd _2666952 :: nil), first (tfields _1284) A, !, parse [A,lbrace,B|Alpha] Beta Result Str.
parse [tfields _1284,lbrace,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (tydechd _2666952 :: nil), first (rbrace) A, !, parse [A,tfields _1284,lbrace,B|Alpha] Beta Result Str.
parse [lbrace,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (tydechd _2684680 :: nil), first (rbrace) A, !, parse [A,lbrace,B|Alpha] Beta Result Str.
parse [ty_gshd,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (tydechd _2688585 :: nil), first (id _1374) A, !, parse [A,ty_gshd,B|Alpha] Beta Result Str.
parse [arrayt,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (tydechd _2688979 :: nil), first (oft) A, !, parse [A,arrayt,B|Alpha] Beta Result Str.
parse [id _1437,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lbrace :: tyfcomma :: fundech _2725445 :: privatet :: nil), first (colon) A, !, parse [A,id _1437,B|Alpha] Beta Result Str.
parse [colon,id _1437,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lbrace :: tyfcomma :: fundech _2725445 :: privatet :: nil), first (id _1449) A, !, parse [A,colon,id _1437,B|Alpha] Beta Result Str.
parse [id _1512,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lbrace :: tyfcomma :: fundech _2733448 :: privatet :: nil), first (colon) A, !, parse [A,id _1512,B|Alpha] Beta Result Str.
parse [colon,id _1512,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lbrace :: tyfcomma :: fundech _2733448 :: privatet :: nil), first (id _1523) A, !, parse [A,colon,id _1512,B|Alpha] Beta Result Str.
parse [id _1523,colon,id _1512,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lbrace :: tyfcomma :: fundech _2733448 :: privatet :: nil), first (tyfcomma) A, !, parse [A,id _1523,colon,id _1512,B|Alpha] Beta Result Str.
parse [tyfcomma,id _1523,colon,id _1512,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lbrace :: tyfcomma :: fundech _2733448 :: privatet :: nil), first (tfields _1535) A, !, parse [A,tyfcomma,id _1523,colon,id _1512,B|Alpha] Beta Result Str.
parse [lvalue _1639,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (bofs :: lbrack :: funcallh _78398 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: nil), first (dott) A, !, parse [A,lvalue _1639,B|Alpha] Beta Result Str.
parse [dott,lvalue _1639,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (bofs :: lbrack :: funcallh _78398 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: nil), first (id _1651) A, !, parse [A,dott,lvalue _1639,B|Alpha] Beta Result Str.
parse [lvalue _1693,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (bofs :: lbrack :: funcallh _88729 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: nil), first (lbrack) A, !, parse [A,lvalue _1693,B|Alpha] Beta Result Str.
parse [lbrack,lvalue _1693,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (bofs :: lbrack :: funcallh _88729 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: nil), first (te _1704) A, !, parse [A,lbrack,lvalue _1693,B|Alpha] Beta Result Str.
parse [te _1704,lbrack,lvalue _1693,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (bofs :: lbrack :: funcallh _88729 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: nil), first (rbrack) A, !, parse [A,te _1704,lbrack,lvalue _1693,B|Alpha] Beta Result Str.
parse [funcallh _1908,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lbrack :: funcallh _96849 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: bofs :: nil), first (args _1916) A, !, parse [A,funcallh _1908,B|Alpha] Beta Result Str.
parse [args _1916,funcallh _1908,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lbrack :: funcallh _96849 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: bofs :: nil), first (rparen) A, !, parse [A,args _1916,funcallh _1908,B|Alpha] Beta Result Str.
parse [funcallh _1962,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (funcallh _104832 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: lbrack :: bofs :: nil), first (rparen) A, !, parse [A,funcallh _1962,B|Alpha] Beta Result Str.
parse [lvalue _2009,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lbrack :: funcallh _100926 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: bofs :: nil), first (lparen) A, !, parse [A,lvalue _2009,B|Alpha] Beta Result Str.
parse [minust,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (funcallh _111146 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: lbrack :: bofs :: nil), first (te _2053) A, !, parse [A,minust,B|Alpha] Beta Result Str.
parse [te _2101,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: funcallh _7485921 :: comma :: lbrack :: bofs :: nil), first (timest) A, !, parse [A,te _2101,B|Alpha] Beta Result Str.
parse [timest,te _2101,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: funcallh _7485921 :: comma :: lbrack :: bofs :: nil), first (te _2113) A, !, parse [A,timest,te _2101,B|Alpha] Beta Result Str.
parse [te _2156,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: minust :: funcallh _7498321 :: comma :: lbrack :: bofs :: nil), first (dividet) A, !, parse [A,te _2156,B|Alpha] Beta Result Str.
parse [dividet,te _2156,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: minust :: funcallh _7498321 :: comma :: lbrack :: bofs :: nil), first (te _2168) A, !, parse [A,dividet,te _2156,B|Alpha] Beta Result Str.
parse [te _2211,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: timest :: minust :: funcallh _7510721 :: comma :: lbrack :: bofs :: nil), first (minust) A, !, parse [A,te _2211,B|Alpha] Beta Result Str.
parse [minust,te _2211,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: timest :: minust :: funcallh _7510721 :: comma :: lbrack :: bofs :: nil), first (te _2223) A, !, parse [A,minust,te _2211,B|Alpha] Beta Result Str.
parse [te _2266,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: timest :: minust :: funcallh _7523121 :: comma :: lbrack :: bofs :: nil), first (plust) A, !, parse [A,te _2266,B|Alpha] Beta Result Str.
parse [plust,te _2266,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: timest :: minust :: funcallh _7523121 :: comma :: lbrack :: bofs :: nil), first (te _2278) A, !, parse [A,plust,te _2266,B|Alpha] Beta Result Str.
parse [te _2321,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: dividet :: timest :: minust :: funcallh _7535521 :: comma :: lbrack :: bofs :: nil), first (ort) A, !, parse [A,te _2321,B|Alpha] Beta Result Str.
parse [ort,te _2321,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: dividet :: timest :: minust :: funcallh _7535521 :: comma :: lbrack :: bofs :: nil), first (te _2333) A, !, parse [A,ort,te _2321,B|Alpha] Beta Result Str.
parse [te _2376,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: plust :: dividet :: timest :: minust :: funcallh _7547921 :: comma :: lbrack :: bofs :: nil), first (andt) A, !, parse [A,te _2376,B|Alpha] Beta Result Str.
parse [andt,te _2376,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: plust :: dividet :: timest :: minust :: funcallh _7547921 :: comma :: lbrack :: bofs :: nil), first (te _2388) A, !, parse [A,andt,te _2376,B|Alpha] Beta Result Str.
parse [te _2431,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: ort :: plust :: dividet :: timest :: minust :: funcallh _7560321 :: comma :: lbrack :: bofs :: nil), first (eqt) A, !, parse [A,te _2431,B|Alpha] Beta Result Str.
parse [eqt,te _2431,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: ort :: plust :: dividet :: timest :: minust :: funcallh _7560321 :: comma :: lbrack :: bofs :: nil), first (te _2443) A, !, parse [A,eqt,te _2431,B|Alpha] Beta Result Str.
parse [te _2486,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7572721 :: comma :: lbrack :: bofs :: nil), first (ltt) A, !, parse [A,te _2486,B|Alpha] Beta Result Str.
parse [ltt,te _2486,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7572721 :: comma :: lbrack :: bofs :: nil), first (te _2498) A, !, parse [A,ltt,te _2486,B|Alpha] Beta Result Str.
parse [te _2541,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7585121 :: comma :: lbrack :: bofs :: nil), first (gtt) A, !, parse [A,te _2541,B|Alpha] Beta Result Str.
parse [gtt,te _2541,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7585121 :: comma :: lbrack :: bofs :: nil), first (te _2553) A, !, parse [A,gtt,te _2541,B|Alpha] Beta Result Str.
parse [te _2596,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7597521 :: comma :: lbrack :: bofs :: nil), first (let) A, !, parse [A,te _2596,B|Alpha] Beta Result Str.
parse [let,te _2596,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7597521 :: comma :: lbrack :: bofs :: nil), first (te _2608) A, !, parse [A,let,te _2596,B|Alpha] Beta Result Str.
parse [te _2651,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7609921 :: comma :: lbrack :: bofs :: nil), first (get) A, !, parse [A,te _2651,B|Alpha] Beta Result Str.
parse [get,te _2651,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7609921 :: comma :: lbrack :: bofs :: nil), first (te _2663) A, !, parse [A,get,te _2651,B|Alpha] Beta Result Str.
parse [te _2706,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7622321 :: comma :: lbrack :: bofs :: nil), first (neqt) A, !, parse [A,te _2706,B|Alpha] Beta Result Str.
parse [neqt,te _2706,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7622321 :: comma :: lbrack :: bofs :: nil), first (te _2718) A, !, parse [A,neqt,te _2706,B|Alpha] Beta Result Str.
parse [lvalue _2761,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7632485 :: comma :: lbrack :: bofs :: nil), first (assignt) A, !, parse [A,lvalue _2761,B|Alpha] Beta Result Str.
parse [assignt,lvalue _2761,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7632485 :: comma :: lbrack :: bofs :: nil), first (te _2773) A, !, parse [A,assignt,lvalue _2761,B|Alpha] Beta Result Str.
parse [lvalue _2815,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7649070 :: comma :: lbrack :: bofs :: nil), first (lbrack) A, !, parse [A,lvalue _2815,B|Alpha] Beta Result Str.
parse [lbrack,lvalue _2815,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7649070 :: comma :: lbrack :: bofs :: nil), first (te _2826) A, !, parse [A,lbrack,lvalue _2815,B|Alpha] Beta Result Str.
parse [te _2826,lbrack,lvalue _2815,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7649070 :: comma :: lbrack :: bofs :: nil), first (rbrack) A, !, parse [A,te _2826,lbrack,lvalue _2815,B|Alpha] Beta Result Str.
parse [rbrack,te _2826,lbrack,lvalue _2815,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7649070 :: comma :: lbrack :: bofs :: nil), first (oft) A, !, parse [A,rbrack,te _2826,lbrack,lvalue _2815,B|Alpha] Beta Result Str.
parse [oft,rbrack,te _2826,lbrack,lvalue _2815,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7649070 :: comma :: lbrack :: bofs :: nil), first (te _2841) A, !, parse [A,oft,rbrack,te _2826,lbrack,lvalue _2815,B|Alpha] Beta Result Str.
parse [whilet,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7661637 :: comma :: lbrack :: bofs :: nil), first (te _2880) A, !, parse [A,whilet,B|Alpha] Beta Result Str.
parse [te _2880,whilet,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7661637 :: comma :: lbrack :: bofs :: nil), first (dot) A, !, parse [A,te _2880,whilet,B|Alpha] Beta Result Str.
parse [dot,te _2880,whilet,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7661637 :: comma :: lbrack :: bofs :: nil), first (te _2892) A, !, parse [A,dot,te _2880,whilet,B|Alpha] Beta Result Str.
parse [fort,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7680792 :: comma :: lbrack :: bofs :: nil), first (id _2937) A, !, parse [A,fort,B|Alpha] Beta Result Str.
parse [id _2937,fort,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7680792 :: comma :: lbrack :: bofs :: nil), first (assignt) A, !, parse [A,id _2937,fort,B|Alpha] Beta Result Str.
parse [assignt,id _2937,fort,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7680792 :: comma :: lbrack :: bofs :: nil), first (te _2948) A, !, parse [A,assignt,id _2937,fort,B|Alpha] Beta Result Str.
parse [te _2948,assignt,id _2937,fort,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7680792 :: comma :: lbrack :: bofs :: nil), first (tot) A, !, parse [A,te _2948,assignt,id _2937,fort,B|Alpha] Beta Result Str.
parse [tot,te _2948,assignt,id _2937,fort,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7680792 :: comma :: lbrack :: bofs :: nil), first (te _2959) A, !, parse [A,tot,te _2948,assignt,id _2937,fort,B|Alpha] Beta Result Str.
parse [te _2959,tot,te _2948,assignt,id _2937,fort,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7680792 :: comma :: lbrack :: bofs :: nil), first (dot) A, !, parse [A,te _2959,tot,te _2948,assignt,id _2937,fort,B|Alpha] Beta Result Str.
parse [dot,te _2959,tot,te _2948,assignt,id _2937,fort,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7680792 :: comma :: lbrack :: bofs :: nil), first (te _2971) A, !, parse [A,dot,te _2959,tot,te _2948,assignt,id _2937,fort,B|Alpha] Beta Result Str.
parse [lett,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7689107 :: comma :: lbrack :: bofs :: nil), first (decs _3056) A, !, parse [A,lett,B|Alpha] Beta Result Str.
parse [decs _3056,lett,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7689107 :: comma :: lbrack :: bofs :: nil), first (intok) A, !, parse [A,decs _3056,lett,B|Alpha] Beta Result Str.
parse [intok,decs _3056,lett,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7689107 :: comma :: lbrack :: bofs :: nil), first (expseq _3067) A, !, parse [A,intok,decs _3056,lett,B|Alpha] Beta Result Str.
parse [expseq _3067,intok,decs _3056,lett,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7689107 :: comma :: lbrack :: bofs :: nil), first (endt) A, !, parse [A,expseq _3067,intok,decs _3056,lett,B|Alpha] Beta Result Str.
parse [ift,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (intok :: ift :: lparen :: semicolon :: thent :: elset :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7707928 :: comma :: lbrack :: bofs :: nil), first (te _3114) A, !, parse [A,ift,B|Alpha] Beta Result Str.
parse [te _3114,ift,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (intok :: ift :: lparen :: semicolon :: thent :: elset :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7707928 :: comma :: lbrack :: bofs :: nil), first (thent) A, !, parse [A,te _3114,ift,B|Alpha] Beta Result Str.
parse [thent,te _3114,ift,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (intok :: ift :: lparen :: semicolon :: thent :: elset :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7707928 :: comma :: lbrack :: bofs :: nil), first (te _3125) A, !, parse [A,thent,te _3114,ift,B|Alpha] Beta Result Str.
parse [te _3125,thent,te _3114,ift,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (intok :: ift :: lparen :: semicolon :: thent :: elset :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7707928 :: comma :: lbrack :: bofs :: nil), first (elset) A, !, parse [A,te _3125,thent,te _3114,ift,B|Alpha] Beta Result Str.
parse [elset,te _3125,thent,te _3114,ift,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (intok :: ift :: lparen :: semicolon :: thent :: elset :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7707928 :: comma :: lbrack :: bofs :: nil), first (te _3137) A, !, parse [A,elset,te _3125,thent,te _3114,ift,B|Alpha] Beta Result Str.
parse [ift,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (ift :: lparen :: semicolon :: thent :: elset :: intok :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7720495 :: comma :: lbrack :: bofs :: nil), first (te _3183) A, !, parse [A,ift,B|Alpha] Beta Result Str.
parse [te _3183,ift,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (ift :: lparen :: semicolon :: thent :: elset :: intok :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7720495 :: comma :: lbrack :: bofs :: nil), first (thent) A, !, parse [A,te _3183,ift,B|Alpha] Beta Result Str.
parse [thent,te _3183,ift,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (ift :: lparen :: semicolon :: thent :: elset :: intok :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7720495 :: comma :: lbrack :: bofs :: nil), first (te _3195) A, !, parse [A,thent,te _3183,ift,B|Alpha] Beta Result Str.
parse [lparen,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (ift :: lparen :: semicolon :: thent :: elset :: intok :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7724653 :: comma :: lbrack :: bofs :: nil), first (expseq _3241) A, !, parse [A,lparen,B|Alpha] Beta Result Str.
parse [expseq _3241,lparen,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (ift :: lparen :: semicolon :: thent :: elset :: intok :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7724653 :: comma :: lbrack :: bofs :: nil), first (rparen) A, !, parse [A,expseq _3241,lparen,B|Alpha] Beta Result Str.
parse [createrec _3286,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lparen :: semicolon :: ift :: thent :: elset :: intok :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7732323 :: comma :: lbrack :: bofs :: nil), first (moreinits _3294) A, !, parse [A,createrec _3286,B|Alpha] Beta Result Str.
parse [moreinits _3294,createrec _3286,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lparen :: semicolon :: ift :: thent :: elset :: intok :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7732323 :: comma :: lbrack :: bofs :: nil), first (rbrace) A, !, parse [A,moreinits _3294,createrec _3286,B|Alpha] Beta Result Str.
parse [id _3340,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lparen :: semicolon :: ift :: thent :: elset :: intok :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7732716 :: comma :: lbrack :: bofs :: nil), first (lbrace) A, !, parse [A,id _3340,B|Alpha] Beta Result Str.
parse [id _3380,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (createrec _3897767 :: acomma :: nil), first (eqt) A, !, parse [A,id _3380,B|Alpha] Beta Result Str.
parse [eqt,id _3380,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (createrec _3897767 :: acomma :: nil), first (te _3392) A, !, parse [A,eqt,id _3380,B|Alpha] Beta Result Str.
parse [id _3450,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (createrec _3911692 :: acomma :: nil), first (eqt) A, !, parse [A,id _3450,B|Alpha] Beta Result Str.
parse [eqt,id _3450,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (createrec _3911692 :: acomma :: nil), first (te _3461) A, !, parse [A,eqt,id _3450,B|Alpha] Beta Result Str.
parse [te _3461,eqt,id _3450,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (createrec _3911692 :: acomma :: nil), first (acomma) A, !, parse [A,te _3461,eqt,id _3450,B|Alpha] Beta Result Str.
parse [acomma,te _3461,eqt,id _3450,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (createrec _3911692 :: acomma :: nil), first (moreinits _3473) A, !, parse [A,acomma,te _3461,eqt,id _3450,B|Alpha] Beta Result Str.
parse [args _3611,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (funcallh _7742794 :: nil), first (comma) A, !, parse [A,args _3611,B|Alpha] Beta Result Str.
parse [comma,args _3611,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (funcallh _7742794 :: nil), first (te _3623) A, !, parse [A,comma,args _3611,B|Alpha] Beta Result Str.
parse [te _3708,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lparen :: semicolon :: intok :: nil), first (semicolon) A, !, parse [A,te _3708,B|Alpha] Beta Result Str.
parse [semicolon,te _3708,B|Alpha] [A|Beta] Result "shift" :- 
	 member B (lparen :: semicolon :: intok :: nil), first (expseq _3720) A, !, parse [A,semicolon,te _3708,B|Alpha] Beta Result Str.
parse [bofs] [A|B] R "shift" :- 
	first (program _17254454) A, parse [A,bofs] B R Str.
parse [Sx,bofs] [eofs] Sx "accept" :- start_symbol Sx.
parse [te _360,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (bofs :: nil), member Sym (eofs :: nil), first Sym A, !, 
	(_351 = _360), parse [program _351,B|Alpha] [A|Beta] Result Str.
parse [tydec_gs _397,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2903774 :: nil), member Sym (intok :: rdbrace :: decs _13210100 :: nil), first Sym A, !, 
	(_388 = _397), parse [dec _388,B|Alpha] [A|Beta] Result Str.
parse [vdec_gs _434,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2915832 :: nil), member Sym (intok :: rdbrace :: decs _13230279 :: nil), first Sym A, !, 
	(_388 = _434), parse [dec _388,B|Alpha] [A|Beta] Result Str.
parse [fdec_gs _471,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2933456 :: nil), member Sym (intok :: rdbrace :: decs _13543978 :: nil), first Sym A, !, 
	(_388 = _471), parse [dec _388,B|Alpha] [A|Beta] Result Str.
parse [cdec_gs _508,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2986587 :: nil), member Sym (intok :: rdbrace :: decs _13596550 :: nil), first Sym A, !, 
	(_388 = _508), parse [dec _388,B|Alpha] [A|Beta] Result Str.
parse [dec _545,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2900036 :: nil), member Sym (intok :: rdbrace :: nil), first Sym A, !, 
	(_536 = _545 :: nil), parse [decs _536,B|Alpha] [A|Beta] Result Str.
parse [decs _596,dec _587,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (publict :: dec _3002939 :: lett :: nil), member Sym (rdbrace :: intok :: nil), first Sym A, !, 
	(_536 = _587 :: _596), parse [decs _536,B|Alpha] [A|Beta] Result Str.
parse [te _652,assignt,id _640,vart,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2922479 :: nil), member Sym (intok :: rdbrace :: decs _13236926 :: nil), first Sym A, !, 
	(_629 = vardec _640 dummytype _652), parse [vdec_gs _629,B|Alpha] [A|Beta] Result Str.
parse [te _721,assignt,id _709,colon,id _698,vart,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2929460 :: nil), member Sym (intok :: rdbrace :: decs _13539982 :: nil), first Sym A, !, 
	(_629 = vardec _698 (namety _709) _721), parse [vdec_gs _629,B|Alpha] [A|Beta] Result Str.
parse [ty_gs _778,tydechd _769,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2911449 :: nil), member Sym (intok :: rdbrace :: decs _13217775 :: nil), first Sym A, !, 
	(_761 = typedec _769 _778), parse [tydec_gs _761,B|Alpha] [A|Beta] Result Str.
parse [eqt,id _823,typet,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2912009 :: nil), member Sym (ty_gs _10864722 :: nil), first Sym A, !, 
	(_812 = _823), parse [tydechd _812,B|Alpha] [A|Beta] Result Str.
parse [rdbrace,decs _891,publict,tfields _880,privatet,ldbrace,id _866,classt,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2995178 :: nil), member Sym (intok :: rdbrace :: decs _13605141 :: nil), first Sym A, !, 
	(_855 = classdec _866 _880 _891), parse [cdec_gs _855,B|Alpha] [A|Beta] Result Str.
parse [te _961,eqt,rparen,tfields _946,fundech _938,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2947380 :: nil), member Sym (intok :: rdbrace :: decs _13557902 :: nil), first Sym A, !, 
	(formfix _938 _946 dummytype _961 _930), parse [fdec_gs _930,B|Alpha] [A|Beta] Result Str.
parse [te _1032,eqt,id _1020,colon,rparen,tfields _1006,fundech _998,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2962197 :: nil), member Sym (intok :: rdbrace :: decs _13572160 :: nil), first Sym A, !, 
	(formfix _998 _1006 (namety _1020) _1032 _930), parse [fdec_gs _930,B|Alpha] [A|Beta] Result Str.
parse [te _1089,eqt,rparen,fundech _1074,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2972356 :: nil), member Sym (intok :: rdbrace :: decs _13582319 :: nil), first Sym A, !, 
	(formfix _1074 nil dummytype _1089 _930), parse [fdec_gs _930,B|Alpha] [A|Beta] Result Str.
parse [te _1153,eqt,id _1141,colon,rparen,fundech _1127,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2982849 :: nil), member Sym (intok :: rdbrace :: decs _13592812 :: nil), first Sym A, !, 
	(formfix _1127 nil (namety _1141) _1153 _930), parse [fdec_gs _930,B|Alpha] [A|Beta] Result Str.
parse [lparen,id _1199,functiont,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lett :: publict :: dec _2947940 :: nil), member Sym (tfields _12731197 :: rparen :: nil), first Sym A, !, 
	(_1188 = _1199), parse [fundech _1188,B|Alpha] [A|Beta] Result Str.
parse [id _1240,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (tydechd _2662794 :: nil), member Sym (intok :: rdbrace :: decs _13218001 :: nil), first Sym A, !, 
	(_1231 = namety _1240), parse [ty_gs _1231,B|Alpha] [A|Beta] Result Str.
parse [rbrace,tfields _1284,lbrace,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (tydechd _2666952 :: nil), member Sym (intok :: rdbrace :: decs _13222158 :: nil), first Sym A, !, 
	(_1231 = recordty _1284), parse [ty_gs _1231,B|Alpha] [A|Beta] Result Str.
parse [rbrace,lbrace,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (tydechd _2684680 :: nil), member Sym (intok :: rdbrace :: decs _13222551 :: nil), first Sym A, !, 
	(_1231 = recordty nil), parse [ty_gs _1231,B|Alpha] [A|Beta] Result Str.
parse [id _1374,ty_gshd,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (tydechd _2688585 :: nil), member Sym (intok :: rdbrace :: decs _13226455 :: nil), first Sym A, !, 
	(_1231 = arrayty _1374), parse [ty_gs _1231,B|Alpha] [A|Beta] Result Str.
parse [oft,arrayt,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (tydechd _2688979 :: nil), member Sym (id _10882560 :: nil), first Sym A, !, 
	(true), parse [ty_gshd,B|Alpha] [A|Beta] Result Str.
parse [id _1449,colon,id _1437,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lbrace :: tyfcomma :: fundech _2725445 :: privatet :: nil), member Sym (rbrace :: rparen :: publict :: nil), first Sym A, !, 
	(_1429 = etpair (varexp (simplevar _1437)) (namety _1449) :: nil), parse [tfields _1429,B|Alpha] [A|Beta] Result Str.
parse [tfields _1535,tyfcomma,id _1523,colon,id _1512,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lbrace :: tyfcomma :: fundech _2733448 :: privatet :: nil), member Sym (rbrace :: rparen :: publict :: nil), first Sym A, !, 
	(_1429 = etpair (varexp (simplevar _1512)) (namety _1523) :: _1535), parse [tfields _1429,B|Alpha] [A|Beta] Result Str.
parse [id _1598,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (bofs :: lbrack :: funcallh _74155 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: nil), member Sym (eofs :: dott :: lbrack :: rbrack :: lparen :: rparen :: timest :: dividet :: minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13241287 :: endt :: comma :: nil), first Sym A, !, 
	(_1589 = simplevar _1598), parse [lvalue _1589,B|Alpha] [A|Beta] Result Str.
parse [id _1651,dott,lvalue _1639,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (bofs :: lbrack :: funcallh _78398 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: nil), member Sym (eofs :: dott :: lbrack :: rbrack :: lparen :: rparen :: timest :: dividet :: minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13245530 :: endt :: comma :: nil), first Sym A, !, 
	(_1589 = fieldvar _1639 _1651), parse [lvalue _1589,B|Alpha] [A|Beta] Result Str.
parse [rbrack,te _1704,lbrack,lvalue _1693,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (bofs :: lbrack :: funcallh _88729 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: nil), member Sym (dott :: lbrack :: rbrack :: lparen :: rparen :: timest :: dividet :: minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13255861 :: endt :: comma :: eofs :: nil), first Sym A, !, 
	(_1589 = subscriptvar _1693 _1704), parse [lvalue _1589,B|Alpha] [A|Beta] Result Str.
parse [nilt,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (bofs :: lbrack :: funcallh _70020 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: nil), member Sym (eofs :: rbrack :: rparen :: timest :: dividet :: minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13237152 :: endt :: comma :: nil), first Sym A, !, 
	(_1742 = nilexp), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [lvalue _1783,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (bofs :: lbrack :: funcallh _73929 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: nil), member Sym (eofs :: rbrack :: rparen :: timest :: dividet :: minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13241061 :: endt :: comma :: nil), first Sym A, !, 
	(_1742 = varexp _1783), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [iconst _1825,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lbrack :: funcallh _88955 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: bofs :: nil), member Sym (rbrack :: rparen :: timest :: dividet :: minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13256087 :: endt :: comma :: eofs :: nil), first Sym A, !, 
	(_1742 = intexp _1825), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [sconst _1867,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lbrack :: funcallh _89180 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: bofs :: nil), member Sym (rbrack :: rparen :: timest :: dividet :: minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13256312 :: endt :: comma :: eofs :: nil), first Sym A, !, 
	(_1742 = stringexp _1867), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [rparen,args _1916,funcallh _1908,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lbrack :: funcallh _96849 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: bofs :: nil), member Sym (rbrack :: rparen :: timest :: dividet :: minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13263981 :: endt :: comma :: eofs :: nil), first Sym A, !, 
	(_1742 = callexp _1908 _1916), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [rparen,funcallh _1962,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (funcallh _104832 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: lbrack :: bofs :: nil), member Sym (rparen :: timest :: dividet :: minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13267886 :: endt :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = callexp _1962 nil), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [lparen,lvalue _2009,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lbrack :: funcallh _100926 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: bofs :: nil), member Sym (args _8673077 :: rparen :: nil), first Sym A, !, 
	(_2001 = _2009), parse [funcallh _2001,B|Alpha] [A|Beta] Result Str.
parse [te _2053,minust,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (funcallh _111146 :: minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: comma :: lbrack :: bofs :: nil), member Sym (rparen :: timest :: dividet :: minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13274199 :: endt :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = opexp "-" (intexp 0) _2053), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2113,timest,te _2101,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (minust :: timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: funcallh _7485921 :: comma :: lbrack :: bofs :: nil), member Sym (rparen :: timest :: dividet :: minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13286599 :: endt :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = opexp "*" _2101 _2113), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2168,dividet,te _2156,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (timest :: dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: minust :: funcallh _7498321 :: comma :: lbrack :: bofs :: nil), member Sym (timest :: dividet :: minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13298999 :: endt :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = opexp "div" _2156 _2168), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2223,minust,te _2211,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: timest :: minust :: funcallh _7510721 :: comma :: lbrack :: bofs :: nil), member Sym (dividet :: minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13311399 :: endt :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = opexp "-" _2211 _2223), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2278,plust,te _2266,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (dividet :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: timest :: minust :: funcallh _7523121 :: comma :: lbrack :: bofs :: nil), member Sym (minust :: plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13323799 :: endt :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = opexp "+" _2266 _2278), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2333,ort,te _2321,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: dividet :: timest :: minust :: funcallh _7535521 :: comma :: lbrack :: bofs :: nil), member Sym (plust :: ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13336199 :: endt :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = opexp "or" _2321 _2333), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2388,andt,te _2376,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: plust :: dividet :: timest :: minust :: funcallh _7547921 :: comma :: lbrack :: bofs :: nil), member Sym (ort :: andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13348599 :: endt :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = opexp "and" _2376 _2388), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2443,eqt,te _2431,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: ort :: plust :: dividet :: timest :: minust :: funcallh _7560321 :: comma :: lbrack :: bofs :: nil), member Sym (andt :: eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13360999 :: endt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = opexp "=" _2431 _2443), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2498,ltt,te _2486,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (eqt :: ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7572721 :: comma :: lbrack :: bofs :: nil), member Sym (eqt :: ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13373399 :: endt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = opexp "<" _2486 _2498), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2553,gtt,te _2541,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (ltt :: gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7585121 :: comma :: lbrack :: bofs :: nil), member Sym (ltt :: gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13385799 :: endt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = opexp ">" _2541 _2553), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2608,let,te _2596,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (gtt :: let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7597521 :: comma :: lbrack :: bofs :: nil), member Sym (gtt :: let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13398199 :: endt :: ltt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = opexp "<=" _2596 _2608), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2663,get,te _2651,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (let :: get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7609921 :: comma :: lbrack :: bofs :: nil), member Sym (let :: get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13410599 :: endt :: gtt :: ltt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = opexp ">=" _2651 _2663), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2718,neqt,te _2706,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (get :: neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7622321 :: comma :: lbrack :: bofs :: nil), member Sym (get :: neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13422999 :: endt :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = opexp "<>" _2706 _2718), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2773,assignt,lvalue _2761,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (neqt :: assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7632485 :: comma :: lbrack :: bofs :: nil), member Sym (neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13433163 :: endt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = assignexp _2761 _2773), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2841,oft,rbrack,te _2826,lbrack,lvalue _2815,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (assignt :: oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7649070 :: comma :: lbrack :: bofs :: nil), member Sym (neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13449748 :: endt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(mkarrayexp _2815 _2826 _2841 _1742), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2892,dot,te _2880,whilet,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (oft :: whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7661637 :: comma :: lbrack :: bofs :: nil), member Sym (neqt :: dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13462315 :: endt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = whileexp _2880 _2892), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _2971,dot,te _2959,tot,te _2948,assignt,id _2937,fort,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (whilet :: tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7680792 :: comma :: lbrack :: bofs :: nil), member Sym (dot :: tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13481470 :: endt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = forexp (vardec _2937 dummytype _2948) _2959 _2971), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [breakt,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7681018 :: comma :: lbrack :: bofs :: nil), member Sym (tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13481696 :: endt :: dot :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = breakexp), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [endt,expseq _3067,intok,decs _3056,lett,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (tot :: intok :: ift :: lparen :: semicolon :: thent :: elset :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7689107 :: comma :: lbrack :: bofs :: nil), member Sym (tot :: intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13489785 :: endt :: dot :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(formlet _3056 (seqexp _3067) _1742), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _3137,elset,te _3125,thent,te _3114,ift,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (intok :: ift :: lparen :: semicolon :: thent :: elset :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7707928 :: comma :: lbrack :: bofs :: nil), member Sym (intok :: thent :: semicolon :: rbrace :: acomma :: elset :: rdbrace :: decs _13508606 :: endt :: tot :: dot :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = ifexp _3114 _3125 _3137), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [te _3195,thent,te _3183,ift,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (ift :: lparen :: semicolon :: thent :: elset :: intok :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7720495 :: comma :: lbrack :: bofs :: nil), member Sym (thent :: semicolon :: rbrace :: acomma :: elset :: intok :: rdbrace :: decs _13521173 :: endt :: tot :: dot :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = ifexp _3183 _3195 dummyexp), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [rparen,expseq _3241,lparen,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (ift :: lparen :: semicolon :: thent :: elset :: intok :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7724653 :: comma :: lbrack :: bofs :: nil), member Sym (thent :: semicolon :: rbrace :: acomma :: elset :: intok :: rdbrace :: decs _13525331 :: endt :: tot :: dot :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = seqexp _3241), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [rbrace,moreinits _3294,createrec _3286,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lparen :: semicolon :: ift :: thent :: elset :: intok :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7732323 :: comma :: lbrack :: bofs :: nil), member Sym (semicolon :: rbrace :: acomma :: thent :: elset :: intok :: rdbrace :: decs _13533001 :: endt :: tot :: dot :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: minust :: dividet :: timest :: rparen :: comma :: rbrack :: eofs :: nil), first Sym A, !, 
	(_1742 = recordexp _3286 _3294), parse [te _1742,B|Alpha] [A|Beta] Result Str.
parse [lbrace,id _3340,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lparen :: semicolon :: ift :: thent :: elset :: intok :: tot :: whilet :: dot :: oft :: assignt :: neqt :: get :: let :: gtt :: ltt :: eqt :: andt :: ort :: plust :: dividet :: timest :: minust :: funcallh _7732716 :: comma :: lbrack :: bofs :: nil), member Sym (moreinits _11765336 :: nil), first Sym A, !, 
	(_3332 = _3340), parse [createrec _3332,B|Alpha] [A|Beta] Result Str.
parse [te _3392,eqt,id _3380,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (createrec _3897767 :: acomma :: nil), member Sym (rbrace :: nil), first Sym A, !, 
	(_3372 = eepair (varexp (simplevar _3380)) _3392 :: nil), parse [moreinits _3372,B|Alpha] [A|Beta] Result Str.
parse [moreinits _3473,acomma,te _3461,eqt,id _3450,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (createrec _3911692 :: acomma :: nil), member Sym (rbrace :: nil), first Sym A, !, 
	(_3372 = eepair (varexp (simplevar _3450)) _3461 :: _3473), parse [moreinits _3372,B|Alpha] [A|Beta] Result Str.
parse [comma,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (te _3911919 :: nil), member Sym (moreinits _12377788 :: nil), first Sym A, !, 
	(true), parse [acomma,B|Alpha] [A|Beta] Result Str.
parse [comma,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (id _2675734 :: nil), member Sym (tfields _10877878 :: nil), first Sym A, !, 
	(true), parse [tyfcomma,B|Alpha] [A|Beta] Result Str.
parse [te _3569,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (funcallh _69794 :: nil), member Sym (rparen :: comma :: nil), first Sym A, !, 
	(_3560 = _3569 :: nil), parse [args _3560,B|Alpha] [A|Beta] Result Str.
parse [te _3623,comma,args _3611,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (funcallh _7742794 :: nil), member Sym (rparen :: comma :: nil), first Sym A, !, 
	(append _3611 (_3623 :: nil) _3560), parse [args _3560,B|Alpha] [A|Beta] Result Str.
parse [te _3665,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (intok :: lparen :: semicolon :: nil), member Sym (rparen :: endt :: nil), first Sym A, !, 
	(_3656 = _3665 :: nil), parse [expseq _3656,B|Alpha] [A|Beta] Result Str.
parse [expseq _3720,semicolon,te _3708,B|Alpha] [A|Beta] Result "reduce" :- 
	member B (lparen :: semicolon :: intok :: nil), member Sym (rparen :: endt :: nil), first Sym A, !, 
	(_3656 = _3708 :: _3720), parse [expseq _3656,B|Alpha] [A|Beta] Result Str.

parse V I R "error" :- !, finderrline V I, print "Remaining Input = ", print_tokens 10 I, !, fail.
