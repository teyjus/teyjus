module lpgram.
accum_sig lambdayacc.
accumulate lambdayacc.

nonterm_list 
[			    
	(ary1 sigormod),(ary1 signaturedef),(ary1 moduledef),(ary1 modpreamble),(ary1 sigpreamble),
	(ary1 modbody),(ary1 signdecls),(ary1 signdecl),(ary1 modsigndecl),(ary1 idlist),(ary1 atype),
	(ary1 akind),(ary1 modclause),(ary1 modheader),(ary1 sigheader),(ary0 tcomma),
	(ary1 fixity), (ary1 clause), (ary1 term), (ary1 termlist),
	(ary1 piid),(ary1 sigmaid),(ary1 typedid),(ary1 nontermst),(ary0 telparen),(ary1 modpreamblecl)
].

terminal_list 
[
	      lparen,rparen,(iconst V),(id S),(sconst St),negt,dott,
	      pit,sigmat,impt,rimpt,arrowt,kindt,typet,slasht, colon,
	      trutht,falset,comma,semicolon,formt,modulet, sigt, gentermt,
	      accumsigt,accumulatet,importt,localt,localkindt,closedt, infixlt, infixrt,
	      infixt, prefixt, prefixrt, postfixt, postfixlt, exportdeft, useonlyt, cutt,
	      addt, subt, mult, divt, modt, andt, ltt, gtt, ltet, gtet, eqlt, realt,
	      intt,stringt,listt,vert,lbracket,rbracket,concatt, percent, nilt, failt, ist,	
	      lparen1,lparen2,lparen3,lparen4,lparen5,sconcatt, instreamt,outstreamt,
	      haltt, stopt, inttorealt, abst, sqrtt, sint, cost, arctant, lnt, floort, ceilt,
	      truncatet, rabst, sizet, chrt, stringtointt, substringt, inttostringt, realtostringt,
	      stdint, stdoutt, stderrt, printtermt, readtermt,
	      openint, openoutt, openappendt, openstringt, closeint, closeoutt, termtostringt,
	      stringtotermt, inputt, outputt, inputlinet, lookaheadt, eoft, flusht, printt, readt

].
%%
comment_list ["%"].

tokchar S :- alltyp S, C is string_to_int S,
	     not (C = 46; C = 44; C = 41; C = 40; C = 59; C = 91; C = 93; C = 124; C = 58; C = 92; C = 34; C = 126).

%%% tokenizer declarations:
printname _ "true" trutht.
printname _ "false" falset.
printname _ "fail" failt.
printname _ "not" negt.
printname _ "," comma.
printname _ ";" semicolon.
printname _ ":" colon.
printname _ "::" concatt.
printname _ "nil" nilt.
printname _ "pi" pit.
printname _ "sigma" sigmat.
printname _ "=>" impt.
printname _ ":-" rimpt.
printname _ "(" lparen.
printname _ "(" lparen1.
printname _ "(" lparen2.
printname _ "(" lparen3.
printname _ "(" lparen4.
printname _ ")" rparen.
printname _ "[" lbracket.
printname _ "]" rbracket.
printname _ "|" vert.
printname _ "\\" slasht.
printname _ "." dott.
printname _ "&" andt.
printname _ "type" typet.
printname _ "kind" kindt.
printname _ "->" arrowt.
printname _ "_" gentermt.
printname _ "o" formt.
printname _ "%" percent.
printname _ "^" sconcatt.

% Additions from here down
printname _ "module" modulet.
printname _ "sig" sigt.
printname _ "accum_sig" accumsigt.
printname _ "accumulate" accumulatet.
printname _ "import" importt.
printname _ "local" localt.
printname _ "localkind" localkindt.
printname _ "closed" closedt.

printname _ "infixl" infixlt.
printname _ "infixr" infixrt.
printname _ "infix" infixt.
printname _ "prefix" prefixt.
printname _ "prefixr" prefixrt.
printname _ "postfix" postfixt.
printname _ "postfixl" postfixlt.

printname _ "exportdef" exportdeft.
printname _ "useonly" useonlyt.

printname _ "!" cutt.

% Mathematical operations
printname _ "+" addt.
printname _ "-" subt.
printname _ "*" mult.
printname _ "div" divt.
printname _ "mod" modt.
printname _ "=" eqlt.
printname _ "is" ist.
printname _ "<" ltt.
printname _ ">" gtt.
printname _ ">=" gtet.
printname _ "=<" ltet.

printname _ "int" intt.
printname _ "string" stringt.
printname _ "real" realt.
printname _ "list" listt.
printname _ "out_stream" outstreamt.
printname _ "in_stream" instreamt.

printname _ "halt" haltt.
printname _ "stop" stopt.
printname _ "int_to_real" inttorealt.
printname _ "abs" abst.
printname _ "sqrt" sqrtt.
printname _ "sin" sint.
printname _ "cos" cost.
printname _ "arctan" arctant.
printname _ "ln" lnt.
printname _ "floor" floort.
printname _ "ceil" ceilt.
printname _ "truncate" truncatet.
printname _ "rabs" rabst.
printname _ "size" sizet.
printname _ "chr" chrt.
printname _ "string_to_int" stringtointt.
printname _ "substring" substringt.
printname _ "int_to_string" inttostringt.
printname _ "real_to_string" realtostringt.
printname _ "std_out" stdoutt.
printname _ "std_in" stdint.
printname _ "stderr" stderrt.

printname _ "open_in" openint.
printname _ "open_out" openoutt.
printname _ "open_append" openappendt.
printname _ "open_string" openstringt.
printname _ "close_in" closeint.
printname _ "close_out" closeoutt.
printname _ "term_to_string" termtostringt.
printname _ "string_to_term" stringtotermt.
printname _ "input" inputt.
printname _ "output" outputt.
printname _ "input_line" inputlinet.
printname _ "lookahead" lookaheadt.
printname _ "eof" eoft.
printname _ "flush" flusht.
printname _ "print" printt.
printname _ "read" readt.
printname _ "printterm" printtermt.
printname _ "readterm" readtermt.

% id, iconst, sconst are universal

start_symbol (sigormod X).


cfg
[
rule ((sigormod Sm1) ==> [signaturedef Sigdef]) (Sm1 = Sigdef),
rule ((sigormod Sm2) ==> [moduledef Moddef]) (Sm2 = Moddef),

rule ((signaturedef SIGNATURE1) ==> [sigheader SH1,sigpreamble SP1,signdecls SD1]) (SIGNATURE1 = definesig SH1 SP1 SD1),
rule ((signaturedef SIGNATURE2) ==> [sigheader SH2,sigpreamble SP2]) (SIGNATURE2 = definesig SH2 SP2 nil),
rule ((signaturedef SIGNATURE3) ==> [sigheader SH3,signdecls SD2]) (SIGNATURE3 = definesig SH3 nil SD2),
rule ((signaturedef SIGNATURE4) ==> [sigheader SH4]) (SIGNATURE4 = definesig SH4 nil nil),

rule ((moduledef MODULE1) ==> [modheader MH1,modpreamble MP1,modbody MB1]) (MODULE1 = definemod MH1 MP1 MB1),
rule ((moduledef MODULE2) ==> [modheader MH2,modpreamble MP2]) (MODULE2 = definemod MH2 MP2 nil),
rule ((moduledef MODULE3) ==> [modheader MH3,modbody MB2]) (MODULE3 = definemod MH3 nil MB2),
rule ((moduledef MODULE4) ==> [modheader MH4]) (MODULE4 = definemod MH4 nil nil),

rule ((modheader ModHead1) ==> [modulet,(id Modname),dott]) (ModHead1 = Modname),

rule ((sigheader SigHead1) ==> [sigt,(id Signame),dott]) (SigHead1 = Signame),

rule ((modpreamble ModPrel1) ==> [modpreamblecl ModPreCl1]) (ModPrel1 = [ModPreCl1]),
rule ((modpreamble ModPrel2) ==> [modpreamblecl ModPreCl2,modpreamble ModPrel3]) (ModPrel2 = [ModPreCl2|ModPrel3]),

rule ((modpreamblecl ModPre1) ==> [importt,idlist Importids,dott]) (ModPre1 = importmod Importids),
rule ((modpreamblecl ModPre2) ==> [accumulatet,idlist Accumids,dott]) (ModPre2 = accummod Accumids),
rule ((modpreamblecl ModPre3) ==> [accumsigt,idlist Saccumids,dott]) (ModPre3 = saccummod Saccumids),

rule ((sigpreamble SigPre1) ==> [accumsigt,idlist Ssaccumids,dott,sigpreamble SigPrein1]) (SigPre1 = [saccummod Ssaccumids|SigPrein1]),
rule ((sigpreamble SigPre2) ==> [accumsigt,idlist Ssaccumids2,dott]) (SigPre2 = [saccummod Ssaccumids2]),

rule ((modbody ModBody1) ==> [modsigndecl Modsd1,modbody ModBodyin1]) (ModBody1 = [Modsd1|ModBodyin1]),
rule ((modbody ModBody2) ==> [modclause Modclause1,modbody ModBodyin2]) (ModBody2 = [modcls Modclause1|ModBodyin2]),
rule ((modbody ModBody3) ==> [modsigndecl Modsd2]) (ModBody3 = [Modsd2]),
rule ((modbody ModBody4) ==> [modclause Modclause4]) (ModBody4 = [modcls Modclause4]),

rule ((signdecls Signd1) ==> [signdecl Signdecl,signdecls Signdin1]) (Signd1 = [Signdecl|Signdin1]),
rule ((signdecls Signd2) ==> [signdecl Signdecl1]) (Signd2 = [Signdecl1]),

rule ((signdecl Signdec1) ==> [kindt,idlist Kinds,akind Kind,dott]) (Signdec1 = kinddec Kinds Kind),
rule ((signdecl Signdec2) ==> [typet,idlist Types,atype Type,dott]) (Signdec2 = typedec Types Type),
rule ((signdecl Signdec3) ==> [fixity Fixtype,idlist Fixes,iconst Fixlevel,dott]) (Signdec3 = fixdec Fixtype Fixes Fixlevel),
rule ((signdecl Signdec4) ==> [exportdeft,idlist Exports,dott]) (Signdec4 = expdec Exports notype),
rule ((signdecl Signdec5) ==> [exportdeft,idlist Exports2,atype Extype,dott]) (Signdec5 = expdec Exports2 Extype),
rule ((signdecl Signdec6) ==> [useonlyt,idlist Useonlys,dott]) (Signdec6 = usedec Useonlys notype),
rule ((signdecl Signdec7) ==> [useonlyt,idlist Useonlys2,atype Usetype,dott]) (Signdec7 = usedec Useonlys2 Usetype),

rule ((modsigndecl MSigndec1) ==> [signdecl MSign]) (MSigndec1 = MSign),
rule ((modsigndecl MSigndec2) ==> [localt,idlist Locals,dott]) (MSigndec2 = localdec Locals notype),
rule ((modsigndecl MSigndec3) ==> [localt,idlist Locals2,atype Localtype,dott]) (MSigndec3 = localdec Locals2 Localtype),
rule ((modsigndecl MSigndec4) ==> [localkindt,idlist Localkinds,dott]) (MSigndec4 = localkinddec Localkinds nokind),
rule ((modsigndecl MSigndec5) ==> [localkindt,idlist Localkinds2,akind Localkind,dott]) (MSigndec5 = localkinddec Localkinds Localkind),
rule ((modsigndecl MSigndec6) ==> [closedt,idlist Closeds,dott]) (MSigndec6 = closeddec Localkinds notype),
rule ((modsigndecl MSigndec7) ==> [localkindt,idlist Closeds2,atype Closedtype,dott]) (MSigndec7 = closeddec Closeds2 Closedtype),

rule ((akind Kind1) ==> [typet]) (Kind1 = basekind),
rule ((akind Kind2) ==> [akind Kind3,arrowt,typet]) (Kind2 = (Kind3 karr basekind)),

/*
rule ((atype Type1) ==> [atype CTypea1,arrowt,atype AType1]) (Type1 = (CTypea1 arr AType1)),
rule ((atype Type3) ==> [formt]) (Type3 = basetype),
rule ((atype Type4) ==> [listt]) (Type4 = listof),
rule ((atype Type5) ==> [atype Ctypec1,atype PrTypec2]) (Type5 = tapp Ctypec1 PrTypec2),
rule ((atype Type6) ==> [(id Idprt1)]) (Type6 = userty Idprt1),
rule ((atype Type7) ==> [lparen,atype ATypepr1,rparen]) (Type7 = ATypepr1),
rule ((atype Type8) ==> [intt]) (Type8 = inttype),
rule ((atype Type9) ==> [stringt]) (Type9 = stringtype),
rule ((atype Type10) ==> [realt]) (Type10 = realtype),
rule ((atype Type11) ==> [instreamt]) (Type11 = intype),
rule ((atype Type12) ==> [outstreamt]) (Type12 = outtype),
*/

rule ((telparen) ==> [lparen]) (true),

rule ((fixity Fix1) ==> [infixt]) (Fix1 = inf),
rule ((fixity Fix2) ==> [infixlt]) (Fix2 = infl),
rule ((fixity Fix3) ==> [infixrt]) (Fix3 = infr),
rule ((fixity Fix4) ==> [prefixt]) (Fix4 = pref),
rule ((fixity Fix5) ==> [prefixrt]) (Fix5 = prefr),
rule ((fixity Fix6) ==> [postfixt]) (Fix6 = postf),
rule ((fixity Fix7) ==> [postfixlt]) (Fix7 = postfl),

rule ((modclause ModClause1) ==> [clause Clausemc1,dott]) (ModClause1 = Clausemc1),

rule ((clause Clause1) ==> [term Atomc1]) (Clause1 = Atomc1),

rule ((term Clause2) ==> [term Clausec1,rimpt,term Clausec1a]) (Clause2 = (rimp @ Clausec1 @ Clausec1a)),
rule ((term Clause3) ==> [term Clausec2,impt,term Clausec2a]) (Clause3 = (imp @ Clausec2 @ Clausec2a)),
rule ((term Clause4) ==> [term Clausec3,comma,term Clausec4]) (Clause4 = (and @ Clausec3 @ Clausec4)),
rule ((term Clause5) ==> [term Clausec5,andt,term Clausec6]) (Clause5 = (and @ Clausec5 @ Clausec6)),
rule ((term Clause7) ==> [piid Piidc2,term Termc1]) (Clause7 = ((forall Piidc2) @ Termc1)),
rule ((term Clause8) ==> [telparen,term Clausec8,rparen]) (Clause8 = Clausec8),
rule ((term Clause9) ==> [failt]) (Clause9 = failure),
rule ((term Clause10) ==> [trutht]) (Clause10 = truth),
rule ((term Clause11) ==> [falset]) (Clause11 = lies),
rule ((term GClause1) ==> [cutt]) (GClause1 = cut),
rule ((term GClause4) ==> [sigmaid Piidg3,term Termg2]) (GClause4 = ((thereexists Piidg3) @ Termg2)),
rule ((term GClause6) ==> [term GClauseg3,semicolon,term GClauseg4]) (GClause6 = (or @ GClauseg3 @ GClauseg4)),
rule ((term GClause7) ==> [negt]) (GClause7 = neg),
rule ((term Term1) ==> [term Termt1,term Termt2]) (Term1 = (Termt1 @ Termt2)),
rule ((term Term2) ==> [typedid Typedidt1,slasht,term Termt3]) (formlam Typedidt1 Termt3 Lamtermt1,Term2 = Lamtermt1),
rule ((term Term2a) ==> [id Typedidt1a,slasht,term Termt3a]) (formlam Typedidt1a Termt3a Lamtermt1a,Term2a = Lamtermt1a),
rule ((term Term3) ==> [lbracket,rbracket]) (Term3 = listwith nil),
rule ((term Term3b) ==> [lbracket,termlist Termt3b,rbracket]) (Term3b = listwith Termt3b),
rule ((term Term4) ==> [lbracket,term Termt4,vert,termlist Termlistt1,rbracket]) (Term4 = listwith [Termt4|Termlistt1]),
rule ((term Term6) ==> [id Idt1]) (Term6 = freevar Idt1),
rule ((term Term7) ==> [term Termlistt2,concatt,termlist Termt6]) (Term7 = listwith [Termlistt2|Termt6]),
rule ((term Term8) ==> [iconst Intterm]) (Term8 = intterm Intterm),
rule ((term Term9) ==> [sconst Stringterm]) (Term9 = stringterm Stringterm),
rule ((term Term10) ==> [nilt]) (Term10 = nillist),
rule ((term Term11) ==> [gentermt]) (Term11 = genterm),

rule ((term Addterm) ==> [term Add1,addt,term Add2]) (Addterm = (add @ Add1 @ Add2)),
rule ((term Subterm) ==> [term Sub1,subt,term Sub2]) (Subterm = (sub @ Sub1 @ Sub2)),
rule ((term Multerm) ==> [term Mul1,mult,term Mul2]) (Multerm = (mul @ Mul1 @ Mul2)),
rule ((term Divterm) ==> [term Div1,divt,term Div2]) (Divterm = (divf @ Div1 @ Div2)),
rule ((term Modterm) ==> [term Mod1,modt,term Mod2]) (Modterm = (modf @ Mod1 @ Mod2)),
rule ((term Ltterm)  ==> [term Ltt1,ltt, term Ltt2]) (Ltterm = (lt @ Ltt1 @ Ltt2)),
rule ((term Lteterm) ==> [term Lte1,ltet,term Lte2]) (Lteterm = (leq @ Lte1 @ Lte2)),
rule ((term Gtterm)  ==> [term Gtt1,gtt, term Gtt2]) (Gtterm = (gt @ Gtt1 @ Gtt2)),
rule ((term Gteterm) ==> [term Gte1,gtet,term Gte2]) (Gteterm = (geq @ Gte1 @ Gte2)),
rule ((term Eqterm)  ==> [term Eq1,eqlt,term Eq2])   (Eqterm = (eql @ Eq1 @ Eq2)),
rule ((term Isterm)  ==> [term Is1,ist,term Is2])    (Isterm = (iss @ Is1 @ Is2)),
rule ((term Concat) ==>  [term Conc1,sconcatt,term Conc2]) (Concat = (sconcat @ Conc1 @ Conc2)),

rule ((term Halt) ==> [haltt]) (Halt = haltterm),
rule ((term Stop) ==> [stopt]) (Stop = stopterm),
rule ((term Int2real) ==> [inttorealt]) (Int2real = inttorealterm),
rule ((term Abs) ==> [abst]) (Abs = absterm),
rule ((term Sqrt) ==> [sqrtt]) (Sqrt = sqrtterm),
rule ((term Sin) ==> [sint]) (Sin = sinterm),
rule ((term Cos) ==> [cost]) (Cos = costerm),
rule ((term Arc) ==> [arctant]) (Arc = arctanterm),
rule ((term Lnt) ==> [lnt]) (Lnt = lnterm),
rule ((term Floor) ==> [floort]) (Floor = floorterm),
rule ((term Ceil) ==> [ceilt]) (Ceil = ceilterm),
rule ((term Trunc) ==> [truncatet]) (Trunc = truncterm),
rule ((term Rabs) ==> [rabst]) (Rabs = rabsterm),
rule ((term Size) ==> [sizet]) (Size = sizeterm),
rule ((term Str2Int) ==> [stringtointt]) (Str2Int = stringtointterm),
rule ((term Substr) ==> [substringt]) (Substr = substringterm),
rule ((term Int2Str) ==> [inttostringt]) (Int2Str = inttostringterm),
rule ((term Real2Str) ==> [realtostringt]) (Real2Str = realtostringterm),
rule ((term Stdin) ==> [stdint]) (Stdin = stdinterm),
rule ((term Stdout) ==> [stdoutt]) (Stdout = stdoutterm),
rule ((term Stderr) ==> [stderrt]) (Stderr = stderrterm),
rule ((term Char) ==> [chrt]) (Char = chrterm),

rule ((term Openin) ==> [openint]) (Openin = openinterm),
rule ((term Openout) ==> [openoutt]) (Openout = openoutterm),
rule ((term Openapp) ==> [openappendt]) (Openapp = openappendterm),
rule ((term Openstr) ==> [openstringt]) (Openstr = openstringterm),
rule ((term Closein) ==> [closeint]) (Closein = closeinterm),
rule ((term Closeout) ==> [closeoutt]) (Closeout = closeoutterm),
rule ((term Term2Str) ==> [termtostringt]) (Term2Str = termtostringterm),
rule ((term Str2Term) ==> [stringtotermt]) (Str2Term = stringtotermterm),
rule ((term Input) ==> [inputt]) (Input = inputterm),
rule ((term Output) ==> [outputt]) (Output = outputterm),
rule ((term Inline) ==> [inputlinet]) (Inline = inputlineterm),
rule ((term LookA) ==> [lookaheadt]) (LookA = lookaheadterm),
rule ((term EOF) ==> [eoft]) (EOF = eofterm),
rule ((term Flush) ==> [flusht]) (Flush = flushterm),
rule ((term Print) ==> [printt]) (Print = print_term),
rule ((term Read) ==> [readt]) (Read = read_term),
rule ((term Printterm) ==> [printtermt]) (Printterm = printtermterm),
rule ((term Readterm) ==> [readtermt]) (Readterm = readtermterm),



rule ((atype Type1) ==> [atype CTypea1,arrowt,atype AType1]) (Type1 = (CTypea1 arr AType1)),
rule ((atype Type3) ==> [formt]) (Type3 = basetype),
rule ((atype Type4) ==> [listt]) (Type4 = listof),
rule ((atype Type5) ==> [atype Ctypec1,atype PrTypec2]) (Type5 = tapp Ctypec1 PrTypec2),
rule ((atype Type6) ==> [(id Idprt1)]) (Type6 = userty Idprt1),
rule ((atype Type7) ==> [lparen,atype ATypepr1,rparen]) (Type7 = ATypepr1),
rule ((atype Type8) ==> [intt]) (Type8 = inttype),
rule ((atype Type9) ==> [stringt]) (Type9 = stringtype),
rule ((atype Type10) ==> [realt]) (Type10 = realtype),
rule ((atype Type11) ==> [instreamt]) (Type11 = intype),
rule ((atype Type12) ==> [outstreamt]) (Type12 = outtype),


rule ((tcomma) ==> [comma]) (true),
rule ((termlist Termlist1) ==> [term Termtl1]) (Termlist1 = [Termtl1]),
rule ((termlist Termlist2) ==> [term Termtl2,tcomma,termlist Termlisttl1]) (Termlist2 = [Termtl2|Termlisttl1]),

rule ((piid Piid1) ==> [pit]) (Piid1 = notype),
rule ((piid Piid2) ==> [pit,colon,atype Typepi1]) (Piid2 = Typepi1),
rule ((piid Piid3) ==> [telparen,piid Piidpi1,rparen]) (Piid3 = Piidpi1),

rule ((sigmaid Sigmaid1) ==> [sigmat]) (Sigmaid1 = notype),
rule ((sigmaid Sigmaid2) ==> [sigmat,colon,atype Typesi1]) (Sigmaid2 = Typesi1),
rule ((sigmaid Sigmaid3) ==> [telparen,sigmaid Sigmaidsi1,rparen]) (Sigmaid3 = Sigmaidsi1),

rule ((typedid Typedid2) ==> [id Idti2,colon,atype Typeti1]) (Typedid2 = Idti2),
rule ((typedid Typedid3) ==> [telparen,typedid Typeti2,rparen]) (Typedid3 = Typeti2),

rule ((idlist IDL1) ==> [id IDLa])	(IDL1 = [IDLa]),
rule ((idlist IDL2) ==> [nontermst Actastr,id IDL3]) (IDL2 = [IDL3|Actastr]),
rule ((nontermst Nonterm) ==> [idlist Idlnt,comma]) (Nonterm = Idlnt)


].


% Type operators
binaryop arrowt (atype A) (atype B) "right" 100. 
implicitop (atype A) (atype B) "left" 110.
unaryop colon (atype A) 120.

% Clause operators
binaryop comma (term A) (term B) "left" 110.
binaryop andt (term A) (term B) "right" 120.
binaryop semicolon (term A) (term B) "left" 100.
binaryop rimpt (term A) (term B) "left" 170.
binaryop impt (term A) (term B) "right" 130.
implicitop (term A) (term B) "left" 20.
unaryop slasht (term A) 180.
unaryop (piid A) (term B) 180.
unaryop (sigmaid A) (term B) 180.
unaryop (termlist A) (term B) 20.
binaryop concatt (term A) (termlist B) "left" 20.


% Arithmetic
binaryop addt (term A) (term B) "left" 50.
binaryop subt (term A) (term B) "left" 50.
binaryop mult (term A) (term B) "left" 60.
binaryop divt (term A) (term B) "left" 60.
binaryop modt (term A) (term B) "left" 60.
binaryop eqlt (term A) (term B) "left" 30.
binaryop ist  (term A) (term B) "left" 30.
binaryop ltt  (term A) (term B) "left" 30.
binaryop gtt  (term A) (term B) "left" 30.
binaryop ltet (term A) (term B) "left" 30.
binaryop gtet (term A) (term B) "left" 30.
binaryop sconcatt (term A) (term B) "left" 50.

formlam S B (lamabs S L) :- pi x\ ((copy (freevar S) x :- !) => copy B (L x)).

% Copy Clause
copy X X :- const X.
copy (A @ B) (C @ D) :- copy A C, copy B D.
copy (lamabs Ps L) (lamabs Ps M) :- pi x\ (copy x x => (copy (L x) (M x))).
copy (freevar S) (freevar S).
copy (clause_coerce A) (clause_coerce A).

%%% Command to parse a signature

parsesignature Name Signame Parsedaccums Defs  :- Signaturename is Name ^ ".sig", 
			    	 		  parsefile Signaturename (sigormod (definesig Signame Accums Defs)), !,
						  accumulatesignatures Accums Parsedaccums.

% Signature accumulation
accumulatesignatures nil nil.
accumulatesignatures ((saccummod A) :: B) Parsed :- accumulateactualsigs A Aparsed, accumulatesignatures B Bparsed, append Aparsed Bparsed Parsed.

accumulateactualsigs nil nil.
accumulateactualsigs (A :: B) (Aparsed :: Bparsed) :- parsesignature A Signame MoreAccums Moredefs, accumulateactualsigs B Bparsed,
						      Aparsed = parsedsig Signame MoreAccums Moredefs.



%%% Command to parse a module

parsemodule Name Modname Parsedincs Clauses :- Modulename is Name ^ ".mod", 
					       parsefile Modulename (sigormod (definemod Modname Incs Clauses)), !,
					       includefiles Incs Parsedincs.

includefiles nil nil.
includefiles ((importmod A) :: B) Parsed :- accumulateactualsigs A Aparsed, includefiles B Bparsed, append Aparsed Bparsed Parsed.
includefiles ((accummod A) :: B) Parsed :- accumulateactualmodules A Aparsed, includefiles B Bparsed, append Aparsed Bparsed Parsed.
includefiles ((saccummod A) :: B) Parsed :- accumulateactualmodules A Aparsed, includefiles B Bparsed, append Aparsed Bparsed Parsed.

accumulateactualmodules nil nil.
accumulateactualmodules (A :: B) (Aparsed :: Bparsed) :- parsemodule A Modname Moreincs Moreclauses, accumulateactualmodules B Bparsed,
							 Aparsed = parsedmod Modname Moreincs Moreclauses.


%% Fix placement of infix declarations -- parsed version's function would be the argument, not the function.
gatherinfixes nil nil.
gatherinfixes ((fixdec infl Term Level) :: B) Termlist :- gatherinfixes B Terms, append Term Terms Termlist, !.
gatherinfixes ((fixdec infr Term Level) :: B) Termlist :- gatherinfixes B Terms, append Term Terms Termlist, !.
gatherinfixes ((fixdec inf  Term Level) :: B) Termlist :- gatherinfixes B Terms, append Term Terms Termlist, !.
gatherinfixes (A :: B) Terms :- gatherinfixes B Terms.

accumedinfixes nil nil.
accumedinfixes ((parsedsig Name Moreaccums Defs) :: B) Fullterms :- gatherinfixes Defs Terms, accumedinfixes Moreaccums Moreterms, 
							            accumedinfixes B Bterms, append Terms Moreterms Tempterms,
								    append Tempterms Bterms Fullterms.

fixfixity nil A A :- !.
fixfixity Fixlist nil nil :- !.
fixfixity Fixlist (modcls (A @ (freevar Infix) @ C) :: B) (modcls ((freevar Infix) @ A1 @ C1) :: Fixedb) :- member Infix Fixlist,
													      fixfixterm Fixlist A A1,
													      fixfixterm Fixlist C C1,
													      fixfixity Fixlist B Fixedb, !.
fixfixity Fixlist (modcls (A @ Infix @ C) :: B) (modcls (A1 @ Infix1 @ C1) :: Fixedb) :- fixfixterm Fixlist A A1,
										             fixfixterm Fixlist C C1,
										             fixfixterm Fixlist Infix Infix1,
											     fixfixity Fixlist B Fixedb,!.
fixfixity Fixlist ((fixdec infl Term Level) :: B) ((fixdec infl Term Level) :: Bfixed) :- append Term Fixlist FX2, fixfixity FX2 B Bfixed, !.
fixfixity Fixlist ((fixdec infr Term Level) :: B) ((fixdec infl Term Level) :: Bfixed) :- append Term Fixlist FX2, fixfixity FX2 B Bfixed, !.
fixfixity Fixlist ((fixdec inf Term Level) :: B) ((fixdec infl Term Level) :: Bfixed) :- append Term Fixlist FX2,  fixfixity FX2 B Bfixed, !.

fixfixity Fixlist (A :: B) (A :: Bfixed) :- fixfixity Fixlist B Bfixed.

fixfixterm Fixlist (A @ (freevar Infix) @ B) ((freevar Infix) @ A1 @ B1) :- member Infix Fixlist, fixfixterm Fixlist A A1, fixfixterm Fixlist B B1, !.
fixfixterm Fixlist (A @ Infix @ B) (A1 @ Infix1 @ B1) :- fixfixterm Fixlist A A1, fixfixterm Fixlist B B1, fixfixterm Fixlist Infix Infix1, !.
fixfixterm Fixlist A A.


%%% Main predicate for parsing Lambda Prolog files.
% It will return as a new object "parsedlp" the name,
% accumulated signatures/modules, the type and kind
% statements, and the clauses in the module.
%%%

parselp Name Parsed :- parsesignature Name Signame2 Accums Defs,
		       parsemodule Name Modname2 Incs Clauses, gatherinfixes Defs Fixlist, accumedinfixes Accums Fixlist2,
		       append Fixlist Fixlist2 FIXIES, fixfixity FIXIES Clauses NewClauses, append Accums Incs Allincs,
		       Parsed = parsedlp Name Allincs Defs NewClauses.


const X :- member X [neg,forall A,thereexists B,imp,rimp,and,or,truth,lies,failure,add,sub,mul,divf,modf,lt,leq,gt,geq,eql,cut,nillist,listwith C,genterm,iss,
		     haltterm, stopterm, inttorealterm, absterm, sqrtterm, sinterm, costerm, stdinterm, stdoutterm, stderrterm,
		     arctanterm, lnterm, floorterm, ceilterm, truncterm, rabsterm, sizeterm,stringtointterm, substringterm, inttostringterm, realtostringterm,
		     openinterm, openoutterm, openappendterm, openstringterm, closeinterm, closeoutterm,
		     termtostringterm, stringtotermterm, inputterm, outputterm, inputlineterm, lookaheadterm,
		     eofterm, flushterm, print_term, read_term, printtermterm, readtermterm,stringterm D, intterm E].