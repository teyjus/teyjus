module bobcatgram.   % hoas version
accumulate lambdayacc, bobabsyn.

% Tokenizer declarations:

printname _ "(" lparen.
printname _ ")" rparen.
printname _ "{" lbrace.
printname _ "}" rbrace.
printname _ "[" lbrack.
printname _ "]" rbrack.
printname _ "+" plust.
printname _ "-" minust.
printname _ "*" timest.
printname _ "/" dividet.
printname _ ";" semicolon.
printname _ "," comma.
printname _ ":" colon.
printname _ "." dott.
printname _ "=" eqt.
printname _ ":=" assignt.
printname _ "<>" neqt.
printname _ "<" ltt.
printname _ ">" gtt.
printname _ "<=" let.
printname _ ">=" get.
printname _ "nil" nilt.
printname _ "&" andt.  
printname _ "|" ort.
printname _ "array" arrayt.
printname _ "if" ift.
printname _ "then" thent.
printname _ "else" elset.
printname _ "let" lett.
printname _ "in" intok.
printname _ "end" endt.
printname _ "var" vart.
printname _ "for" fort.
printname _ "while" whilet.
printname _ "function" functiont.
printname _ "type" typet.
printname _ "break" breakt.
printname _ "of" oft.
printname _ "do" dot.   % not dott
printname _ "to" tot.
% bobcat extensions:
printname _ "class" classt.
printname _ "private" privatet.
printname _ "public" publict.
printname _ "{:" ldbrace.
printname _ ":}" rdbrace.

% Parser declarations:

terminal X :-
  oncememb X [id A,iconst N, sconst S,comma, colon, semicolon, 
lparen, rparen, lbrack, rbrack,
lbrace, rbrace, dott, plust, minust, timest, dividet, eqt, neqt,
ltt, let, gtt, get, andt, ort, assignt, arrayt, ift, thent, elset,
whilet, fort, tot, dot, lett, intok, endt, oft, breakt, nilt,
functiont, vart, typet, errort,classt,privatet,publict,ldbrace,rdbrace].

non_terminal X :- member X [program S1,te S2,dec S3,decs S4,tydec_gs S5,
ty_gs S6,tfields S7,vdec_gs S8,fdec_gs S9,lvalue T1,args T2,expseq T3,
moreinits T5,index T6,tyfcomma,acomma,fundech T7,teqt,
createrec T4,funcallh T8,tydechd T9,ty_gshd, cdec_gs T10]. 
ntnum 11.   


start_symbol (program S).

% Grammar for bobcat: object oriented variant of tiger

cfg
[
 rule ((program P1) ==> [te TE1]) (P1 = TE1),

 rule ((dec D1) ==> [tydec_gs T1]) (D1 = T1),
 rule ((dec D2) ==> [vdec_gs T2]) (D2 = T2),
 rule ((dec D3) ==> [fdec_gs T3]) (D3 = T3),
 rule ((dec D3b) ==> [cdec_gs T3b]) (D3b = T3b),
 rule ((decs DS1) ==> [dec D4]) (DS1 = [D4]),
 rule ((decs DS2) ==> [dec D5,decs DS3]) (DS2 = [D5|DS3]),
 rule ((vdec_gs VD1) ==> [vart,id Vn1,assignt,te Ve1])
		(VD1 = vardec Vn1 dummytype Ve1),
 rule ((vdec_gs VD2) ==> [vart,id Vn2,colon,id Vt2,assignt,te Ve2])
		(VD1 = vardec Vn2 (namety Vt2) Ve2),
 rule ((tydec_gs TD1) ==> [tydechd Tn1,ty_gs Ty1])
		(TD1 = typedec Tn1 Ty1),
 rule ((tydechd THD1) ==> [typet,id THDa1,eqt]) (THD1 = THDa1),
 rule ((cdec_gs CDEC1) ==> [classt,id CN1,ldbrace,privatet,tfields Prl,
        publict,decs Pul,rdbrace])
  		(CDEC1 = (classdec CN1 Prl Pul)),
 rule ((fdec_gs FD1) ==> [fundech Fn1,tfields TFL1,rparen,eqt,te FB1])
		(formfix Fn1 TFL1 dummytype FB1 FD1),
 rule ((fdec_gs FD2) ==> [fundech Fn2,tfields TFL2,rparen,colon,id RT2,eqt,te FB2])
		(formfix Fn2 TFL2 (namety RT2) FB2 FD2),
 rule ((fdec_gs FDb1) ==> [fundech Fnb1,rparen,eqt,te FBb1])
		(formfix Fnb1 [] dummytype FBb1 FDb1),
 rule ((fdec_gs FDb2) ==> [fundech Fnb2,rparen,colon,id RTb2,eqt,te FBb2])
		(formfix Fnb2 [] (namety RTb2) FBb2 FDb2),
 rule ((fundech Fn3) ==> [functiont,id Funame3,lparen]) (Fn3 = Funame3),

 rule ((ty_gs TY1) ==> [id Tname1]) (TY1 = (namety Tname1)),
 rule ((ty_gs TY2) ==> [lbrace,tfields TFL3,rbrace]) (TY2 = (recordty TFL3)),
 rule ((ty_gs TYb2) ==> [lbrace,rbrace]) (TYb2 = (recordty [])),
 rule ((ty_gs TY3) ==> [ty_gshd,id Tname2]) (TY3 = (arrayty Tname2)),
 rule (ty_gshd ==> [arrayt,oft]) true,

 rule ((tfields TFL4) ==> [id Vn4,colon,id Tn4]) 
		(TFL4 = [etpair (varexp (simplevar Vn4)) (namety Tn4)]),
 rule ((tfields TFL5) ==> [id Vn5,colon,id Tn5,tyfcomma,tfields TFL6]) 
	(TFL5 = [(etpair (varexp (simplevar Vn5)) (namety Tn5))|TFL6]),

 rule ((lvalue LV1) ==> [id Vname1]) (LV1 = (simplevar Vname1)),
 rule ((lvalue LV2) ==> [lvalue LV3,dott,id Vname2])
		(LV2 = (fieldvar LV3 Vname2)),
 rule ((lvalue LV4) ==> [lvalue LV5,lbrack,te Indexp1,rbrack])
		(LV4 = (subscriptvar LV5 Indexp1)),

 rule ((te E1) ==> [nilt]) (E1 = nilexp),
 rule ((te E2) ==> [lvalue LV6]) (E2 = (varexp LV6)),
 rule ((te E3) ==> [iconst IC3]) (E3 = (intexp IC3)),
 rule ((te E4) ==> [sconst SC4]) (E4 = (stringexp SC4)),
 rule ((te E5) ==> [funcallh Funame5,args AL5,rparen]) 
		(E5 = (callexp Funame5 AL5)),
 rule ((te Ey5) ==> [funcallh Funamey5,rparen]) 
		(Ey5 = (callexp Funamey5 [])),
 rule ((funcallh Funame6) ==> [lvalue Funame7,lparen]) (Funame6 = Funame7),

 rule ((te E6) ==> [minust, te Ea6]) (E6 = (opexp "-" (intexp 0) Ea6)),
 rule ((te E7) ==> [te Ea7,timest,te Eb7]) (E7 = (opexp "*" Ea7 Eb7)),
 rule ((te E8) ==> [te Ea8,dividet,te Eb8]) (E8 = (opexp "div" Ea8 Eb8)),
 rule ((te E9) ==> [te Ea9,minust,te Eb9]) (E9 = (opexp "-" Ea9 Eb9)),
 rule ((te E10) ==> [te Ea10,plust,te Eb10]) (E10 = (opexp "+" Ea10 Eb10)),
 rule ((te E11) ==> [te Ea11,ort,te Eb11]) (E11 = (opexp "or" Ea11 Eb11)),
 rule ((te E12) ==> [te Ea12,andt,te Eb12]) (E12 = (opexp "and" Ea12 Eb12)),
 rule ((te E13) ==> [te Ea13,eqt,te Eb13]) (E13 = (opexp "=" Ea13 Eb13)),
 rule ((te E14) ==> [te Ea14,ltt,te Eb14]) (E14 = (opexp "<" Ea14 Eb14)),
 rule ((te E15) ==> [te Ea15,gtt,te Eb15]) (E15 = (opexp ">" Ea15 Eb15)),
 rule ((te E16) ==> [te Ea16,let,te Eb16]) (E16 = (opexp "<=" Ea16 Eb16)),
 rule ((te E17) ==> [te Ea17,get,te Eb17]) (E17 = (opexp ">=" Ea17 Eb17)),
 rule ((te E18) ==> [te Ea18,neqt,te Eb18]) (E18 = (opexp "<>" Ea18 Eb18)),

 rule ((te E19) ==> [lvalue Ea19,assignt,te Eb19])
		(E19 = (assignexp Ea19 Eb19)),
 rule ((te E20) ==> [lvalue Ea20,lbrack,te Eb20,rbrack,oft,te Ec20])
		(mkarrayexp Ea20 Eb20 Ec20 E20),
 rule ((te E21) ==> [whilet,te Ea21,dot,te Eb21]) 
		(E21 = (whileexp Ea21 Eb21)),
 rule ((te E22) ==> [fort,id Ea22,assignt,te Eb22,tot,te Ec22,dot,te Ed22])
	(E22 = (forexp (vardec Ea22 dummytype Eb22) Ec22 Ed22)),
 rule ((te E23) ==> [breakt]) (E23 = breakexp),
 rule ((te E24) ==> [lett,decs Ea24,intok,expseq Eb24,endt])
	(formlet Ea24 (seqexp Eb24) E24),
 rule ((te E25) ==> [ift,te Ea25,thent,te Eb25,elset,te Ec25])
	(E25 = (ifexp Ea25 Eb25 Ec25)),
 rule ((te E26) ==> [ift,te Ea26,thent,te Eb26])
	(E26 = (ifexp Ea26 Eb26 dummyexp)),
 rule ((te E27) ==> [lparen,expseq Ea27,rparen]) (E27 = (seqexp Ea27)),
 rule ((te E28) ==> [createrec Ea28,moreinits Eb28,rbrace]) 
		(E28 = (recordexp Ea28 Eb28)),
 rule ((createrec Ec28) ==> [id Ed28,lbrace]) (Ec28 = Ed28),
 rule ((moreinits MI2) ==> [id Ma2,eqt,te Me2])
		(MI2 = [eepair (varexp (simplevar Ma2)) Me2]),
 rule ((moreinits MI3) ==> [id Ma3,eqt,te Me3,acomma,moreinits MI4])
		(MI3 = [(eepair (varexp (simplevar Ma3)) Me3)|MI4]),
% rule ((moreinits MI3) ==> [eqtinit Ma3 Me3,acomma,moreinits MI4])
%		(MI3 = [(eepair (varexp (simplevar Ma3)) Me3)|MI4]),
% rule ((eqtinit Ma3a Me3a) ==> [id Ma3b, eqt, te Me3b]) 
%				(Me3a = Me3b, Ma3a = Ma3b),
 rule (acomma ==> [comma]) true,
 rule (tyfcomma ==> [comma]) true,
% rule ((args AG2) ==> [te AGa2]) (AG2 = [AGa2]),
% rule ((args AG3) ==> [te AGa3,comma,args AG4]) (AG3 = [AGa3|AG4]),
 rule ((args AG2) ==> [te AGa2]) (AG2 = [AGa2]),
 rule ((args AG3) ==> [args AG4,comma,te AGa3]) (append AG4 [AGa3] AG3),
 rule ((expseq ESQ1) ==> [te ESa1]) (ESQ1 = [ESa1]),
 rule ((expseq ESQ2) ==> [te ESa2,semicolon,expseq ESb2])
		(ESQ2 = [ESa2|ESb2])
].



% Operator precedence and associativity declarations:
% if p < q then level p has higher precedence than q.
unaryop minust (te X) 1.
binaryop plust (te X) (te Y) "left" 3.
binaryop minust (te X) (te Y) "left" 3.
binaryop timest (te X) (te Y) "left" 2.
binaryop dividet (te X) (te Y) "left" 2.
binaryop andt (te X) (te Y) "left" 9.
binaryop ort (te X) (te Y) "left" 9.
binaryop eqt (te X) (te Y) "left" 6.
binaryop neqt (te X) (te Y) "left" 6.
binaryop ltt (te X) (te Y) "left" 6.
binaryop gtt (te X) (te Y) "left" 6.
binaryop let (te X) (te Y) "left" 6.
binaryop get (te X) (te Y) "left" 6.

% This is needed because in Tiger there's no distinction between
% expressions and statements
binaryop assignt X (te Y) "left" 8.
binaryop oft X (te Y) "left" 11.
%binaryop tot (te X) (te Y) "left" 9.
binaryop dot (te X) (te Y) "left" 10.
binaryop elset (te X) (te Y) "left" 10.
binaryop thent (te X) (te Y) "left" 11.  % must be looser than "elset"

% semantic action clauses:

mkarrayexp (simplevar Name) Eb20 Ec20 (arrayexp Name Eb20 Ec20).

formlet [] Body Body.
formlet [D|Ds] Body (letexp Dabs Nabs)  :- formnabs [D|Ds] [D|Ds] Body Dabs Nabs.

formnabs Dcs [] Body (declist Das) Body2 :- 
        deccopy Dcs Das, copyexp Body Body2.
% below version abstract over too much: can't do  let var a := a+1.
%formnabs Dcs [] Body Das Body2 :- 
%	copydec (declist Dcs) Das, copyexp Body Body2.
formnabs Dcs [(typedec S T)|Ds] Body (decabs Da) (nameterm A) :-
	pi x\ ((copystr S x :- !) => formnabs Dcs Ds Body (Da x) (A x)).
formnabs Dcs [(vardec S T E)|Ds] Body (decabs Da) (nameterm A) :-
	pi x\ ((copystr S x :- !) => formnabs Dcs Ds Body (Da x) (A x)).
formnabs Dcs [(fixptdec S E AT RT)|Ds] Body (decabs Da) (nameterm A) :-
	pi f\ ((copystr S f :- !) => formnabs Dcs Ds Body (Da f) (A f)).
formnabs Dcs [(classdec S Pr Pb)|Ds] Body (decabs Da) (nameterm A) :-
	pi f\ ((copystr S f :- !) => formnabs Dcs Ds Body (Da f) (A f)).

local deccopy (list tdec) -> (list tdec) -> o.
deccopy [] [].
deccopy [(vardec S T E)|R] [(vardec S T2 E)|R2] :- !, copyty T T2,
  deccopy R R2.
deccopy [A|B] [C|D] :- copydec A C, deccopy B D.


% special form needed for no arg functions f():
%	(fixptdec Fname (n\ (absterm (x\ (Body2 n)))) [] Retype) :- !,
formfix Fname [] Retype Body 
	(fixptdec Fname (n\ (Body2 n)) [] Retype) :- !,
  pi f\ ((copystr Fname f :- !) => copyexp Body (Body2 f)).
formfix Fname Args Retype Body (fixptdec Fname A Atypes Retype) :-
  pi f\ ((copystr Fname f :- !) => formfix2 Body (A f) Args Atypes).
formfix2 Body Body2 [] [] :- copyexp Body Body2.
formfix2 Body (nameterm B) [(etpair (varexp (simplevar Tm)) Ty)|As] [Ty|Bs] :-
  pi v\ ((copystr Tm v :- !) => formfix2 Body (B v) As Bs).




% Clauses needed because of implementation shortcut (ignore):

freshcopy (program A) (program B) :- !.
freshcopy (te A) (te B) :- !.
freshcopy (dec A) (dec B) :- !.
freshcopy (decs A) (decs B) :- !.
freshcopy (tydec_gs A) (tydec_gs B) :- !.
freshcopy (ty_gs A) (ty_gs B) :- !.
freshcopy (tfields A) (tfields B) :- !.
freshcopy (vdec_gs A) (vdec_gs B) :- !.
freshcopy (fdec_gs A) (fdec_gs B) :- !.
freshcopy (cdec_gs A) (cdec_gs B) :- !.
freshcopy (lvalue A) (lvalue B) :- !.
freshcopy (args A) (args B) :- !.
freshcopy (expseq A) (expseq B) :- !.
freshcopy (moreinits A) (moreinits B) :- !.
freshcopy (index A) (index B) :- !.
freshcopy (fundech A) (fundech B) :- !.
freshcopy (funcallh A) (funcallh B) :- !.
freshcopy (tydechd A) (tydechd B) :- !.
freshcopy (createrec A) (createrec B) :- !.
%freshcopy (eqtinit A C) (eqtinit B D) :- !.
freshcopy A A.




