sig lpgram.
accum_sig lyaccshares,blists.

% Terminal Items
type dott, slasht gs.
type trutht, falset,failt gs.
type negt, comma, andt, semicolon, rimpt, impt gs.
type pit, sigmat gs.
type lparen, rparen, lbracket, rbracket, vert, colon, concatt, nilt gs.
type lparen1,lparen2,lparen3,lparen4,lparen5 gs.
type formt, intt, stringt, realt, listt gs.
type instreamt,outstreamt gs.
type percent,quotet gs.
type telparen, tcomma gs.
type gentermt gs.

type typet, kindt, arrowt gs.

type modulet, sigt, accumsigt, accumulatet, importt, localt, localkindt, closedt gs.

type exportdeft, useonlyt gs.

type cutt gs.
type dotcom gs.

type addt, subt, mult, divt, modt gs.
type eqlt, ltt, gtt, ltet, gtet,ist gs.
type infixlt, infixrt, infixt, prefixt, prefixrt, postfixt, postfixlt gs.
type sconcatt gs.
type haltt, stopt, inttorealt, abst, sqrtt, sint, cost, arctant, lnt, floort, ceilt gs.
type truncatet, rabst, sizet, chrt, stringtointt, substringt, inttostringt, realtostringt gs.
type stdint, stdoutt, stderrt gs.

type openint, openoutt, openappendt, openstringt, closeint, closeoutt, termtostringt gs.
type stringtotermt, inputt, outputt, inputlinet, lookaheadt, eoft, flusht, printt, readt gs.
type printtermt, readtermt gs.


%%%%%%%%%
type  modclause  (aclause -> gs).
type  akind  (knd -> gs).
type  atype  (typ -> gs).
type  typestr string -> gs.
type  notype  typ.
type  nokind  knd.
type  basekind knd.
type  noclause aclause.

kind filedef type.
kind declaration type.
kind imports type.
kind aclause type.
kind typ type.
kind knd type.
kind fixtype type.

type inf,infl,infr,pref,prefr,postf,postfl fixtype.

type tapp typ -> typ -> typ.
type basetype typ.
type userty string -> typ.
type listof typ.
type realtype,inttype,stringtype,intype,outtype typ.
type arr typ -> typ -> typ.
type karr knd -> knd -> knd.
infix arr,karr 5.

type cut aclause.
type nillist aclause.
type genterm aclause.
type neg aclause.
type add,sub,mul,divf,modf,lt,leq,gt,geq,eql,iss aclause.
type failure,truth,lies,chrterm aclause.
type haltterm, stopterm, inttorealterm, absterm, sqrtterm, sinterm, costerm aclause.
type arctanterm, lnterm, floorterm, ceilterm, truncterm, rabsterm, sizeterm aclause.
type stringtointterm, substringterm, inttostringterm, realtostringterm aclause.
type stdinterm, stdoutterm, stderrterm aclause.

type openinterm, openoutterm, openappendterm, openstringterm, closeinterm, closeoutterm aclause.
type termtostringterm, stringtotermterm, inputterm, outputterm, inputlineterm, lookaheadterm aclause.
type eofterm, flushterm, print_term, read_term, printtermterm, readtermterm aclause.

type nontermst (list string) -> gs.
type idnt string -> gs.

type sigormod filedef -> gs.
type moduledef filedef -> gs.

type signaturedef filedef -> gs.
type moduledef filedef -> gs.

type modheader string -> gs.
type sigheader string -> gs.

type modpreamble (list imports) -> gs.
type modpreamblecl imports -> gs.
type sigpreamble (list imports) -> gs.

type modbody (list declaration) -> gs.
type signdecls (list declaration) -> gs.

type signdecl declaration -> gs.
type modsigndecl declaration -> gs.

type fixity fixtype -> gs.

type clause aclause -> gs.
type term aclause -> gs.
type intterm int -> aclause.
type stringterm string -> aclause.
type termlist (list aclause) -> gs.

type piid typ -> gs.
type sigmaid typ -> gs.
type typedid string -> gs.
type termtid string -> gs.

type freevar string -> aclause.

type definesig string -> (list imports) -> (list declaration) -> filedef.
type definemod string -> (list imports) -> (list declaration) -> filedef.

type importmod (list string) -> imports.
type accummod (list string) -> imports.
type saccummod (list string) -> imports.

type modcls aclause -> declaration.

type kinddec (list string) -> knd -> declaration.
type typedec (list string) -> typ -> declaration.
type fixdec fixtype -> (list string) -> int -> declaration.
type expdec (list string) -> typ -> declaration.
type usedec (list string) -> typ -> declaration.
type localdec (list string) -> typ -> declaration.
type localkinddec (list string) -> knd -> declaration.
type closeddec (list string) -> typ -> declaration.
type sconcat aclause.

type idlist (list string) -> gs.
type @ aclause -> aclause -> aclause.
infixl @ 9.
type rimp,imp,and,or aclause.
type forall typ -> aclause.
type thereexists typ -> aclause.
type formlam string -> aclause -> aclause -> o.
type listwith (list aclause) -> aclause.

type copy aclause -> aclause -> o.
type const aclause -> o.
type lamabs string -> (aclause -> aclause) -> aclause.
type clause_coerce A -> aclause.

% Processing
type parsesignature string -> string -> (list gs) -> (list declaration) -> o.
type accumulatesignatures (list imports) -> (list gs) -> o.
type accumulateactualsigs (list string) -> (list gs) -> o.
type parsedsig string -> (list gs) -> (list declaration) -> gs.

type parsemodule string -> string -> (list gs) -> (list declaration) -> o.
type includefiles (list imports) -> (list gs) -> o.
type accumulateactualmodules (list string) -> (list gs) -> o.
type parsedmod string -> (list gs) -> (list declaration) -> gs.

type gatherinfixes (list declaration) -> (list string) -> o.
type accumedinfixes (list gs) -> (list string) -> o.

type fixfixity (list string) -> (list declaration) -> (list declaration) -> o.

type fixfixterm (list string) -> aclause -> aclause -> o.

type parselp string -> gs -> o.
type parsedlp string -> (list gs) -> (list declaration) -> (list declaration) -> gs.
type lpparser o.
