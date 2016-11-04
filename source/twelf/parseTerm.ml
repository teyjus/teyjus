(* This is taken from the Twelf implementation *)


  (* Operators and atoms for operator precedence parsing *)
  type 'a operator =
        Atom of 'a
      | Infix of (int * Lfabsyn.assoc) * ('a * 'a -> 'a)
      | Prefix of int * ('a -> 'a)
      | Postfix of int * ('a -> 'a)

    (* Predeclared infix operators *)
  let juxOp = Infix ((Lfabsyn.maxPrec+1, Lfabsyn.Left), ExtSyn.app) (* juxtaposition *)
  let arrowOp = Infix ((Lfabsyn.minPrec-1, Lfabsyn.Right), ExtSyn.arrow)
  let backArrowOp = Infix ((Lfabsyn.minPrec-1, Lfabsyn.Left), ExtSyn.backarrow)
  let colonOp = Infix ((Lfabsyn.minPrec-2, Lfabsyn.Left), ExtSyn.hastype)

  let infixOp (infixity, tm) =
          Infix (infixity, (fun (tm1, tm2) -> ExtSyn.app (ExtSyn.app (tm, tm1), tm2)))
  let prefixOp (prec, tm) =
          Prefix (prec, (fun tm1 -> ExtSyn.app (tm, tm1)))
  let postfixOp (prec, tm) =
          Postfix (prec, (fun tm1 -> ExtSyn.app (tm, tm1)))

  let idToTerm args =
    match args with
        (Tparsing.Parsing.Lexer'.Lower, ids, name, r) -> ExtSyn.lcid (ids, name, r)
      | (Tparsing.Parsing.Lexer'.Upper, ids, name, r) -> ExtSyn.ucid (ids, name, r)
      | (Tparsing.Parsing.Lexer'.Quoted, ids, name, r) -> ExtSyn.quid (ids, name, r)

  let isQuoted arg =
    match arg with
        (Tparsing.Parsing.Lexer'.Quoted) -> true
      | _ -> false

  type stack = (ExtSyn.term operator) list
  type opr = ExtSyn.term operator

    (* The next section deals generically with fixity parsing          *)
    (* Because of juxtaposition, it is not clear how to turn this      *)
    (* into a separate module without passing a juxtaposition operator *)
    (* into the shift and resolve functions                            *)

    module P :
      (sig
	val reduce : stack -> stack
        val reduceAll : Paths.region * stack -> ExtSyn.term
        val shiftAtom : ExtSyn.term * stack -> stack
        val shift : Paths.region * opr * stack -> stack
        val resolve : Paths.region * opr * stack -> stack
      end) =
    struct
      (* Stack invariants, refinements of operator list *)
      (*
	 <p>       ::= <pStable> | <pRed>
	 <pStable> ::= <pAtom> | <pOp?>
	 <pAtom>   ::= Atom _ :: <pOp?>
	 <pOp?>    ::= nil | <pOp>
	 <pOp>     ::= Infix _ :: <pAtom> :: <pOp?>
		     | Prefix _ :: <pOp?>
	 <pRed>    ::= Postfix _ :: Atom _ :: <pOp?>
		     | Atom _ :: <pOp>
      *)
      (* val reduce : <pRed> -> <p> *)
      let reduce arg =
        match arg with
            (Atom(tm2)::Infix(_,con)::Atom(tm1)::p') -> Atom(con(tm1,tm2))::p'
	  | (Atom(tm)::Prefix(_,con)::p') -> Atom(con(tm))::p'
	  | (Postfix(_,con)::Atom(tm)::p') -> Atom(con(tm))::p'
	(* no other cases should be possible by stack invariant *)

      (* val reduceRec : <pStable> -> ExtSyn.term *)
      let rec reduceRec arg = 
        match arg with
            [Atom(e)] -> e
	  | (p) -> reduceRec (reduce p)

      (* val reduceAll : <p> -> ExtSyn.term *)
      let reduceAll args =
        match args with
            (r, [Atom(e)]) -> e
          | (r, Infix _::p') -> Tparsing.Parsing.error (r, "Incomplete infix expression")
  	  | (r, Prefix _::p') -> Tparsing.Parsing.error (r, "Incomplete prefix expression")
  	  | (r, []) -> Tparsing.Parsing.error (r, "Empty expression")
	  | (r, p) -> reduceRec (reduce p)

      (* val shiftAtom : term * <pStable> -> <p> *)
      (* does not raise Error exception *)
      let shiftAtom (tm,p) =
        match (tm,p) with
            (tm, (Atom _::p')) ->
	      (* insert juxOp operator and reduce *)
	      (* juxtaposition binds most strongly *)
	      reduce (Atom(tm)::juxOp::p)
	  | (tm, p) -> Atom(tm)::p

      (* val shift : Paths.region * opr * <pStable> -> <p> *)
      let shift args =
        match args with
            (r, ((Atom _) as opr), ((Atom _::p') as p)) ->
	      (* insert juxOp operator and reduce *)
	      (* juxtaposition binds most strongly *)
	      reduce (opr::juxOp::p)
	(* Atom/Infix: shift *)
	(* Atom/Prefix: shift *)
	(* Atom/Postfix cannot arise *)
	(* Atom/Empty: shift *)
	(* Infix/Atom: shift *)
	  | (r, Infix _, Infix _::p') ->
	    Tparsing.Parsing.error (r, "Consective infix operators")
	  | (r, Infix _, Prefix _::p') ->
	    Tparsing.Parsing.error (r, "Infix operator following prefix operator")
	(* Infix/Postfix cannot arise *)
	  | (r, Infix _, []) ->
	    Tparsing.Parsing.error (r, "Leading infix operator")
	  | (r, ((Prefix _) as opr), ((Atom _::p') as p)) ->
	    (* insert juxtaposition operator *)
	    (* will be reduced later *)
	    opr::juxOp::p
	(* Prefix/{Infix,Prefix,Empty}: shift *)
	(* Prefix/Postfix cannot arise *)
	(* Postfix/Atom: shift, reduced immediately *)
	  | (r, Postfix _, Infix _::p') ->
	    Tparsing.Parsing.error (r, "Postfix operator following infix operator")
	  | (r, Postfix _, Prefix _::p') ->
	    Tparsing.Parsing.error (r, "Postfix operator following prefix operator")
	(* Postfix/Postfix cannot arise *)
	  | (r, Postfix _, []) ->
	    Tparsing.Parsing.error (r, "Leading postfix operator")
	  | (r, opr, p) -> opr::p

      (* val resolve : Paths.region * opr * <pStable> -> <p> *)
      (* Decides, based on precedence of opr compared to the top of the
         stack whether to shift the new operator or reduce the stack
      *)
      let rec resolve (r, opr, p) =
        match (r, opr, p) with
            (r, Infix((prec, assoc), _), (Atom(_)::Infix((prec', assoc'), _)::p')) ->
	      (match (prec-prec', assoc, assoc') with
	           (n,_,_) when n > 0 -> shift(r, opr, p)
	         | (n,_,_) when n < 0 -> resolve (r, opr, reduce(p))
	         | (0, Lfabsyn.Left, Lfabsyn.Left) -> resolve (r, opr, reduce(p))
	         | (0, Lfabsyn.Right, Lfabsyn.Right) -> shift(r, opr, p)
	         | _ -> Tparsing.Parsing.error (r, "Ambiguous: infix following infix of identical precedence"))
	  | (r, Infix ((prec, assoc), _), (Atom(_)::Prefix(prec', _)::p')) ->
	      (match prec-prec' with
	           n when n > 0 -> shift(r, opr, p)
	         | n when n < 0 -> resolve (r, opr, reduce(p))
	         | 0 -> Tparsing.Parsing.error (r, "Ambiguous: infix following prefix of identical precedence"))
	(* infix/atom/atom cannot arise *)
	(* infix/atom/postfix cannot arise *)
	(* infix/atom/<empty>: shift *)

	(* always shift prefix *)
	  | (r, Prefix _, p) ->
	    shift(r, opr, p)

	(* always reduce postfix, possibly after prior reduction *)
	  | (r, Postfix(prec, _), (Atom _::Prefix(prec', _)::p')) ->
	      (match prec-prec' with
	           n when n > 0 -> reduce (shift (r, opr, p))
	  	 | n when n < 0 -> resolve (r, opr, reduce (p))
		 | 0 -> Tparsing.Parsing.error (r, "Ambiguous: postfix following prefix of identical precedence"))
	(* always reduce postfix *)
	  | (r, Postfix(prec, _), (Atom _::Infix((prec', _), _)::p')) ->
	      (match prec - prec' with
	           n when n > 0 -> reduce (shift (r, opr, p))
	         | n when n < 0 -> resolve (r, opr, reduce (p))
                 | 0 -> Tparsing.Parsing.error (r, "Ambiguous: postfix following infix of identical precedence"))
	  | (r, Postfix _, [Atom _]) ->
	    reduce (shift (r, opr, p))

	(* default is shift *)
	  | (r, opr, p) ->
	    shift(r, opr, p)

    end  (* structure P *)

  (* parseQualifier' f = (ids, f')
     pre: f begins with Tparsing.Parsing.Lexer'.ID
     Note: precondition for recursive call is enforced by the lexer. *)
  let rec parseQualId' (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (_, id) as t, r), s')) =
      (match Tparsing.Parsing.Lexer'.Stream'.expose s' with
           Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.PATHSEP, _), s'') ->
             let ((ids, (t, r)), f') = parseQualId' (Tparsing.Parsing.Lexer'.Stream'.expose s'') in
             ((id::ids, (t, r)), f')
         | f' -> (([], (t, r)), f'))


  (* val parseExp : (Tparsing.Parsing.Lexer'.token * Tparsing.Parsing.Lexer'.region) Tparsing.Parsing.Lexer'.Stream'.stream * <p>
                      -> ExtSyn.term * (Tparsing.Parsing.Lexer'.token * Tparsing.Parsing.Lexer'.region) Tparsing.Parsing.Lexer'.Stream'.front *)
  let rec parseExp (s, p) = parseExp' (Tparsing.Parsing.Lexer'.Stream'.expose s, p)
  and parseExp' (f,p) =
    match (f,p) with
        (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.ID _, r0), _), p) ->
          let ((ids, (Tparsing.Parsing.Lexer'.ID (idCase, name), r1)), f') = parseQualId' f in
          let r = Paths.join (r0, r1) in
          let tm = idToTerm (idCase, ids, name, r) in
          (* Currently, we cannot override fixity status of identifiers *)
          (* Thus isQuoted always returns false *)
          if isQuoted (idCase)
          then parseExp' (f', P.shiftAtom (tm, p))
          else parseExp' (f', P.shiftAtom (tm, p))
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.UNDERSCORE,r), s), p) ->
          parseExp (s, P.shiftAtom (ExtSyn.omitted r, p))
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.TYPE,r), s), p) ->
	  parseExp (s, P.shiftAtom (ExtSyn.typ r, p))
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.COLON,r), s), p) ->
	  parseExp (s, P.resolve (r, colonOp, p))
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.BACKARROW,r), s), p) ->
	  parseExp (s, P.resolve (r, backArrowOp, p))
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.ARROW,r), s), p) ->
          parseExp (s, P.resolve (r, arrowOp, p))
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.LPAREN,r), s), p) ->
	  decideRParen (r, parseExp (s, []), p)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.RPAREN,r), s), p) ->
	  (P.reduceAll (r, p), f)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.LBRACE,r), s), p) ->
	  decideRBrace (r, parseDec (s), p)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.RBRACE,r), s), p) ->
          (P.reduceAll (r, p), f)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.LBRACKET,r), s), p) ->
          decideRBracket (r, parseDec (s), p)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.RBRACKET,r), s), p) ->
	  (P.reduceAll (r, p), f)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.DOT,r), s), p) ->
	  (P.reduceAll (r, p), f)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.EOF,r), s), p) ->
	  (P.reduceAll (r, p), f)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((t,r), s), p) ->
	  (* possible error recovery: insert DOT *)
	  Tparsing.Parsing.error (r, "Unexpected token " ^ Tparsing.Parsing.Lexer'.toString t
			    ^ " found in expression")

  and parseDec (s) = parseDec' (Tparsing.Parsing.Lexer'.Stream'.expose s)
  and parseDec' args =
    match args with
        (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (Tparsing.Parsing.Lexer'.Quoted,name), r), s')) ->
          (* cannot happen at present *)
	  Tparsing.Parsing.error (r, "Illegal bound quoted identifier " ^ name)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (idCase,name), r), s')) ->
        (* MKS: we have single file, so nothing would ever be in the table to lookup *)
        parseDec1 (Some(name), Tparsing.Parsing.Lexer'.Stream'.expose s')
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.UNDERSCORE, r), s')) ->
          parseDec1 (None, Tparsing.Parsing.Lexer'.Stream'.expose s')
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.EOF, r), s')) ->
	  Tparsing.Parsing.error (r, "Unexpected end of stream in declaration")
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((t, r), s')) ->
	  Tparsing.Parsing.error (r, "Expected variable name, found token " ^ Tparsing.Parsing.Lexer'.toString t)

  and parseDec1 args =
    match args with
        (x, Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.COLON, r), s')) ->
          let (tm, f'') = parseExp (s', []) in
          ((x, Some tm), f'') 
      | (x, (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.RBRACE, _), _) as f)) ->
          ((x, None), f)
      | (x, (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.RBRACKET, _), _) as f)) ->
          ((x, None), f)
      | (x, Tparsing.Parsing.Lexer'.Stream'.Cons ((t,r), s')) ->
	  Tparsing.Parsing.error (r, "Expected optional type declaration, found token "
			    ^ Tparsing.Parsing.Lexer'.toString t)

  and decideRParen args =
    match args with
        (r0, (tm, Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.RPAREN,r), s)), p) ->
          parseExp (s, P.shiftAtom(tm,p))
      | (r0, (tm, Tparsing.Parsing.Lexer'.Stream'.Cons((_, r), s)), p) ->
	  Tparsing.Parsing.error (Paths.join(r0, r), "Unmatched open parenthesis")

  and decideRBrace args =
    match args with
        (r0, ((x, yOpt), Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.RBRACE,r), s)), p) ->
          let dec = (match yOpt with
                         None -> ExtSyn.dec0 (x, Paths.join (r0, r))
                       | Some y -> ExtSyn.dec (x, y, Paths.join (r0, r))) in
	  let (tm, f') = parseExp (s, []) in
	  parseExp' (f', P.shiftAtom (ExtSyn.pi (dec, tm), p))
      | (r0, (_, Tparsing.Parsing.Lexer'.Stream'.Cons ((_, r), s)), p) ->
	  Tparsing.Parsing.error (Paths.join(r0, r), "Unmatched open brace")

  and decideRBracket args =
    match args with
        (r0, ((x, yOpt), Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.RBRACKET,r), s)), p) ->
          let dec = (match yOpt with
                         None -> ExtSyn.dec0 (x, Paths.join (r0, r))
                       | Some y -> ExtSyn.dec (x, y, Paths.join (r0, r))) in
	  let(tm, f') = parseExp (s, []) in
	  parseExp' (f', P.shiftAtom (ExtSyn.lam (dec, tm), p))
      | (r0, (dec, Tparsing.Parsing.Lexer'.Stream'.Cons ((_, r), s)), p) ->
	  Tparsing.Parsing.error (Paths.join(r0, r), "Unmatched open bracket")



  let parseTerm' = (fun f -> parseExp' (f, []))
