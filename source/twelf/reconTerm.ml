  let delayedList : (unit -> unit) list ref = ref []

  let clearDelayed () = (delayedList := [])
  let addDelayed f = (delayedList := f::(!delayedList))
  let runDelayed () =
        let rec run' l =
          match l with
              [] -> ()
            | (h::t) -> (run' t; h ())
        in
        run' (!delayedList)

  exception Error of string

  let errorCount = ref 0
  let errorFileName = ref "no file"

  let errorThreshold = ref (Some (20))
  let exceeds args =
    match args with
      (i, None) -> false
    | (i, Some(j)) -> i > j

  let resetErrors (fileName) =
      (errorCount := 0;
       errorFileName := fileName)

  let die (r) =
        raise (Error (Paths.wrap (r,
                      " " ^ (string_of_int (!errorCount))
                      ^ " error" ^ (if !errorCount > 1 then "s" else "")
                      ^ " found")))
      
  let checkErrors (r) =
       if !errorCount > 0 then die (r) else ()

  (* Since this structure uses a non-standard error reporting mechanism,
     any errors reported here while chatter = 1 will be printed
     in between the "[Loading file ..." message and the closing "]",
     instead of after the closing "]".  If we don't emit a newline
     when chatter = 1, the first such error will appear on the same line
     as "[Loading file ...", terribly confusing the Emacs error parsing code.
   *)
  let chatterOneNewline () =
      if !Global.chatter = 1 && !errorCount = 1
        then Msg.message "\n"
      else ()

  let fatalError (r, msg) =
      (errorCount := !errorCount + 1;
       chatterOneNewline ();
       Msg.message (!errorFileName ^ ":" ^ Paths.wrap (r, msg) ^ "\n");
       die (r))       
      
  let error (r, msg) =
      (errorCount := !errorCount + 1;
       chatterOneNewline ();
       Msg.message (!errorFileName ^ ":" ^ Paths.wrap (r, msg) ^ "\n");
       if exceeds (!errorCount, !errorThreshold)
          then die (r)
       else ())


  open ExtSyn

  type job =
      Jnothing
    | Jand of job * job
    | Jwithctx of dec IntSyn.ctx * job
    | Jterm of term
    | Jclass of term
    | Jof of term * term
    | Jof' of term * IntSyn.exp

  let jnothing = Jnothing
  let jand (j1,j2) = Jand(j1,j2)
  let jwithctx (ctx, j) = Jwithctx (ctx, j)
  let jterm t = Jterm(t)
  let jclass t = Jclass(t)
  let jof (t1,t2) = Jof(t1,t2)
  let jof' (t, e) = Jof'(t, e)

  let termRegion arg =
    match arg with
      (Internal (u, v, r)) -> r 
    | (Constant (h, r)) -> r
    | (Bvar (k, r)) -> r
    | (Evar (name, r)) -> r
    | (Fvar (name, r)) -> r
    | (Typ (r)) -> r
    | (Arrow (tm1, tm2)) ->
        Paths.join (termRegion tm1, termRegion tm2)
    | (Pi (tm1, tm2)) ->
        Paths.join (decRegion tm1, termRegion tm2)
    | (Lam (tm1, tm2)) ->
        Paths.join (decRegion tm1, termRegion tm2)
    | (App (tm1, tm2)) ->
        Paths.join (termRegion tm1, termRegion tm2)
    | (Hastype (tm1, tm2)) ->
        Paths.join (termRegion tm1, termRegion tm2)
    | (Mismatch (tm1, tm2, _, _)) ->
        termRegion tm2
    | (Omitted (r)) -> r
    | (Lcid (_, _, r)) -> r
    | (Ucid (_, _, r)) -> r
    | (Quid (_, _, r)) -> r
    | (Scon (_, r)) -> r
    | (Omitapx (u, v, l, r)) -> r
    | (Omitexact (u, v, r)) -> r

  and decRegion (Dec (name, tm, r)) = r

  let ctxRegion arg =
    match arg with
        (IntSyn.Null) -> None
      | (IntSyn.Decl (g, tm)) ->
          ctxRegion' (g, decRegion tm)

  and ctxRegion' args =
    match args with
        (IntSyn.Null, r) -> Some r
      | (IntSyn.Decl (g, tm), r) ->
          ctxRegion' (g, Paths.join (r, decRegion tm))

    open Approx
    type 'a ctx = 'a IntSyn.ctx
    type dec = Dec of string option * exp | NDec of string option
  


    let filterLevel (tm, l, max, msg) =
      let notGround = makeGroundUni l in
      let Level i = whnfUni l in
      if i > max
      then fatalError (termRegion tm,
                       "Level too high\n" ^ msg) 
      else if notGround
      then error (termRegion tm,
                  "Ambiguous level\n" ^
                  "The level of this term could not be inferred\n" ^
                  "Defaulting to " ^
                  (match i with
                       1 -> "object"
                     | 2 -> "type family"
                     | 3 -> "kind") ^
                  " level")
      else ()
  
    let findOmitted (g, qid, r) =
          (error (r, "Undeclared identifier "
                     ^ Names.qidToString (valOf (Names.constUndef qid)));
           omitted (r))

    let rec findBVar' args =
      match args with
          (Null, name, k) -> None
      | (Decl (g, Dec (None, _)), name, k) ->
          findBVar' (g, name, k+1)
      | (Decl (g, NDec _), name, k) ->
          findBVar' (g, name, k+1)
      | (Decl (g, Dec (Some(name'), _)), name, k) ->
          if name = name' then Some (k)
          else findBVar' (g, name, k+1)
  
    let findBVar fc (g, qid, r) =
        (match Names.unqualified qid with
             None -> fc (g, qid, r)
          | Some name ->
              (match findBVar' (g, name, 1) with
                    None -> fc (g, qid, r)
                  | Some k -> bvar (k, r)))

    let findConst fc (g, qid, r) =
        (match Names.constLookup qid with
              None -> fc (g, qid, r)
            | Some cid ->
	      (match IntSyn.sgnLookup cid with
		    IntSyn.ConDec _ -> constant (IntSyn.Const cid, r)
	          | IntSyn.ConDef _ -> constant (IntSyn.Def cid, r)
		  | IntSyn.AbbrevDef _ -> constant (IntSyn.NSDef cid, r)
		  | _ -> 
		    (error (r, "Invalid identifier\n"
			    ^ "Identifier `" ^ Names.qidToString qid
			    ^ "' is not a constant, definition or abbreviation");
		     omitted (r))))

    let findCSConst fc (g, qid, r) =
        ( match Names.unqualified qid with
              None -> fc (g, qid, r)
            | Some name ->
              (match CSManager.parse name with
                    None -> fc (g, qid, r)
                  | Some (cs, conDec) ->
                      constant (IntSyn.FgnConst (cs, conDec), r)))

    let findEFVar fc (g, qid, r) =
        (match Names.unqualified qid with
              None -> fc (g, qid, r)
            | Some name -> (if !queryMode then evar else fvar) (name, r))

    let findLCID x = findBVar (findConst (findCSConst findOmitted)) x
    let findUCID x = findBVar (findConst (findCSConst (findEFVar findOmitted))) x
    let findQUID x = findConst (findCSConst findOmitted) x


    let inferApx (g, tm) =
      match (g, tm) with
        (g, Internal (u, v, r)) ->
          let (u', v', l') = exactToApx (u, v) in
          (tm, u', v', l')

      | (g,Lcid (ids, name, r)) ->
          let qid = Names.Qid (ids, name) in
          inferApx (g, findLCID (g, qid, r))
      | (g, Ucid (ids, name, r)) ->
          let qid = Names.Qid (ids, name) in
          inferApx (g, findUCID (g, qid, r))
      | (g, Quid (ids, name, r)) ->
          let qid = Names.Qid (ids, name) in
          inferApx (g, findQUID (g, qid, r))
      | (g, Scon (name, r)) ->
          (match CSManager.parse name with
                None -> (error (r, "Strings unsupported in current signature");
                         inferApx (g, omitted (r)))
              | Some (cs, conDec) ->
                  inferApx (g, constant (IntSyn.FgnConst (cs, conDec), r)))

      | (g, Constant (h, r)) ->
          let cd = headConDec h in
          let (u', v', l') = exactToApx (IntSyn.Root (h, IntSyn.Nil),
                                         IntSyn.conDecType cd) in
          let dropImplicit args =
            (match args with
               (v, 0) -> v
             | (Arrow (_, v), i) -> dropImplicit (v, i-1))
          in
          let v'' = dropImplicit (v', IntSyn.conDecImp cd) in
          (tm, u', v'', l')
      | (g, Bvar (k, r)) ->
          let Dec (_, v) = IntSyn.ctxLookup (g, k) in
          (tm, Undefined, v, Approx.aType)
      | (g, Evar (name, r)) ->
          (tm, Undefined, getEVarTypeApx name, Approx.aType)
      | (g, Fvar (name, r)) ->
          (tm, Undefined, getFVarTypeApx name, Approx.aType)
      | (g, Typ (r)) ->
          (tm, Uni Approx.aType, Uni Approx.aKind, Approx.aHyperkind)
      | (g, Arrow (tm1, tm2)) ->
          let l = newLVar () in
          let (tm1', v1) = checkApx (g, tm1, Uni Approx.aType, Approx.aKind,
                                     "Left-hand side of arrow must be a type") in
          let (tm2', v2) = checkApx (g, tm2, Uni l, Next l,
                                     "Right-hand side of arrow must be a type or a kind") in
          (arrow (tm1', tm2'), Arrow (v1, v2), Uni l, Next l)
      | (g, Pi (tm1, tm2)) ->
          let (tm1', ((Dec (_, v1)) as d)) = inferApxDec (g, tm1) in
          let l = newLVar () in
          let (tm2', v2) = checkApx (Decl (g, d), tm2, Uni l, Next l,
                                     "Body of pi must be a type or a kind") in
          (pi (tm1', tm2'), Arrow (v1, v2), Uni l, Next l)
      | (g, Lam (tm1, tm2)) ->
          let (tm1', ((Dec (_, v1)) as d)) = inferApxDec (g, tm1) in
          let (tm2', u2, v2, l2) = inferApx (Decl (g, d), tm2) in
          (lam (tm1', tm2'), u2, Arrow (v1, v2), l2)
      | (g, App (tm1, tm2)) ->
          let l = newLVar () in
          let va = newCVar () in
          let vr = newCVar () in
          let (tm1', u1) = checkApx (g, tm1, Arrow (va, vr), l,
                                     "Non-function was applied to an argument") in
          (* probably a confusing message if the problem is the level: *)
          let (tm2', _) = checkApx (g, tm2, va, Approx.aType,
                                    "Argument type did not match function domain type") in
          (app (tm1', tm2'), u1, vr, l) 
      | (g, Hastype (tm1, tm2)) ->
          let l = newLVar () in
          let (tm2', v2) = checkApx (g, tm2, Uni l, Next l,
                                     "Right-hand side of ascription must be a type or a kind") in
          let (tm1', u1) = checkApx (g, tm1, v2, l,
                                     "Ascription did not hold") in
          let _ = addDelayed (fun () -> filterLevel (tm, l, 2, "Ascription can only be applied to objects and type families")) in
          (hastype (tm1', tm2'), u1, v2, l)
      | (g, Omitted (r)) ->
          let l = newLVar () in
          let v = newCVar () in
          let u = newCVar () in(* guaranteed not to be used if l is type *)
          (omitapx (u, v, l, r), u, v, l)

    and checkApx (g, tm, v, l, location_msg) =
      try
          let (tm', u', v', l') = inferApx (g, tm) in
          (matchuni (l, l'); matchFun (v, v'); (tm', u'))
      with Unify problem_msg ->
            let r = termRegion tm in
            let (tm'', u'') = checkApx (g, omitted (r), v, l, location_msg) in
            (* just in case *)
            let _ = addDelayed (fun () -> (makegroundUni l'; ())) in
            (mismatch (tm', tm'', location_msg, problem_msg), u'')

    and inferApxDec (g, Dec (name, tm, r)) =
          let (tm', v1) = checkApx (g, tm, Uni Approx.aType, Approx.aKind,
                                    "Classifier in declaration must be a type") in
          let d = Dec (name, v1) in
          (dec (name, tm', r), d)

    let inferApxJob args =
      match args with
        (g, Jnothing) -> jnothing
      | (g, Jand (j1, j2)) ->
          jand (inferApxJob (g, j1), inferApxJob (g, j2))
      | (g, Jwithctx (g, j)) ->
          let ia arg =
            (match arg with
                 (Null) -> (g, Null) 
               | (Decl (g, tm)) ->
                    let (g', g') = ia (g) in
                    let _ = clearDelayed () in
                    let (tm', d) = inferApxDec (g', tm) in
                    let _ = runDelayed () in
                   (Decl (g', d), Decl (g', tm')))
          in
          let (g', g') = ia (g) in
          jwithctx (g', inferApxJob (g', j))
      | (g, Jterm (tm)) ->
          let _ = clearDelayed () in
          let (tm', u, v, l) = inferApx (g, tm) in
          let _ = filterLevel (tm', l, 2,
                               "The term in this position must be an object or a type family") in
          let _ = runDelayed () in
          jterm (tm')
      | (g, Jclass (tm)) ->
          let _ = clearDelayed () in
          let l = newLVar () in
          let (tm', v) = checkApx (g, tm, Uni l, Next l,
                                   "The term in this position must be a type or a kind") in
          let _ = filterLevel (tm', Next l, 3,
                               "The term in this position must be a type or a kind") in
          let _ = runDelayed () in
          jclass (tm')
      | (g, Jof (tm1, tm2)) ->
          let _ = clearDelayed () in
          let l = newLVar () in
          let (tm2', v2) = checkApx (g, tm2, Uni l, Next l,
                                     "The term in this position must be a type or a kind") in
          let (tm1', u1) = checkApx (g, tm1, v2, l,
                                     "Ascription in declaration did not hold") in
          let _ = filterLevel (tm1', l, 2,
                               "The term in this position must be an object or a type family") in
          let _ = runDelayed () in
          jof (tm1', tm2')
      | (g, Jof' (tm1, v)) ->
          let _ = clearDelayed () in
          let l = newLVar () in
	  let (v2, _) = Approx.classToApx v in
          let (tm1', u1) = checkApx (g, tm1, v2, l,
                                     "Ascription in declaration did not hold") in
          let _ = filterLevel (tm1', l, 2,
                               "The term in this position must be an object or a type family") in
          let _ = runDelayed () in
          jof' (tm1', v)

    let rec ctxToApx arg =
      match arg with
        IntSyn.Null -> IntSyn.Null
      | (IntSyn.Decl (g, IntSyn.NDec x)) ->
          IntSyn.Decl (ctxToApx g, NDec x)
      | (IntSyn.Decl (g, IntSyn.Dec (name, v))) -> 
	    let (v', _) = Approx.classToApx v in
	    IntSyn.Decl (ctxToApx g, Dec (name, v'))

    let inferApxJob' (g, t) =
        inferApxJob (ctxToApx g, t)
 
  (* Final reconstruction job syntax *)

  type job' =
      JNothing
    | JAnd of job' * job'
    | JWithCtx of IntSyn.dec IntSyn.ctx * job'
    | JTerm of (IntSyn.exp * Paths.occExp) * IntSyn.exp * IntSyn.uni
    | JClass of (IntSyn.exp * Paths.occExp) * IntSyn.uni
    | JOf of (IntSyn.exp * Paths.occExp) * (IntSyn.exp * Paths.occExp) * IntSyn.uni
  
  (* This little datatype makes it easier to work with eta-expanded terms
     The idea is that Elim E represents a term U if
       E (s, S) = U[s] @ S *)

  type bidi =
      Elim of IntSyn.sub * (IntSyn.spine -> IntSyn.exp)
    | Intro of IntSyn.exp

  let elimSub (E, s) = (fun (s', S) -> E (comp (s, s'), S))
  let elimApp (E, u) = (fun (s, S) -> E (s, App (EClo (u, s), S)))

  let bvarElim (n) = (fun (s, S) ->
      (match bvarSub (n, s) with
            Idx (n') -> Root (BVar n', S)
          | Exp (u) -> Redex (u, S)))
  let fvarElim (name, v, s) =
        (fun (s', S) -> Root (FVar (name, v, comp (s, s')), S))
  let redexElim (u) = (fun (s, S) -> Redex (EClo (u, s), S))
  (* headElim (H) = E
     assumes H not Proj _ *)
  let headElim arg =
    match arg with
        (BVar n) -> bvarElim n
      | (FVar fv) -> fvarElim fv
      | (NSDef d) -> redexElim (constDef d)
      | (h) ->
        (match conDecStatus (headConDec h) with
            Foreign (csid, f) -> (fun (s, S) -> f S)
          | _ -> (fun (s, S) -> Root (h, S)))
  (* although internally EVars are lowered intro forms, externally they're
     raised elim forms.
     this conforms to the external interpretation:
     the type of the returned elim form is ([[G]] V) *)
  let evarElim ((EVar _) as x) =
        (fun (s, S) -> EClo (x, Whnf.spineToSub (S, s)))

  let etaExpandW args =
    match args with
      (E, (Pi (((Dec (_, va) as d), _), vr), s)) ->
        let u1 = etaExpand (bvarElim (1), (va, comp (s, shift))) in
        let d' = decSub (d, s) in
        Lam (d', etaExpand (elimApp (elimSub (E, shift), u1), (vr, dot1 s)))
    | (E, _) -> E (id, Nil)
  and etaExpand (E, vs) = etaExpandW (E, Whnf.whnfExpandDef vs)

  (* preserves redices *)
  let toElim arg =
    match arg with
      (Elim E) -> E
    | (Intro u) -> redexElim u

  let toIntro args =
    match args with
        (Elim E, vs) -> etaExpand (E, vs)
      | (Intro u, vs) -> u

  let addImplicit1W (g, E, (Pi ((Dec (_, va), _), vr), s), i (* >= 1 *)) =
        let x = Whnf.newLoweredEVar (g, (va, s)) in
        addImplicit (g, elimApp (E, x), (vr, Whnf.dotEta (Exp (x), s)), i-1)

      (* if no implicit arguments, do not expand Vs!!! *)
  and addImplicit args =
    match args with
        (g, E, vs, 0) -> (E, EClo vs)
      | (g, E, vs, i) -> addImplicit1W (g, E, Whnf.whnfExpandDef vs, i)

 

    let inferExactN (g, tm) =
      match (g, tm) with
          (g, Internal (u, v, r)) ->
            (tm, Intro u, v) 
      | (g, Constant (h, r)) ->
          let cd = headConDec (h) in
          let (E, v) = addImplicit (g, headElim h, (conDecType cd, id), conDecImp cd) in
          (tm, Elim E, v)
      | (g, Bvar (k, r)) ->
          let Dec (_, v) = ctxDec (g, k) in
          (tm, Elim (bvarElim k), v)
      | (g, Evar (name, r)) ->
          (* externally EVars are raised elim forms *)
            let (x, v) = 
              try
                getEVar (name, false)
              with Approx.Ambiguous ->
                    let (x, v) = getEVar (name, true) in
                    delayAmbiguous (g, v, r, "Free variable has ambiguous type");
                    (x, v)
            in
            let s = Shift (ctxLength (g)) (* necessary? -kw *) in
            (tm, Elim (elimSub (evarElim x, s)), EClo (v, s))
      | (g, Fvar (name, r)) -> 
          let v = 
            try
              getFVarType (name, false)
            with Approx.Ambiguous ->
                    let v = getFVarType (name, true) in
                    delayAmbiguous (g, v, r, "Free variable has ambiguous type");
                    v
          in
          let s = Shift (ctxLength (g)) in(* necessary? -kw *)
          (tm, Elim (fvarElim (name, v, s)), EClo (v, s))
      | (g, Typ (r)) ->
          (tm, Intro (Uni Approx.aType), Uni Approx.aKind)
      | (g, Arrow (tm1, tm2)) ->
          let (tm1', b1, _ (* Uni Type *)) = inferExact (g, tm1) in
          let d = Dec (None, toIntro (b1, (Uni Approx.aType, id))) in
          let (tm2', b2, l) = inferExact (g, tm2) in
          let v2 = toIntro (b2, (l, id)) in
          (arrow (tm1', tm2'), Intro (Pi ((d, No), EClo (v2, shift))), l)
      | (g, Pi (tm1, tm2)) ->
          let (tm1', d) = inferExactDec (g, tm1) in
          let (tm2', b2, l) = inferExact (Decl (g, d), tm2) in
          let v2 = toIntro (b2, (l, id)) in
          (pi (tm1', tm2'), Intro (Pi ((d, Maybe), v2)), l)
      | (g, Lam (tm1, tm2)) ->
          let (tm1', d) = inferExactDec (g, tm1) in
          let (tm2', b2, v2) = inferExact (Decl (g, d), tm2) in
          let u2 = toIntro (b2, (v2, id)) in
          (lam (tm1', tm2'), Intro (Lam (d, u2)), Pi ((d, Maybe), v2))
      | (g, App (tm1, tm2)) ->
          let (tm1', b1, v1) = inferExact (g, tm1) in
          let E1 = toElim (b1) in
          let (Pi ((Dec (_, va), _), vr), s) = Whnf.whnfExpandDef (v1, id) in
          let (tm2', b2) = checkExact (g, tm2, (va, s),
                                       "Argument type did not match function domain type\n(Index object(s) did not match)") in
          let u2 = toIntro (b2, (va, s)) in
          (app (tm1', tm2'), Elim (elimApp (E1, u2)), EClo (vr, Whnf.dotEta (Exp u2, s)))
      | (g, Hastype (tm1, tm2)) ->
          let (tm2', b2, l) = inferExact (g, tm2) in
          let v = toIntro (b2, (l, id)) in
          let (tm1', b1) = checkExact (g, tm1, (v, id),
                                      "Ascription did not hold\n(Index object(s) did not match)") in
          (hastype (tm1', tm2'), b1, v)
      | (g, Mismatch (tm1, tm2, location_msg, problem_msg)) ->
          let (tm1', _, v1) = inferExact (g, tm1) in
          let (tm2', b, v) = inferExactN (g, tm2) in
          let _ = if !trace then reportMismatch (g, (v1, id), (v, id), problem_msg) else () in
          let _ = delayMismatch (g, v1, v, termRegion tm2', location_msg, problem_msg) in
          (mismatch (tm1', tm2', location_msg, problem_msg), b, v)
      | (g, Omitapx (u, v, l, r)) ->
          let v' = 
            try
              Approx.apxToClass (g, v, l, false)
            with Approx.Ambiguous ->
                     let v' = Approx.apxToClass (g, v, l, true) in
                     delayAmbiguous (g, v', r, "Omitted term has ambiguous " ^
                       (match Approx.whnfUni l with
                             Approx.Level 1 -> "type"
                           | Approx.Level 2 -> "kind"
                             (* yes, this can happen in pathological cases, e.g.
                                  a : type. b = a : _ _. *)
                             (* FIX: this violates an invariant in printing *)
                           | Approx.Level 3 -> "hyperkind"));
                       v'
          in
          let u' = 
            try
              Approx.apxToExact (g, u, (v', id), false)
            with Approx.Ambiguous ->
                     let u' = Approx.apxToExact (g, u, (v', id), true) in
                     delayAmbiguous (g, u', r, "Omitted " ^
                       (match Approx.whnfUni l with
                             Approx.Level 2 -> "type"
                           | Approx.Level 3 -> "kind") ^ " is ambiguous");
                     u'
          in
          (omitexact (u', v', r), Intro u', v')

    and inferExact (g, tm) = inferExactN (g, tm)

    and inferExactDec (g, Dec (name, tm, r)) =
          let (tm', b1, _ (* Uni Type *)) = inferExact (g, tm) in
          let v1 = toIntro (b1, (Uni Approx.aType, id)) in
          let d = Dec (name, v1) in
          (dec (name, tm', r), d)

    and checkExact1 args =
      match args with
        (g, Lam (Dec (name, tm1, r), tm2), vhs) ->
          let (Pi ((Dec (_, va), _), vr), s) = Whnf.whnfExpandDef vhs in
          let ((tm1', b1, _ (* Uni Type *)), ok1) = unifyExact (g, tm1, (va, s)) in
          let v1 = toIntro (b1, (Uni Approx.aType, id)) in
          let d = Dec (name, v1) in
          let ((tm2', b2, v2), ok2) =
                if ok1 then checkExact1 (Decl (g, d), tm2, (vr, dot1 s))
                else (inferExact (Decl (g, d), tm2), false)
          in
          let u2 = toIntro (b2, (v2, id)) in
          ((lam (dec (name, tm1', r), tm2'), Intro (Lam (d, u2)), Pi ((d, Maybe), v2)), ok2)
      | (g, Hastype (tm1, tm2), vhs) ->
          let ((tm2', b2, l), ok2) = unifyExact (g, tm2, vhs) in
          let v = toIntro (b2, (l, id)) in
          let (tm1', b1) = checkExact (g, tm1, (v, id),
                                       "Ascription did not hold\n(Index object(s) did not match)") in
          ((hastype (tm1', tm2'), b1, v), ok2)
      | (g, Mismatch (tm1, tm2, location_msg, problem_msg), vhs) ->
          let (tm1', _, v1) = inferExact (g, tm1) in
          let ((tm2', b, v), ok2) = checkExact1 (g, tm2, vhs) in
          let _ = delayMismatch (g, v1, v, termRegion tm2', location_msg, problem_msg) in
          ((mismatch (tm1', tm2', location_msg, problem_msg), b, v), ok2)
      | (g, Omitapx (u, v (* = vhs *), l, r), vhs) ->
          let v' = EClo vhs in
          let u' = 
            try
              Approx.apxToExact (g, u, vhs, false) 
            with Approx.Ambiguous ->
                     let u' = Approx.apxToExact (g, u, vhs, true) in
                     delayAmbiguous (g, u', r, "Omitted " ^
                       (match Approx.whnfUni l with
                             Approx.Level 2 -> "type"
                           | Approx.Level 3 -> "kind") ^ " is ambiguous");
                     u'
          in
          ((omitexact (u', v', r), Intro u', v'), true)
      | (g, tm, vhs) ->
          let (tm', b', v') = inferExact (g, tm) in
          ((tm', b', v'), unifiableIdem (g, vhs, (v', id)))

    and checkExact (g, tm, vs, location_msg) =
        if not (!trace) then
          let ((tm', b', v'), ok) = checkExact1 (g, tm, vs) in
          if ok then (tm', b')
          else
            try
              ((unifyIdem (g, (v', id), vs));
              raise Match (* can't happen *))
            with Unify.Unify problem_msg ->
              let r = termRegion tm in
              let u' = toIntro (b', (v', id)) in
              let (uapx, vapx, lapx) = Approx.exactToApx (u', v') in
              let ((tm'', b'', _ (* vs *)), _ (* true *)) =
                    checkExact1 (g, omitapx (uapx, vapx, lapx, r), vs) in
              let _ = delayMismatch (g, v', EClo vs, r, location_msg, problem_msg) in
              (mismatch (tm', tm'', location_msg, problem_msg), b'')

        else
          let (tm', b', v') = inferExact (g, tm) in
          try
            (reportUnify (g, (v', id), vs); (tm', b'))
          with Unify.Unify problem_msg ->
            let r = termRegion tm in
            let u' = toIntro (b', (v', id)) in
            let (uapx, vapx, lapx) = Approx.exactToApx (u', v') in
            let (tm'', b'') =
                  checkExact (g, omitapx (uapx, vapx, lapx, r), vs, location_msg) in
            let _ = delayMismatch (g, v', EClo vs, r, location_msg, problem_msg) in
            (mismatch (tm', tm'', location_msg, problem_msg), b'')

    and unifyExact args =
      match args with
        (g, Arrow (tm1, tm2), vhs) ->
          let (Pi ((Dec (_, va), _), vr), s) = Whnf.whnfExpandDef vhs in
          let ((tm1', b1, _ (* Uni Type *)), ok1) = unifyExact (g, tm1, (va, s)) in
          let v1 = toIntro (b1, (Uni Approx.aType, id)) in
          let d = Dec (None, v1) in
          let (tm2', b2, l) = inferExact (g, tm2) in
          let v2 = toIntro (b2, (l, id)) in
          ((arrow (tm1', tm2'), Intro (Pi ((d, No), EClo (v2, shift))), l),
           ok1 && unifiableIdem (Decl (g, d), (vr, dot1 s), (v2, shift)))
      | (g, Pi (Dec (name, tm1, r), tm2), vhs) ->
          let (Pi ((Dec (_, va), _), vr), s) = Whnf.whnfExpandDef vhs in
          let ((tm1', b1, _ (* Uni Type *)), ok1) = unifyExact (g, tm1, (va, s)) in
          let v1 = toIntro (b1, (Uni Approx.aType, id)) in
          let d = Dec (name, v1) in
          let ((tm2', b2, l), ok2) =
                if ok1 then unifyExact (Decl (g, d), tm2, (vr, dot1 s))
                else (inferExact (Decl (g, d), tm2), false) in
          let v2 = toIntro (b2, (l, id)) in
          ((pi (dec (name, tm1', r), tm2'), Intro (Pi ((d, Maybe), v2)), l), ok2)
        (* lam impossible *)
      | (g, Hastype (tm1, tm2), vhs) ->
          (* vh : L by invariant *)
          let (tm2', _ (* Uni L *), _ (* Uni (Next L) *)) = inferExact (g, tm2) in
          let ((tm1', b, l), ok1) = unifyExact (g, tm1, vhs) in
          ((hastype (tm1', tm2'), b, l), ok1)
      | (g, Mismatch (tm1, tm2, location_msg, problem_msg), vhs) ->
          let (tm1', _, l1) = inferExact (g, tm1) in
          let ((tm2', b, l), ok2) = unifyExact (g, tm2, vhs) in
          let _ = delayMismatch (g, l1, l, termRegion tm2', location_msg, problem_msg) in
          ((mismatch (tm1', tm2', location_msg, problem_msg), b, l), ok2)
      | (g, Omitapx (v (* = vhs *), l, nL (* Next L *), r), vhs) ->
          (* cannot raise Ambiguous *)
          let l' = Approx.apxToClass (g, l, nL, false) in
          let v' = EClo vhs in
          ((omitexact (v', l', r), Intro v', l'), true)
      | (g, tm, vhs) -> 
          let (tm', b', l') = inferExact (g, tm) in
          let v' = toIntro (b', (l', id)) in
          ((tm', b', l'), unifiableIdem (g, vhs, (v', id)))

    let occElim args =
      match args with
        (Constant (h, r), os, rs, i) ->
          (* should probably treat a constant with Foreign
             attribute as a redex *)
          let r' = List.fold_right Paths.join r rs in
          (Paths.root (r', Paths.leaf r, conDecImp (headConDec h), i, os), r')
      | (Bvar (k, r), os, rs, i) ->
          let r' = List.fold_right Paths.join r rs in
          (Paths.root (r', Paths.leaf r, 0, i, os), r')
      | (Fvar (name, r), os, rs, i) ->
          let r' = List.fold_right Paths.join r rs in
          (Paths.root (r', Paths.leaf r, 0, i, os), r')
      | (App (tm1, tm2), os, rs, i) ->
          let (oc2, r2) = occIntro tm2 in
          occElim (tm1, Paths.app (oc2, os), r2::rs, i+1)
      | (Hastype (tm1, tm2), os, rs, i) -> occElim (tm1, os, rs, i)
      | (tm, os, rs, i) ->
        (* this is some kind of redex or evar-under-substitution
           also catches simple introduction forms like `type' *)
          let r' = List.fold_right Paths.join (termRegion tm) rs in
          (Paths.leaf r', r')

    and occIntro arg =
      match arg with 
        (Arrow (tm1, tm2)) ->
          let (oc1, r1) = occIntro tm1 in
          let (oc2, r2) = occIntro tm2 in
          let r' = Paths.join (r1, r2) in
          (Paths.bind (r', Some oc1, oc2), r')
      | (Pi (Dec (name, tm1, r), tm2)) ->
          let (oc1, r1) = occIntro tm1 in
          let (oc2, r2) = occIntro tm2 in
          let r' = Paths.join (r, r2) in
          (* not quite consistent with older implementation for dec0 *)
          (Paths.bind (r', Some oc1, oc2), r')
      | (Lam (Dec (name, tm1, r), tm2)) ->
          let (oc1, r1) = occIntro tm1 in
          let (oc2, r2) = occIntro tm2 in
          let r' = Paths.join (r, r2) in
          (* not quite consistent with older implementation for dec0 *)
          (Paths.bind (r', Some oc1, oc2), r')
      | (Hastype (tm1, tm2)) -> occIntro tm1
      | (tm) ->
          (* still doesn't work quite right for the location -> occurrence map? *)
          let (oc, r) = occElim (tm, Paths.nils, [], 0) in
          (oc, r)
 
    let inferExactJob args =
      match args with
          (g, jnothing) -> JNothing
      | (g, Jand (j1, j2)) ->
          JAnd (inferExactJob (g, j1), inferExactJob (g, j2))
      | (g, Jwithctx (g, j)) ->
          let ie a =
            (match a with
               (Null) -> (g, Null)
             | (Decl (g, tm)) ->
                 let (g', gresult) = ie (g) in
                 let (_, d) = inferExactDec (g', tm) in
                 (Decl (g', d), Decl (gresult, d)))
          in
          let (g', gresult) = ie (g) in
          JWithCtx (gresult, inferExactJob (g', j))
      | (g, Jterm (tm)) ->
          let (tm', b, v) = inferExact (g, tm) in
          let u = toIntro (b, (v, id)) in
          let (oc, r) = occIntro (tm') in
          let rec iu a =
            (match a with
               (Uni aType) -> aKind
             | (Pi (_, v)) -> iu v
             | (Root _) -> aType
             | (Redex (v, _)) -> iu v
             | (Lam (_, v)) -> iu v
             | (EClo (v, _)) -> iu v )
              (* others impossible *)
          in
          JTerm ((u, oc), v, iu v)
      | (g, Jclass (tm)) ->
          let (tm', b, l) = inferExact (g, tm) in
          let v = toIntro (b, (l, id)) in
          let (oc, r) = occIntro (tm') in
          let (Uni l, _) = Whnf.whnf (l, id) in
          JClass ((v, oc), l)
      | (g, Jof (tm1, tm2)) ->
          let (tm2', b2, l2) = inferExact (g, tm2) in
          let v2 = toIntro (b2, (l2, id)) in
          let (tm1', b1) = checkExact (g, tm1, (v2, id),
                                       "Ascription in declaration did not hold\n"
                                       ^ "(Index object(s) did not match)") in
          let u1 = toIntro (b1, (v2, id)) in
          let (oc2, r2) = occIntro tm2' in
          let (oc1, r1) = occIntro tm1' in
          let (Uni l2, _) = Whnf.whnf (l2, id) in
          JOf ((u1, oc1), (v2, oc2), l2)
      | (g, Jof' (tm1, v2)) ->
(*          let (tm2', b2, l2) = inferExact (g, tm2)
          let v2 = toIntro (b2, (l2, id)) *)
          let (tm1', b1) = checkExact (g, tm1, (v2, id),
                                       "Ascription in declaration did not hold\n"
                                       ^ "(Index object(s) did not match)") in
          let u1 = toIntro (b1, (v2, id)) in
(*          let (oc2, r2) = occIntro tm2' *)
          let (oc1, r1) = occIntro tm1' in
(*          let (Uni l2, _) = Whnf.whnf (l2, id) *)
          JOf ((u1, oc1), (v2, oc1), Approx.aType)

    let recon' (j) =
          (* we leave it to the context to call Names.varReset
             reason: this code allows reconstructing terms containing
             existing EVars, and future developments might use that *)
          (* context must already have called resetErrors *)
          let _ = Approx.varReset () in
          let _ = varReset () in
          let j' = inferApxJob (Null, j) in
          let _ = clearDelayed () in
          let j'' = inferExactJob (Null, j') in
          let _ = runDelayed () in
          (* we leave it to the context to call checkErrors
             reason: the caller may want to do further processing on
             the "best effort" result returned, even if there were
             errors *)
          j''

    let recon (j) = (queryMode := false; recon' j)
    let reconQuery (j) = (queryMode := true; recon' j)
