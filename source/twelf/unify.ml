(* This is taken from the Twelf implementation *)

  exception Unify of string
  exception NotInvertible
    open IntSyn

    type action =
      Instantiate of exp option ref
    | InstantiateBlock of block option ref
    | Add of cnstrRef list ref
    | Solve of cnstrRef * cnstr

    type cAction = 
      BindCnstr of cnstr ref * cnstr

    type fAction = 
      BindExp of exp option ref * exp option
    | BindBlock of block option ref * block option
    | BindAdd of cnstrRef list ref * cAction list
    | FSolve of cnstr ref * cnstr * cnstr (* ? *)
(*
    type unifTrail = FAction Trail.trail

    let globalTrail = Trail.trail () : action Trail.trail 
 *)
  let rec copyCnstr l  =
    match l with
        [] -> []
      | (refC :: clist) -> 
          (BindCnstr (refC, !refC) :: copyCnstr clist)

  let copy arg =
    match arg with
        (Instantiate refU) -> 
          (BindExp (refU , !refU))
      | (InstantiateBlock refB) -> 
          (BindBlock (refB , !refB))
      | (Add (cnstrs)) ->
          (BindAdd (cnstrs , copyCnstr(!cnstrs)))
      | (Solve (cnstr1, cnstr2)) ->  
          (FSolve (cnstr1, cnstr2, !cnstr1)) 


  let rec resetCnstr l =
    match l with
        [] -> [] 
      | (BindCnstr(refC, cnstr)::l) -> 
          (refC := cnstr;
	   (refC::(resetCnstr l)))


  let reset arg =
    match arg with
        (BindExp (refU, u)) ->
          (refU := u;
	   Instantiate refU)
      | (BindBlock (refB, b)) ->
          (refB := b;
	   InstantiateBlock refB)
      | (BindAdd (cnstrs , cActions)) ->
	  (cnstrs := resetCnstr cActions;
	   Add cnstrs)
      | (FSolve (refCnstr, cnstr, cnstr')) ->
	  (refCnstr := cnstr';
	   Solve (refCnstr, cnstr))
      
(*
    let suspend () = Trail.suspend (globalTrail, copy)

    let resume trail = Trail.resume (trail, globalTrail, reset)
 *)
  let undo arg =
    match arg with
        (Instantiate refU) ->
          (refU := None)
      | (InstantiateBlock refB) ->
	  (refB := None)
      | (Add (cnstrs)) ->
          (match (!cnstrs) with
               (cnstr :: cnstrL) ->
                 (cnstrs := cnstrL))
      | (Solve (cnstr, cnstr')) ->
          (cnstr := cnstr')
(*
    let reset () = Trail.reset globalTrail

    let mark () = Trail.mark globalTrail

    let unwind () = Trail.unwind (globalTrail, undo)
 *)
  let addConstraint (cnstrs, cnstr) =
    (
      cnstrs := cnstr :: (!cnstrs)
 (*   Trail.log (globalTrail, Add (cnstrs)) *)
    )

  let solveConstraint (cnstr) = 
    (
      cnstr := Solved
(*    Trail.log (globalTrail, Solve (cnstr, Cnstr)) *)
     )

    (* Associate a constraint to an expression *)
    (* delayExpW ((U, s), cnstr) = ()

       Invariant: 
       If   G' |- s : G    G |- U : V    (U,s)  in whnf
       then
       the constraint cnstr is added to all the rigid EVar occurrences in U[s]
    *)
  let rec delayExpW args =
    match args with
        ((((Uni l) as u), s1), _) -> ()
      | ((Pi ((d, p), u), s), cnstr) -> 
          (delayDec ((d, s), cnstr); delayExp ((u, dot1 s), cnstr))
      | ((Root (h, ss), s), cnstr) ->
	  (delayHead (h, cnstr); delaySpine ((ss, s), cnstr))
      | ((Lam (d, u), s), cnstr) -> 
          (delayDec ((d, s), cnstr); delayExp ((u, dot1 s), cnstr))
      | ((EVar (g, r, v, cnstrs), s), cnstr) ->
          addConstraint(cnstrs, cnstr)
      | ((FgnExp (i, csfe), s), cnstr) -> (* s = id *)
          FgnExpStd.App.apply (i,csfe) (fun u -> delayExp ((u, s), cnstr))
      (* no other cases by invariant *)

    (* delayExp ((U, s), cnstr) = ()
       as in delayExpCW, except that the argument may not be in whnf 
    *)
  and delayExp (us, cnstr) =
          delayExpW (Whnf.whnf us, cnstr)

    (* delayHead (H, s, rOccur) = ()

       Invariant: 
       If   G' |- H : V    
       and  G' |- s : G         s is a pattern substitution
       then
       the constraint cnstr is added to a total of n rigid EVar occurrences in H[s]
    *)
  and delayHead args =
    match args with
      (FVar (x, v, s'), cnstr) ->
        delayExp ((v, id), cnstr)
    | (h, _) -> ()

    (* delaySpine ((S, s), cnstr) = ()

       Invariant: 
       If   G' |- s : G    G |- S : V > W
       then      G  |- S' : V' > W'
       the constraint cnstr is added to all the rigid EVar occurrences in S[s]
    *)
  and delaySpine args =
    match args with
        ((Nil, s), cnstr) -> ()
      | ((App (u, ss), s), cnstr) ->
          (delayExp ((u, s), cnstr); delaySpine ((ss, s), cnstr))
      | ((SClo(ss, s'), s), cnstr) ->
	  delaySpine ((ss, comp (s', s)), cnstr)

    (* delayDec see delayExp *)
  and delayDec ((Dec (name, v), s), cnstr) =
          delayExp ((v, s), cnstr)

    let awakenCnstrs : cnstrRef list ref = ref [] 
    let resetAwakenCnstrs () = (awakenCnstrs := [])

    let nextCnstr () = 
      match (!awakenCnstrs) with
          [] -> None
        | (cnstr :: cnstrL) -> 
                   (awakenCnstrs := cnstrL; Some(cnstr))

      (* Instantiating EVars  *)
    let instantiateEVar (refU, v, cnstrL) =
      (
        refU := Some(v);
	(*        Trail.log (globalTrail, Instantiate (refU));*)
        awakenCnstrs := cnstrL @ !awakenCnstrs
      )

      (* Instantiating LVars  *)
    let instantiateLVar (refB, b) =
      ( refB := Some(b)
      (*Trail.log (globalTrail, InstantiateBlock (refB))*)
      )
	
    (* intersection (s1, s2) = s'
       s' = s1 /\ s2 (see JICSLP'96)
       
       Invariant: 
       If   G |- s1 : G'    s1 patsub
       and  G |- s2 : G'    s2 patsub
       then G |- s' : G'' for some G''  
       and  s' patsub
    *)
  let rec intersection args =
    match args with
	(Dot (Idx (k1), s1), Dot (Idx (k2), s2)) -> 
 	  if (k1 = k2) then dot1 (intersection (s1, s2))
	  else comp (intersection (s1, s2), shift)
      | ((Dot _) as s1, Shift (n2)) ->
	  intersection (s1, Dot (Idx (n2+1), Shift (n2+1)))
      | (Shift (n1), ((Dot _) as s2)) -> 
	  intersection (Dot (Idx (n1+1), Shift (n1+1)), s2)
      | (Shift _ , Shift _) -> id
        (* both substitutions are the same number of shifts by invariant *)
      (* all other cases impossible for pattern substitutions *)


    (* weakenSub (G1, s, ss) = w'
     
       Invariant:
       If    G |- s : G1       (* s patsub *)
       and   G2 |- ss : G      (* ss strsub *)
       then  G1 |- w' : G1'    (* w' weaksub *)

       and   G2 |- w' o s o ss : G1'  is fully defined
       and   G1' is maximal such
    *)

  let rec weakenSub args =
    match args with
        (g, Shift n, ss) ->
          if n < ctxLength g 
	  then weakenSub (g, Dot (Idx (n+1), Shift (n+1)), ss)
	  else id
      | (g, Dot (Idx n, s'), ss) ->
          (match bvarSub (n, ss) with
 	       Undef -> comp (weakenSub (g, s', ss), shift)
	     | Idx _ -> dot1 (weakenSub (g, s', ss)))
	    (* no other cases, ss is strsub *)
      | (g, Dot (Undef, s'), ss) ->
	   comp (weakenSub (g, s', ss), shift)

    (* invert (G, (U, s), ss, rOccur) = U[s][ss]

       G |- s : G'   G' |- U : V  (G |- U[s] : V[s])
       G'' |- ss : G

       Effect: raises NotInvertible if U[s][ss] does not exist
               or rOccurs occurs in U[s]
               does NOT prune EVars in U[s] according to ss; fails instead
    *)
  let rec invertExp (g, us, ss, rOccur) =
          invertExpW (g, Whnf.whnf us, ss, rOccur)
  and invertExpW args =
    match args with
        (g, (((Uni _) as u), s), _, _) -> u
      | (g, (Pi ((d, p), v), s), ss, rOccur) -> 
          Pi ((invertDec (g, (d, s), ss, rOccur), p),
	      invertExp (Decl (g, decSub (d, s)), (v, dot1 s), dot1 ss, rOccur))
      | (g, (Lam (d, v), s), ss, rOccur) ->
	  Lam (invertDec (g, (d, s), ss, rOccur),
	       invertExp (Decl (g, decSub (d, s)), (v, dot1 s), dot1 ss, rOccur))
      | (g, (Root (h, ss'), s (* = id *)), ss, rOccur) -> 
	  Root (invertHead (g, h, ss, rOccur),
		invertSpine (g, (ss', s), ss, rOccur))
      | (g, (((EVar (r, gx, v, cnstrs)) as x), s), ss, rOccur) -> 
	  if (rOccur == r) then raise NotInvertible
	  else if Whnf.isPatSub (s) then
            let w = weakenSub (g, s, ss) in
            if Whnf.isId w
	    then EClo (x, comp (s, ss))
	    else raise NotInvertible
          else (* s not patsub *)
		 (* invertExp may raise NotInvertible *)
            EClo (x, invertSub (g, s, ss, rOccur))
      | (g, (FgnExp (i,csfe), s), ss, rOccur) ->
          FgnExpStd.Map.apply (i,csfe) (fun u -> invertExp (g, (u, s), ss, rOccur))

      (* other cases impossible since (U,s1) whnf *)
  and invertDec (g, (Dec (name, v), s), ss, rOccur) =
	  Dec (name, invertExp (g, (v, s), ss, rOccur))
  and invertSpine args =
    match args with
        (g, (Nil, s), ss, rOccur) -> Nil
      | (g, (App (u, ss'), s), ss, rOccur) -> 
	  App (invertExp (g, (u, s), ss, rOccur),
	       invertSpine (g, (ss', s), ss, rOccur))
      | (g, (SClo (ss', s'), s), ss, rOccur) -> 
	  invertSpine (g, (ss', comp (s', s)), ss, rOccur)
  and invertHead args =
    match args with(g, BVar k, ss, rOccur) -> 
        (match (bvarSub (k, ss)) with
	     Undef -> raise NotInvertible
	    | Idx k' -> BVar k')
      | (g, (( Const _) as h), ss, rOccur) -> h
      | (g, Proj (((Bidx k) as b), i), ss, rOccur) ->
	(* blockSub (B, ss) should always be defined *)
	(* Fri Dec 28 10:03:12 2001 -fp !!! *)
	  (match blockSub (b, ss) with
	       Bidx(k') -> Proj (Bidx (k'), i))
      | (g, ((Proj (LVar (r, sk, (l, t)), i)) as h), ss, rOccur) -> 
        (* claim: LVar does not need to be pruned since . |- t : Gsome *)
	(* so we perform only the occurs-check here as for FVars *)
	(* Sat Dec  8 13:39:41 2001 -fp !!! *)
	(* this is not true any more, Sun Dec  1 11:28:47 2002 -cs  *)
	(* Changed from Null to G Sat Dec  7 21:58:00 2002 -fp *)
	   ( invertSub (g, t, id, rOccur) ;
	     h )
      | (g, ((Skonst _) as h), ss, rOccur) -> h
      | (g, ((Def _) as h), ss, rOccur) -> h
      | (g, FVar (x, v, s'), ss, rOccur) ->
	  (* V does not to be pruned, since . |- V : type and s' = ^k *)
	  (* perform occurs-check for r only *)
	  (invertExp (g, (v, id), id, rOccur);  (* why G here? -fp !!! *)
	   FVar (x, v, comp (s', ss)))
      | (g, ((FgnConst _) as h), ss, rOccur) -> h
    (* pruneSub never allows pruning OUTDATED *)
    (* in the presence of block variables, this invariant 
       doesn't hold any more, because substitutions do not
       only occur in EVars any more but also in LVars!
       and there pruning is allowed!   Tue May 29 21:50:17 EDT 2001 -cs *)
  and invertSub args =
    match args with
        (g, ((Shift (n)) as s), ss, rOccur) ->
          if n < ctxLength (g) 
	  then invertSub (g, Dot (Idx (n+1), Shift (n+1)), ss, rOccur)
	  else comp (s, ss)		(* must be defined *)
      | (g, Dot (Idx (n), s'), ss, rOccur) ->
          (match bvarSub (n, ss) with
	       Undef -> raise NotInvertible
	     | ft -> Dot (ft, invertSub (g, s', ss, rOccur)))
      | (g, Dot (Exp (u), s'), ss, rOccur) ->
	  (* below my raise NotInvertible *)
	  Dot (Exp (invertExp (g, (u, id), ss, rOccur)),
	       invertSub (g, s', ss, rOccur))
      (* pruneSub (G, Dot (Undef, s), ss, rOccur) is impossible *)
      (* By invariant, all EVars X[s] are such that s is defined everywhere *)
      (* Pruning establishes and maintains this invariant *)
    (* invertCtx does not appear to be necessary *)
    (*
    and invertCtx (Shift n, Null, rOccur) = Null
      | invertCtx (Dot (Idx k, t), Decl (G, D), rOccur) =
        let 
	  let t' = comp (t, invShift)
	  let D' = invertDec (G, (D, id), t', rOccur)
	in
          Decl (invertCtx (t', G, rOccur), D')
	end
      | invertCtx (Dot (Undef, t), Decl (G, d), rOccur) = 
          invertCtx (t, G, rOccur)
      | invertCtx (Shift n, G, rOccur) = 
	  invertCtx (Dot (Idx (n+1), Shift (n+1)), G, rOccur)
    *)

    (* prune (G, (U, s), ss, rOccur) = U[s][ss]

       !!! looks wrong to me -kw
       G |- U : V    G' |- s : G  (G' |- U[s] : V[s])
       G'' |- ss : G'
       !!! i would say
       G |- s : G'   G' |- U : V  (G  |- U[s] : V[s])
       G'' |- ss : G

       Effect: prunes EVars in U[s] according to ss
               raises Unify if U[s][ss] does not exist, or rOccur occurs in U[s]
    *)
  let rec pruneExp  (g, us, ss, rOccur) = 
        pruneExpW (g, Whnf.whnf us, ss, rOccur)
  and pruneExpW args =
    match args with
        (g, ((Uni _) as u, s), _, _) -> u
      | (g, (Pi ((d, p), v), s), ss, rOccur) -> 
          Pi ((pruneDec (g, (d, s), ss, rOccur), p),
	      pruneExp (Decl (g, decSub (d, s)), (v, dot1 s), dot1 ss, rOccur))
      | (g, (Lam (d, v), s), ss, rOccur) ->
	  Lam (pruneDec (g, (d, s), ss, rOccur),
	       pruneExp (Decl (g, decSub (d, s)), (v, dot1 s), dot1 ss, rOccur))
      | (g, (Root (h, ss'), s (* = id *)), ss, rOccur) -> 
	  Root (pruneHead (g, h, ss, rOccur),
		pruneSpine (g, (ss', s), ss, rOccur))
      | (g, (((EVar (r, gx, v, cnstrs)) as x), s), ss, rOccur) -> 
	  if (rOccur == r) then raise (Unify "Variable occurrence")
	  else if Whnf.isPatSub (s) then
            let w = weakenSub (g, s, ss) in
              if Whnf.isId w
	      then EClo (x, comp (s, ss))
	      else
	        let wi = Whnf.invert w in
               (* let v' = EClo (v, wi) *)
	        let v' = pruneExp (gx, (v, id), wi, rOccur) in
                (* let gy = Whnf.strengthen (wi, gx) *)
	        let gy = pruneCtx (wi, gx, rOccur) in
	     (* shortcut on gy possible by invariant on gx and V[s]? -fp *)
	     (* could optimize by checking for identity subst *)
	        let y = newEVar (gy, v') in
	        let yw = EClo (y, w) in
	        let _ = instantiateEVar (r, yw, !cnstrs) in
                EClo (yw, comp (s, ss))
          else (* s not patsub *)
            (
            try
              EClo (x, invertSub (g, s, ss, rOccur))
	    with NotInvertible -> 
	        (* let GY = Whnf.strengthen (ss, G) *)
                (* shortcuts possible by invariants on G? *)
              let gy = pruneCtx (ss, g, rOccur) in (* prune or invert??? *)
                 (* let v' = EClo (v, comp (s, ss)) *)
	      let v' = pruneExp (g, (v, s), ss, rOccur) in(* prune or invert??? *)
	      let y = newEVar (gy, v') in
	      let _ = addConstraint (cnstrs, ref (Eqn (g, EClo (x, s),
							             EClo (y, Whnf.invert ss)))) in
	      y
	    )
      | (g, (FgnExp (i,csfe), s), ss, rOccur) ->
          FgnExpStd.Map.apply (i,csfe) (fun u -> pruneExp (g, (u, s), ss, rOccur))
      | (g, (((AVar _) as x), s), ss, rOccur) ->  
	(* this case should never happen! *)  
	  raise (Unify "Left-over AVar")


      (* other cases impossible since (U,s1) whnf *)
  and pruneDec args =
    match args with
        (g, (Dec (name, v), s), ss, rOccur) ->
	  Dec (name, pruneExp (g, (v, s), ss, rOccur))
      | (g, (NDec x, _), _, _) -> NDec x
      (* Added for the meta level -cs Tue Aug 17 17:09:27 2004 *)
  and pruneSpine args =
    match args with
        (g, (Nil, s), ss, rOccur) -> Nil
      | (g, (App (u, ss'), s), ss, rOccur) -> 
	  App (pruneExp (g, (u, s), ss, rOccur),
	       pruneSpine (g, (ss', s), ss, rOccur))
      | (g, (SClo (ss', s'), s), ss, rOccur) -> 
	  pruneSpine (g, (ss', comp (s', s)), ss, rOccur)
  and pruneHead args =
    match args with
        (g, BVar k, ss, rOccur) -> 
          (match (bvarSub (k, ss)) with
	       Undef -> raise (Unify "Parameter dependency")
	     | Idx k' -> BVar k')
      | (g, ((Const _) as h), ss, rOccur) -> h
      | (g, Proj (((Bidx k) as b), i), ss, rOccur) ->
	(* blockSub (b, ss) should always be defined *)
	(* Fri Dec 28 10:03:12 2001 -fp !!! *)
	  (match blockSub (b, ss) with
	       Bidx(k') -> Proj (Bidx (k'), i))
      | (g, ((Proj (LVar (r, sk, (l, t)), i)) as h), ss, rOccur) -> 
        (* claim: LVar does not need to be pruned since . |- t : Gsome *)
	(* so we perform only the occurs-check here as for FVars *)
	(* Sat Dec  8 13:39:41 2001 -fp !!! *)
	(* this is not true any more, Sun Dec  1 11:28:47 2002 -cs  *)
	(* Changed from Null to G Sat Dec  7 21:58:00 2002 -fp *)
	   ( pruneSub (g, t, id, rOccur) ;
	     h )
      | (g, ((Skonst _) as h), ss, rOccur) -> h
      | (g, ((Def _) as h), ss, rOccur) -> h
      | (g, FVar (x, v, s'), ss, rOccur) ->
	  (* V does not to be pruned, since . |- V : type and s' = ^k *)
	  (* perform occurs-check for r only *)
	  (pruneExp (g, (v, id), id, rOccur);  (* why g here? -fp !!! *)
	   FVar (x, v, comp (s', ss)))
      | (g, ((FgnConst _) as h), ss, rOccur) -> h
    (* pruneSub never allows pruning OUTDATED *)
    (* in the presence of block variables, this invariant 
       doesn't hold any more, because substitutions do not
       only occur in EVars any more but also in LVars!
       and there pruning is allowed!   Tue May 29 21:50:17 EDT 2001 -cs *)
  and pruneSub args =
    match args with
        (g, ((Shift (n)) as s), ss, rOccur) ->
          if n < ctxLength (g) 
	  then pruneSub (g, Dot (Idx (n+1), Shift (n+1)), ss, rOccur)
	  else comp (s, ss)		(* must be defined *)
      | (g, Dot (Idx (n), s'), ss, rOccur) ->
	  (match bvarSub (n, ss) with
	       Undef -> raise (Unify "Not prunable")
	     | ft -> Dot (ft, pruneSub (g, s', ss, rOccur)))
      | (g, Dot (Exp (u), s'), ss, rOccur) ->
	  (* below my raise Unify *)
	  Dot (Exp (pruneExp (g, (u, id), ss, rOccur)),
	       pruneSub (g, s', ss, rOccur))
      (* pruneSub (G, Dot (Undef, s), ss, rOccur) is impossible *)
      (* By invariant, all EVars X[s] are such that s is defined everywhere *)
      (* Pruning establishes and maintains this invariant *)
  and pruneCtx args =
    match args with
        (Shift n, Null, rOccur) -> Null
      | (Dot (Idx k, t), Decl (g, d), rOccur) ->
	  let t' = comp (t, invShift) in
	  let d' = pruneDec (g, (d, id), t', rOccur) in
          Decl (pruneCtx (t', g, rOccur), d')
      | (Dot (Undef, t), Decl (g, d), rOccur) -> 
          pruneCtx (t, g, rOccur)
      | (Shift n, g, rOccur) -> 
	  pruneCtx (Dot (Idx (n+1), Shift (n+1)), g, rOccur)


    (* unifyExpW (G, (U1, s1), (U2, s2)) = ()
     
       Invariant:
       If   G |- s1 : G1   G1 |- U1 : V1    (U1,s1) in whnf
       and  G |- s2 : G2   G2 |- U2 : V2    (U2,s2) in whnf 
       and  G |- V1 [s1] = V2 [s2]  : L    (for some level L)
        ***** or V1 = V2 = kind  (needed to check type definitions)
        ***** added by kw Apr 5 2002
       and  s1, U1, s2, U2 do not contain any blockvariable indices Bidx
       then if   there is an instantiation I :  
                 s.t. G |- U1 [s1] <I> == U2 [s2] <I>
            then instantiation is applied as effect, () returned
	    else exception Unify is raised
       Other effects: EVars may be lowered
                      constraints may be added for non-patterns
    *)
  let rec unifyExpW (g, us1, us2) =
    match (g, us1, us2) with
        (g, (FgnExp (i1,csfe1), _), (u2, s2)) ->
          (match (FgnExpStd.UnifyWith.apply (i1,csfe1) (g, EClo (u2, s2))) with
               (Succeed residualL) ->
                 let execResidual args =
		   (match args with
		        (Assign (g, EVar(r, _, _, cnstrs), w, ss)) ->
                          let w' = pruneExp (g, (w, id), ss, r) in
                          instantiateEVar (r, w', !cnstrs)
                      | (Delay (u, cnstr)) ->
                          delayExp ((u, id), cnstr))
                  in
                  List.iter execResidual residualL
              | Fail -> raise (Unify "Foreign Expression Mismatch"))
      | (g, (u1, s1), (FgnExp (i2,csfe2), _)) ->
          (match (FgnExpStd.UnifyWith.apply (i2,csfe2) (g, EClo (u1, s1))) with
               (Succeed opL) ->
                 let execOp args =
		   (match args with
                        (Assign (g, EVar(r, _, _, cnstrs), w, ss)) ->
                          let w' = pruneExp (g, (w, id), ss, r) in
                          instantiateEVar (r, w', !cnstrs)
                      | (Delay (u, cnstr)) -> delayExp ((u, id), cnstr))
                  in
                  List.iter execOp opL
              | Fail -> raise (Unify "Foreign Expression Mismatch"))

      | (g, (Uni (l1), _), (Uni(l2), _)) ->
          (* L1 = L2 = type, by invariant *)
          (* unifyUni (L1, L2) - removed Mon Aug 24 12:18:24 1998 -fp *)
          ()

      | (g, (Root ((h1 : head), ss1), s1), (Root ((h2 : head), ss2), s2)) ->
	  (* s1 = s2 = id by whnf *)
          (* order of calls critical to establish unifySpine invariant *)
          (match (h1, h2) with 
 	       (BVar(k1), BVar(k2)) -> 
	         if (k1 = k2) then unifySpine (g, (ss1, s1), (ss2, s2))
	         else raise (Unify "Bound variable clash")
  	     | (Const(c1), Const (c2)) ->  
	         if (c1 = c2) then unifySpine (g, (ss1, s1), (ss2, s2))
	         else raise (Unify "Constant clash")
	     | (Proj (b1, i1), Proj (b2, i2)) ->
	         if (i1 = i2) then (unifyBlock (g, b1, b2); unifySpine (g, (ss1, s1), (ss2, s2)))
	         else raise (Unify "Global parameter clash")
	     | (Skonst(c1), Skonst(c2)) -> 	  
	         if (c1 = c2) then unifySpine (g, (ss1, s1), (ss2, s2))
	         else raise (Unify "Skolem constant clash")
	     | (FVar (n1,_,_), FVar (n2,_,_)) ->
	         if (n1 = n2) then unifySpine (g, (ss1, s1), (ss2, s2))
	         else raise (Unify "Free variable clash")
	     | _ ->
                 raise (Unify ("Head mismatch: "^(IntSyn.exp_to_string (Root (h1,ss1)))^" and "^(IntSyn.exp_to_string (Root (h2, ss2)))))  )
(*	     | (Def (d1), Def (d2)) ->
	         if (d1 = d2) then (* because of strict *) 
	           unifySpine (g, (ss1, s1), (ss2, s2))
	         else (*  unifyExpW (G, Whnf.expandDef (Us1), Whnf.expandDef (Us2)) *)
		   unifyDefDefW (g, us1, us2)
	   (* four new cases for defined constants *)
	     | (Def (d1), Const (c2)) ->
	       (match defAncestor d1 with
	            Anc (_, _, None) -> (* conservative *) unifyExpW (g, Whnf.expandDef us1, us2)
		  | Anc (_, _, Some(c1)) ->
		      if (c1 = c2) then unifyExpW (g, Whnf.expandDef us1, us2)
		      else raise (Unify "Constant clash"))
	     | (Const (c1), Def (d2)) ->
	         (match defAncestor d2 with
                      Anc (_, _, None) -> (* conservative *) unifyExpW (g, us1, Whnf.expandDef us2)
                    | Anc (_, _, Some(c2)) ->
		        if (c1 = c2) then unifyExpW (g, us1, Whnf.expandDef us2)
		        else raise (Unify "Constant clash"))
             | (Def (d1), BVar (k2)) -> raise (Unify "Head mismatch") (* due to strictness! *)
	     | (BVar (k1), Def (d2)) -> (raise Unify "Head mismatch") (* due to strictness! *)
           (* next two cases for def/fgn, fgn/def *)
	     | (Def (d1), _) -> unifyExpW (g, Whnf.expandDef us1, us2)
	     | (_, Def(d2)) -> unifyExpW (g, us1, Whnf.expandDef us2)
             | (FgnConst (cs1, ConDec (n1, _, _, _, _, _)), FgnConst (cs2, ConDec (n2, _, _, _, _, _))) ->
               (* we require unique string representation of external constants *)
                 if (cs1 = cs2) andalso (n1 = n2) then ()
                 else raise (Unify "Foreign Constant clash")
             | (FgnConst (cs1, ConDef (n1, _, _, w1, _, _, _)), FgnConst (cs2, ConDef (n2, _, _, v, w2, _, _))) ->
                 if (cs1 = cs2) andalso (n1 = n2) then ()
                 else unifyExp (g, (w1, s1), (w2, s2))
             | (FgnConst (_, ConDef (_, _, _, w1, _, _, _)), _) ->
                 unifyExp (g, (w1, s1), us2)
             | (_, FgnConst (_, ConDef (_, _, _, w2, _, _, _))) ->
                 unifyExp (g, us1, (w2, s2))              *)
      | (g, (Pi ((d1, _), u1), s1), (Pi ((d2, _), u2), s2)) ->         
	  (unifyDec (g, (d1, s1), (d2, s2)) ; 
           unifyExp (Decl (g, decSub (d1, s1)), (u1, dot1 s1), (u2, dot1 s2)))
(*
      | (g,(Pi (_, _), _), (Root (Def _, _), _)) ->
	  unifyExpW (g, us1, Whnf.expandDef (us2))
      | (g, (Root (Def _, _), _), (Pi (_, _), _)) ->
	  unifyExpW (g, Whnf.expandDef (us1), us2)
*)
      | (g, (Lam (d1, u1), s1), (Lam (d2, u2), s2)) ->           
          (* D1[s1] = D2[s2]  by invariant *)
	  unifyExp (Decl (g, decSub (d1, s1)), (u1, dot1 s1),(u2, dot1 s2))

      | (g, (Lam (d1, u1), s1), (u2, s2)) ->	                   
	  (* ETA: can't occur if eta expanded *)
	  unifyExp (Decl (g, decSub (d1, s1)), (u1, dot1 s1), 
		    (Redex (EClo (u2, shift), App (Root (BVar (1), Nil), Nil)), dot1 s2))
           (* for rhs:  (U2[s2])[^] 1 = U2 [s2 o ^] 1 = U2 [^ o (1. s2 o ^)] 1
                        = (U2 [^] 1) [1.s2 o ^] *)

      | (g, (u1, s1), (Lam (d2, u2), s2)) ->                     
          (* Cannot occur if expressions are eta expanded *)
	  unifyExp (Decl (g, decSub (d2, s2)), 
		    (Redex (EClo (u1, shift), App (Root (BVar (1), Nil), Nil)), dot1 s1),
		    (u2, dot1 s2))  
	   (* same reasoning holds as above *)
	
      | (g, (((EVar(r1, g1, v1, cnstrs1)) as u1), s1),
		   (((EVar(r2, g2, v2, cnstrs2)) as u2), s2)) ->
	  (* postpone, if s1 or s2 are not patsub *)
	  if r1 == r2 then 
	    if Whnf.isPatSub(s1) then 
	      if Whnf.isPatSub(s2) then
                let s' = intersection (s1,s2) in	(* compute ss' directly? *)
		  (* without the next optimization, bugs/hangs/sources.cfg
		     would gets into an apparent infinite loop
		     Fri Sep  5 20:23:27 2003 -fp 
		  *)
                if Whnf.isId s' (* added for definitions Mon Sep  1 19:53:13 2003 -fp *)
	        then () (* X[s] = X[s] *)
	        else
		  let ss' = Whnf.invert s' in
		  let v1' = EClo (v1, ss')  in(* invertExp ((v1, id), s', ref None) *)
		  let g1' = Whnf.strengthen (ss', g1) in
                  instantiateEVar (r1, (EClo (newEVar (g1', v1'), s')), !cnstrs1)
	      else addConstraint (cnstrs2, ref (Eqn (g, EClo (u2, s2), EClo (u1,s1))))
            else addConstraint (cnstrs1, ref (Eqn (g, EClo (u1, s1), EClo (u2,s2))))
	  else
	    if Whnf.isPatSub(s1) then 
              let ss1 = Whnf.invert s1 in
	      let u2' = pruneExp (g, us2, ss1, r1) in
		(* instantiateEVar (r1, EClo (U2, comp(s2, ss1)), !cnstrs1) *)
		(* invertExpW (Us2, s1, ref None) *)
	      instantiateEVar (r1, u2', !cnstrs1)
	    else if Whnf.isPatSub(s2) then 
              let ss2 = Whnf.invert s2 in
	      let u1' = pruneExp (g, us1, ss2, r2) in
		(* instantiateEVar (r2, EClo (U1, comp(s1, ss2)), !cnstr2) *)
	        (* invertExpW (Us1, s2, ref None) *)
              instantiateEVar (r2, u1', !cnstrs2)
            else
              let cnstr = ref (Eqn (g, EClo (u1,s1), EClo (u2,s2))) in
              addConstraint (cnstrs1, cnstr)

      | (g, (EVar(r, gx, v, cnstrs) as u1, s), (u2,s2)) ->
	  if Whnf.isPatSub(s) then
	    let ss = Whnf.invert s in
	    let u2' = pruneExp (g, us2, ss, r) in
	    (* instantiateEVar (r, EClo (U2, comp(s2, ss)), !cnstrs) *)
	    (* invertExpW (Us2, s, r) *)
	    instantiateEVar (r, u2', !cnstrs)
          else
            addConstraint (cnstrs, ref (Eqn (g, EClo (u1,s), EClo (u2,s2))))

      | (g,(u1,s1), (EVar (r, gx, v, cnstrs) as u2, s)) ->
	  if Whnf.isPatSub(s) then 
	    let ss = Whnf.invert s in
	    let u1' = pruneExp (g, us1, ss, r) in
	    (* instantiateEVar (r, EClo (U1, comp(s1, ss)), !cnstrs) *)
	    (* invertExpW (Us1, s, r) *)
	    instantiateEVar (r, u1', !cnstrs)
        else
          addConstraint (cnstrs, ref (Eqn (g, EClo (u1,s1), EClo (u2,s))))

      | (g, us1, us2) -> 
        raise (Unify "Expression clash")

    (* covers most remaining cases *)
    (* the cases for EClo or Redex should not occur because of whnf invariant *)

    (* unifyExp (g, (U1, s1), (U2, s2)) = ()
       as in unifyExpW, except that arguments may not be in whnf 
    *)
  and unifyExp (g, (e1,s1), (e2,s2)) = 
          unifyExpW (g, Whnf.whnf (e1,s1), Whnf.whnf (e2,s2))
(*
  and unifyDefDefW (g, (((Root (Def (d1), ss1)), s1) as us1), ((Root (Def (d2), ss2), s2) as us2)) =
        (*  unifyExpW (g, Whnf.expandDef (Us1), Whnf.expandDef (Us2)) *)
    let Anc (_, h1, c1Opt) = defAncestor d1 in
    let Anc (_, h2, c2Opt) = defAncestor d2 in
    let _ =
      match(c1Opt,c2Opt) with
	  (Some(c1), Some(c2)) ->
	    if c1 <> c2
	    then raise (Unify "Irreconcilable defined constant clash")
	    else ()
         | _ -> () (* conservative *)
    in
    match Int.compare (h1, h2) with
	 EQUAL -> unifyExpW (g, Whnf.expandDef (us1), Whnf.expandDef (us2))
       | LESS -> unifyExpW (g, us1, Whnf.expandDef (us2))
       | GREATER -> unifyExpW (g, Whnf.expandDef (us1), us2)
*)
    (* unifySpine (G, (S1, s1), (S2, s2)) = ()
     
       Invariant:
       If   G |- s1 : G1   G1 |- S1 : V1 > W1 
       and  G |- s2 : G2   G2 |- S2 : V2 > W2 
       and  G |- V1 [s1] = V2 [s2]  : L    (for some level L)
       and  G |- W1 [s1] = W2 [s2]  
       then if   there is an instantiation I :
                 s.t. G |- S1 [s1] <I> == S2 [s2] <I> 
            then instantiation is applied as effect, () returned
	    else exception Unify is raised
       Other effects: EVars may be lowered,
                      constraints may be added for non-patterns
    *)
  and unifySpine args =
    match args with
        (g, (Nil,_), (Nil,_)) -> ()
      | (g, (SClo (ss1, s1'), s1), sss) -> unifySpine (g, (ss1, comp (s1', s1)), sss)
      | (g, sss, (SClo (ss2, s2'), s2)) -> unifySpine (g, sss, (ss2, comp (s2', s2)))
      | (g, (App (u1, ss1), s1), (App (u2, ss2), s2)) -> 
        (unifyExp (g, (u1, s1), (u2, s2)) ; 
	 unifySpine (g, (ss1, s1), (ss2, s2)))
      (* Nil/App or App/Nil cannot occur by typing invariants *)

  and unifyDec (g, (Dec(_, v1), s1), (Dec (_, v2), s2)) =
          unifyExp (g, (v1, s1), (v2, s2))

    (* unifySub (G, s1, s2) = ()
     
       Invariant:
       If   G |- s1 : G'
       and  G |- s2 : G'
       then unifySub (G, s1, s2) terminates with () 
	    iff there exists an instantiation I, such that
	    s1 [I] = s2 [I]

       Remark:  unifySub is used only to unify the instantiation of Some variables
    *)
    (* conjecture: G == Null at all times *)
    (* Thu Dec  6 21:01:09 2001 -fp *)
  and unifySub args =
    match args with
        (g, Shift (n1), Shift (n2)) -> ()
         (* by invariant *)
      | (g, Shift(n), ((Dot _) as s2)) -> 
          unifySub (g, Dot(Idx(n+1), Shift(n+1)), s2)
      | (g, ((Dot _) as s1), Shift(m)) -> 
	  unifySub (g, s1, Dot(Idx(m+1), Shift(m+1)))
      | (g, Dot(ft1,s1), Dot(ft2,s2)) ->
	  ((match (ft1, ft2) with
	       (Idx (n1), Idx (n2)) -> 
	         if n1 <> n2 then raise (Error "Some variables mismatch")
	         else ()                      
             | (Exp (u1), Exp (u2)) -> unifyExp (g, (u1, id), (u2, id))
	     | (Exp (u1), Idx (n2)) -> unifyExp (g, (u1, id), (Root (BVar (n2), Nil), id))
             | (Idx (n1), Exp (u2)) -> unifyExp (g, (Root (BVar (n1), Nil), id), (u2, id)));
(*	     | (Undef, Undef) -> 
	     | _ -> false *)   (* not possible because of invariant? -cs *)
	          unifySub (g, s1, s2))

    (* substitutions s1 and s2 were redundant here --- removed *)
    (* Sat Dec  8 11:47:12 2001 -fp !!! *)
  and unifyBlock args =
    match args with
        (g, LVar (r, s, _), b2) when Option.isSome (!r) -> unifyBlock (g, blockSub (Option.get (!r), s), b2)
      | (g, b1, LVar (r, s, _)) when Option.isSome (!r) -> unifyBlock (g, b1, blockSub (Option.get (!r), s))
      | (g, b1, b2) -> unifyBlockW (g, b1, b2)

  and unifyBlockW args =
    match args with
        (g, LVar (r1, ((Shift(k1)) as s1), (l1, t1)), LVar (r2, ((Shift(k2)) as s2), (l2, t2))) -> 
          if l1 <> l2 then
  	    raise (Unify "Label clash")
          else
	    if r1 == r2
	    then ()
	    else
	    ( unifySub (g, comp(t1,s1), comp(t2,s2)) ; (* Sat Dec  7 22:04:31 2002 -fp *)
	      (* was: unifySub (G, t1, t2)  Jul 22 2010 *)
	      if k1 < k2 then instantiateLVar (r1, LVar(r2, Shift(k2-k1), (l2, t2)))
	      else instantiateLVar (r2, LVar(r1, Shift (k1-k2), (l1, t1)))
	    )

      | (g, LVar (r1, s1, (l1, t1)),  b2) -> 
	    instantiateLVar(r1, blockSub (b2, Whnf.invert s1)) (* -- ABP *)
	    
      | (g,  b1, LVar (r2, s2, (l2, t2))) -> 
	    instantiateLVar(r2, blockSub (b1, Whnf.invert s2)) (* -- ABP *)

(*      | unifyBlockW (G, LVar (r1, Shift(k1), (l1, t1)), Bidx i2) -> 
	    (r1 := Some (Bidx (i2 -k1)) ; ()) (* -- ABP *)

      | unifyBlockW (G, Bidx i1, LVar (r2, Shift(k2), (l2, t2))) -> 
	    (r2 := Some (Bidx (i1 -k2)) ; ()) (* -- ABP *)
*)
      (* How can the next case arise? *)
      (* Sat Dec  8 11:49:16 2001 -fp !!! *)      
      | (g, Bidx (n1), (Bidx (n2))) ->
	  if n1 <> n2
	  then raise (Unify "Block index clash")
	  else ()


(*
      | unifyBlock (LVar (r1, _, _), B as Bidx _) -> instantiate (r1, B) 
      | unifyBlock (B as Bidx _, LVar (r2, _, _)) -> 

      This is still difficult --- B must make sense in the context of the LVar
      Shall we use the inverse of a pattern substitution? Or postpone as 
      a constraint if pattern substitution does not exist?
      Sun Dec  1 11:33:13 2002 -cs 
      
*)
  let rec unify1W (g, us1, us2) =
          (unifyExpW (g, us1, us2); awakeCnstr (nextCnstr ()))

  and unify1 (g, us1, us2) =
          (unifyExp (g, us1, us2); awakeCnstr (nextCnstr ()))

  and awakeCnstr arg =
    match arg with
        (None) -> ()
      | (Some(s)) when s = ref Solved -> awakeCnstr (nextCnstr ())
      | (Some(cnstr)) ->
          (match !cnstr with
             (Eqn (g, u1, u2)) ->
               (solveConstraint cnstr;
                  unify1 (g, (u1, id), (u2, id)))
           | (FgnCnstr (i,csfc)) ->
               if (FgnCnstrStd.Awake.apply (i,csfc) ()) then ()
               else raise (Unify "Foreign constraint violated"))

    let unifyW (g, us1, us2) =
          (resetAwakenCnstrs (); unify1W (g, us1, us2))

    let unify (g, us1, us2) =
          (resetAwakenCnstrs (); unify1 (g, us1, us2))



  let delay = delayExp
	    
    let invertible (g, us, ss, rOccur) =
      try
        (invertExp (g, us, ss, rOccur); true)
      with NotInvertible -> false

    let unifiable (g, us1, us2) =
      try
        (unify (g, us1, us2); 
	   true)
      with Unify msg ->  false

    let unifiable' (g, us1, us2) = 
      try
        (unify (g, us1, us2); None)
      with Unify(msg) -> Some(msg)
