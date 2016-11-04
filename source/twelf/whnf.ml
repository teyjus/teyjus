
  (*
     Weak Head-Normal Form (whnf)

     whnf ::= (L, s) | (Pi DP. U, s) | (Root (#k(b), S))
            | (Root(n,S), id) | (Root(c,S), id) | (Root(d,S), id) | (Root(F[s'], S), id)
            | (Root(fgnC,S), id) where fgnC is a foreign constant
            | (Lam D. U, s) | (X, s) where X is uninstantiated, X of base type
                                     during type reconstruction, X might have variable type
            | (FgnExp, id) where FgnExp is a foreign expression

     Normal Form (nf)

	UA ::= L | Pi (DA,P). UA
             | Root(n,SA) | Root(c,SA) | Root(d,SA) | Root(fgnC,SA) | Root (#k(b), S)
             | Lam DA. UA | FgnExp
	DA ::= x:UA
	SA ::= Nil | App (UA, SA)

     Existential Normal Form (enf)

     Existential normal forms are like normal forms, but also allows
     X[s] where X is uninstantiated with no particular restriction on s
     or type of X.

     An existential normal form is a hereditary weak head-normal form.
  *)
  open IntSyn


    (* exception Undefined *)

  exception Eta

    (* etaContract' (U, s, n) = k'

       Invariant: 
       if   G, V1, .., Vn |- s : G1  and  G1 |- U : V
       then if   lam V1...lam Vn. U[s] =eta*=> k 
	    then k' = k
            and  G |- k' : Pi V1...Pi Vn. V [s]
	    else Eta is raised
	      (even if U[s] might be eta-reducible to some other expressions).
    *)
    (* optimization(?): quick check w/o substitution first *)
  let rec etaContract' args =
    match args with
        (Root (BVar(k), ss), s, n) ->
          (match bvarSub (k, s) with
	       Idx (k') -> if k' > n
	                   then (etaContract'' (ss, s, n); k'-n)
			    else raise Eta
	      | _ -> raise Eta)
        | (Lam (d, u), s, n) ->
  	    etaContract' (u, dot1 s, n+1)
        | (EClo (u, s'), s, n) ->
	    etaContract' (u, comp (s', s), n)
        | (EVar (r, _, _, _), s, n) when Option.isSome (!r) ->
            etaContract' (Option.get (!r), s, n)
        | (AVar r, s, n) when Option.isSome (!r) ->
            etaContract' (Option.get (!r), s, n) 
        | _ -> raise Eta
        (* Should fail: (c@S), (d@S), (F@S), X *)
        (* Not treated (fails): U@S *)
        (* Could weak head-normalize for more thorough checks *)
        (* Impossible: L, Pi D.V *)

    (* etaContract'' (S, s, n) = R'

       Invariant:
       If  G |- s : G1    and  G1 |- S : V > W
       then if   S[s] =eta*=> n ; n-1 ; ... ; 1 ; Nil
	    then () 
       else Eta is raised
    *)
  and etaContract'' args =
    match args with
        (Nil, s, 0) -> ()
      | (App(u, ss), s, n) ->
          if etaContract' (u, s, 0) = n
	  then etaContract'' (ss, s, n-1)
	  else raise Eta
      | (SClo (ss, s'), s, n) ->
	  etaContract'' (ss, comp (s', s), n)
      | _ -> raise Eta

    (* dotEta (Ft, s) = s'
       
       Invariant: 
       If   G |- s : G1, V  and G |- Ft : V [s]
       then Ft  =eta*=>  Ft1
       and  s' = Ft1 . s
       and  G |- s' : G1, V
    *)
  let dotEta (ft, s) =
    match (ft, s) with
        (Idx _, s) -> Dot (ft, s)
      | (Exp (u), s) ->
	  let ft' = try Idx (etaContract' (u, id, 0))
		    with Eta -> ft
          in
	  Dot (ft', s)
      | (Undef, s) -> Dot (ft, s)
 

    (* appendSpine ((S1, s1), (S2, s2)) = S' 

       Invariant:
       If    G |- s1 : G1   G1 |- S1 : V1' > V1 
       and   G |- s2 : G2   G2 |- S2 : V2  > V2'
       and   G |- V1 [s1] == V2 [s2]
       then  G |- S' : V1' [s1] > V2' [s2]   
    *)
  let rec appendSpine args =
    match args with
        ((Nil, s1), ss2) -> let (p1, p2) = ss2 in SClo (p1, p2)
      | ((App (u1, ss1), s1), ss2) ->
          App (EClo (u1, s1), appendSpine ((ss1, s1), ss2))
      | ((SClo (ss1, s1'), s1), ss2) ->
	  appendSpine ((ss1, comp(s1', s1)), ss2)

    (* whnfRedex ((U, s1), (S, s2)) = (U', s')

       Invariant:
       If    G |- s1 : G1   G1 |- U : V1,   (U,s1) whnf 
	     G |- s2 : G2   G2 |- S : V2 > W2
	     G |- V1 [s1] == V2 [s2] == V : L
       then  G |- s' : G',  G' |- U' : W'
       and   G |- W'[s'] == W2[s2] == W : L
       and   G |- U'[s'] == (U[s1] @ S[s2]) : W
       and   (U',s') whnf

       Effects: EVars may be lowered to base type.
    *)
  let rec whnfRedex (us, (arg2, arg3)) =
    match (us, (arg2, arg3)) with
        (us, (SClo (ss, s2'), s2)) ->
          whnfRedex (us, (ss, comp (s2', s2)))
      | ((Root (r1, r2), s1), (Nil, s2)) -> us
      | ((Root (h1, ss1), s1), (ss2, s2)) ->
	  (* S2 = App _, only possible if term is not eta-expanded *)
	  (Root (h1, appendSpine ((ss1, s1), (ss2, s2))), id)
      | ((Lam (_, u1), s1), (App (u2, ss), s2)) ->
	  whnfRedex (whnf (u1, dotEta (frontSub (Exp (u2), s2), s1)), (ss, s2)) 
      | ((Lam _, s1), _) -> us  (* S2[s2] = Nil *)
      | ((EVar _, s1), (Nil, s2)) -> us
      | (((EVar _) as x, s1), sss2) -> 
	  (* Ss2 must be App, since prior cases do not apply *)
	  (* lowerEVar X results in redex, optimize by unfolding call to whnfRedex *)
	  (lowerEVar x; whnfRedex (whnf us, sss2))
      | ((AVar r, s1), sss2) when Option.isSome (!r) -> 
	  whnfRedex((Option.get (!r),s1), sss2)
      | ((AVar r, s1), sss2) when Option.isNone (!r)-> us
      | ((FgnExp _, _), _) -> us
      (* Uni and Pi can arise after instantiation of EVar X : K *)
      | ((Uni _, s1), _) -> us	(* S2[s2] = Nil *)
      | ((Pi _, s1), _) -> us	(* S2[s2] = Nil *)
      (* Other cases impossible since (U,s1) whnf *)

    (* lowerEVar' (G, V[s]) = (X', U), see lowerEVar *)
  and lowerEVar' args =
    match args with
        (g, (Pi ((d',_), v'), s')) ->
          let d'' = decSub (d', s') in
          let (x', u) = lowerEVar' (Decl (g, d''), whnfExpandDef (v', dot1 s')) in
          (x', Lam (d'', u))
      | (g, vs') ->
	 let (p1, p2) = vs' in
	  let x' = newEVar (g, (EClo (p1,p2))) in
	  (x', x')
    (* lowerEVar1 (X, V[s]), V[s] in whnf, see lowerEVar *)
  and lowerEVar1 (x, vs) =
    match (x, vs) with
        (EVar (r, g, _, _), (Pi _, _)) ->
	  let (x', u) = lowerEVar' (g, vs) in
	  let _ = r := Some (u) in
	  x'
      | (x, _) -> x

    (* lowerEVar (X) = X'

       Invariant:
       If   G |- X : {{G'}} P
            X not subject to any constraints
       then G, G' |- X' : P

       Effect: X is instantiated to [[G']] X' if G' is empty
               otherwise X = X' and no effect occurs.
    *)
  and lowerEVar x =
    match x with
        (EVar (r, g, v, n)) when n = ref [] -> lowerEVar1 (x, whnfExpandDef (v, id))
      | (EVar _) ->
        (* It is not clear if this case can happen *)
        (* pre-Twelf 1.2 code walk, Fri May  8 11:05:08 1998 *)
        raise (Error "Typing ambiguous -- constraint of functional type cannot be simplified")


    (* whnfRoot ((H, S), s) = (U', s')

       Invariant:
       If    G |- s : G1      G1 |- H : V
			      G1 |- S : V > W
       then  G |- s' : G'     G' |- U' : W'
       and   G |- W [s] = W' [s'] : L

       Effects: EVars may be instantiated when lowered
    *)
  and whnfRoot args =
    match args with
        ((BVar (k), ss), s)   ->
          (match bvarSub (k, s) with
	       Idx (k) -> (Root (BVar (k), SClo (ss, s)), id)
	     | Exp (u) -> whnfRedex (whnf (u, id), (ss, s)))
      (* Undef should be impossible *)
      | ((Proj (((Bidx _) as b), i), ss), s) -> 
	 (* could blockSub (B, s) return instantiated LVar ? *)
	 (* Sat Dec  8 13:43:17 2001 -fp !!! *)
	 (* yes Thu Dec 13 21:48:10 2001 -fp !!! *)
	 (* was: (Root (Proj (blockSub (B, s), i), SClo (S, s)), id) *)
	  (match blockSub (b, s) with
               (Bidx (k)) as b' -> (Root (Proj (b', i), SClo (ss, s)), id)
             | (LVar _) as b' -> whnfRoot ((Proj (b', i), SClo (ss, s)), id)
	     | Inst l -> whnfRedex (whnf (List.nth l (i-1), id), (ss, s)))
      | ((Proj (LVar (r, sk, (l, t)), i), ss), s) when Option.isSome (!r) ->
	   whnfRoot ((Proj (blockSub (Option.get (!r), comp (sk, s)), i), SClo (ss, s)), id)
      | ((Proj (((LVar (r, sk, (l, t)))), i), ss), s) -> (* r = ref None *)
  	   (Root (Proj (LVar (r, comp (sk, s), (l, t)), i), SClo (ss, s)), id)
	 (* scary: why is comp(sk, s) = ^n ?? -fp July 22, 2010, -fp -cs *)
	(* was:
	 (Root (Proj (LVar (r, comp (sk, s), (l, comp(t, s))), i), SClo (ss, s)), id)
	 Jul 22, 2010 -fp -cs
	 *)
         (* do not compose with t due to globality invariant *)
	 (* Thu Dec  6 20:34:30 2001 -fp !!! *)
	 (* was: (Root (Proj (L, i), SClo (ss, s)), id) *)
	 (* going back to first version, because globality invariant *)
	 (* no longer satisfied Wed Nov 27 09:49:58 2002 -fp *)
      (* Undef and Exp should be impossible by definition of substitution -cs *)
      | ((FVar (name, v, s'), ss), s) ->
	   (Root (FVar (name, v, comp (s', s)), SClo (ss, s)), id)
(*      | ((NSDef (d), ss), s) ->
	    whnfRedex (whnf (IntSyn.constDef d, id), (ss, s))  *)
      | ((h, ss), s) ->
	   (Root (h, SClo (ss, s)), id)

    (* whnf (U, s) = (U', s')

       Invariant:
       If    G |- s : G'    G' |- U : V
       then  G |- s': G''   G''|- U' : V'
       and   G |- V [s] == V' [s'] == V'' : L  
       and   G |- U [s] == U' [s'] : V'' 
       and   (U', s') whnf
    *)
    (*
       Possible optimization :
         Define whnf of Root as (Root (n , S [s]), id)
	 Fails currently because appendSpine does not necessairly return a closure  -cs
	 Advantage: in unify, abstract... the spine needn't be treated under id, but under s
    *)
  and whnf (u, s) =
    match (u, s) with
        (Uni _, s) -> (u,s)
      | (Pi _, s) -> (u,s)
      (* simple optimization (C@S)[id] = C@S[id] *)
      (* applied in Twelf 1.1 *)
      (* Sat Feb 14 20:53:08 1998 -fp *)
(*      | whnf (Us as (Root _, Shift (0))) = Us*)
      (* commented out, because non-strict definitions slip
	 Mon May 24 09:50:22 EDT 1999 -cs  *)
      | (Root (r1, r2), s) ->  whnfRoot ((r1,r2), s)
      | (Redex (u, ss), s) ->  whnfRedex (whnf (u, s), (ss, s))
      | ((Lam _, s)) -> (u,s)
      | (AVar r, s) when Option.isSome (!r) ->  whnf (Option.get (!r), s)
      | ((AVar _, s)) ->  (u,s)
      | (EVar (r, _, _, _), s) when Option.isSome (!r) -> whnf (Option.get (!r), s)
      (* | (us as (EVar _, s)) = us *)
      (* next two avoid calls to whnf (V, id), where V is type of X *)
      | ((EVar (r, _, Root _, _), s)) ->  (u, s)
      | ((EVar (r, _, Uni _, _), s)) ->  (u, s)
      | ((((EVar (r, _, v, _)) as x), s)) -> 
          (match whnf (v, id) with
	       (Pi _, _) -> (lowerEVar x; whnf (u, s))
	                     (* possible opt: call lowerEVar1 *)
	      | _ -> (u, s))
      | (EClo (u, s'), s) -> whnf (u, comp (s', s))
      | ((FgnExp _, Shift (0))) -> (u, s)
      | ((FgnExp (i, csfe) , s)) ->
          (FgnExpStd.Map.apply (i, csfe) (fun u -> EClo (u, s)), id)

    (* expandDef (Root (Def (d), S), s) = (U' ,s')
     
       Invariant:
       If    G |- s : G1     G1 |- S : V > W            ((d @ S), s) in whnf
                             .  |- d = U : V'  
       then  G |- s' : G'    G' |- U' : W'
       and   G |- V' == V [s] : L
       and   G |- W' [s'] == W [s] == W'' : L
       and   G |- (U @ S) [s] == U' [s'] : W'
       and   (U', s') in whnf
    *)
(*
  and expandDef (Root (Def (d), ss), s) =
          (* why the call to whnf?  isn't constDef (d) in nf? -kw *)
    whnfRedex (whnf (constDef (d), id), (ss, s))
*)
  and whnfExpandDefW us =
    match us with
        (*((Root (Def _, _), _)) -> whnfExpandDefW (expandDef us) *)
      | us -> us
  and whnfExpandDef us = whnfExpandDefW (whnf us)

  let rec newLoweredEVarW args =
    match args with
        (g, (Pi ((d, _), v), s)) ->
          let d' = decSub (d, s) in
          Lam (d', newLoweredEVar (Decl (g, d'), (v, dot1 s)))
      | (g, (vs1, vs2)) -> newEVar (g, EClo (vs1, vs2))

  and newLoweredEVar (g, vs) = newLoweredEVarW (g, whnfExpandDef vs)

  let rec newSpineVarW args =
    match args with
        (g, (Pi ((Dec (_, va), _), vr), s)) ->
          let x = newLoweredEVar (g, (va, s)) in
          App (x, newSpineVar (g, (vr, dotEta (Exp (x), s))))
      | (g, _) -> Nil
                   
  and newSpineVar (g, vs) = newSpineVarW (g, whnfExpandDef vs)
                   
  let rec spineToSub args =
    match args with
        (Nil, s) -> s
      | (App (u, ss), s) -> spineToSub (ss, dotEta (Exp (u), s))


    (* inferSpine ((S, s1), (V, s2)) = (V', s')

       Invariant:
       If  G |- s1 : G1  and  G1 |- S : V1 > V1'
       and G |- s2 : G2  and  G2 |- V : L,  (V, s2) in whnf
       and G |- S[s1] : V[s2] > W  (so V1[s1] == V[s2] and V1[s1] == W)
       then G |- V'[s'] = W
    *)
    (* FIX: this is almost certainly mis-design -kw *)
  let rec inferSpine args =
    match args with
        ((Nil, _), vs) -> vs
      | ((SClo (ss, s'), s), vs) -> 
          inferSpine ((ss, comp (s', s)), vs)
      | ((App (u, ss), s1), (Pi (_, v2), s2)) ->
	  inferSpine ((ss, s1), whnfExpandDef (v2, Dot (Exp (EClo (u, s1)), s2)))

    (* inferCon (C) = V  if C = c or C = d or C = sk and |- C : V *)
    (* FIX: this is almost certainly mis-design -kw *)
  let inferCon arg =
    match arg with
        (Const (cid)) -> constType (cid)
      | (Skonst (cid)) -> constType (cid) 
      | (Def (cid)) -> constType (cid)

    (* etaExpand' (U, (V,s)) = U'
           
       Invariant : 
       If    G |- U : V [s]   (V,s) in whnf
       then  G |- U' : V [s]
       and   G |- U == U' : V[s]
       and   (U', id) in whnf and U' in head-eta-long form
    *)
    (* quite inefficient -cs *)
    (* FIX: this is almost certainly mis-design -kw *)
  let rec etaExpand' args =
    match args with
        (u, (Root _, s)) -> u
      | (u, (Pi ((d, _), v), s)) ->
          Lam (decSub (d, s), 
	       etaExpand' (Redex (EClo (u, shift), 
				  App (Root (BVar (1), Nil), Nil)), whnfExpandDef (v, dot1 s)))

    (* etaExpandRoot (Root(H, S)) = U' where H = c or H = d

       Invariant:
       If   G |- H @ S : V  where H = c or H = d
       then G |- U' : V
       and  G |- H @ S == U'
       and (U',id) in whnf and U' in head-eta-long form
    *)
    (* FIX: this is almost certainly mis-design -kw *)
  let etaExpandRoot ((Root(h, ss)) as u) =
          etaExpand' (u, inferSpine ((ss, id), (inferCon(h), id)))

    (* whnfEta ((U, s1), (V, s2)) = ((U', s1'), (V', s2)')
     
       Invariant:
       If   G |- s1 : G1  G1 |- U : V1
       and  G |- s2 : G2  G2 |- V : L
       and  G |- V1[s1] == V[s2] : L

       then G |- s1' : G1'  G1' |- U' : V1'
       and  G |- s2' : G2'  G2' |- V' : L'
       and  G |- V1'[s1'] == V'[s2'] : L
       and (U', s1') is in whnf
       and (V', s2') is in whnf
       and (U', s1') == Lam x.U'' if V[s2] == Pi x.V''

       Similar to etaExpand', but without recursive expansion
    *)
    (* FIX: this is almost certainly mis-design -kw *)
  let rec whnfEta (us, vs) = whnfEtaW (whnf us, whnf vs)

  and whnfEtaW usvs =
    match usvs with
        ((_, (Root _, _))) -> usvs
      | (((Lam _, _), (Pi _, _))) -> usvs
      | ((u, s1), ((Pi ((d, p), v), s2) as vs2)) ->
          ((Lam (decSub (d, s2), 
		 Redex (EClo (u, comp (s1, shift)), 
			App (Root (BVar (1), Nil), Nil))), id), vs2)

    (* Invariant:
     
       normalizeExp (U, s) = U'
       If   G |- s : G' and G' |- U : V 
       then U [s] = U'
       and  U' in existential normal form

       If (U, s) contain no existential variables,
       then U' in normal formal
    *)
  let rec normalizeExp us = normalizeExpW (whnf us)

  and normalizeExpW (u, s) =
    match (u, s) with
        (Uni (l), s) -> u
      | (Pi (dP, u), s) -> 
          Pi (normalizeDecP (dP, s), normalizeExp (u, dot1 s))
      | (Root (h, ss), s) -> (* s = id *)
	  Root (h, normalizeSpine (ss, s))
      | (Lam (d, u), s) -> 
	  Lam (normalizeDec (d, s), normalizeExp (u, dot1 s))
      | ((EVar _, s)) -> EClo (u, s)
      | (FgnExp (i,csfe) , s) ->
          FgnExpStd.Map.apply (i,csfe) (fun u -> normalizeExp (u, s))
      | ((AVar r ,s)) when Option.isSome (!r) -> 
	  normalizeExpW (Option.get (!r),s)
      | ((AVar _  ,s)) -> (print_string "Normalize  AVAR\n"; raise (Error ""))


  and normalizeSpine args =
    match args with
        (Nil, s) -> 
          Nil 
      | (App (u, ss), s) -> 
          App (normalizeExp (u, s), normalizeSpine (ss, s))
      | (SClo (ss, s'), s) ->
	  normalizeSpine (ss, comp (s', s))

  and normalizeDec args =
    match args with
        (Dec (xOpt, v), s) -> Dec (xOpt, normalizeExp (v, s))
      | (BDec (xOpt, (c, t)), s) -> 
           BDec (xOpt, (c, normalizeSub (comp (t, s))))
  and normalizeDecP ((d, p), s) = (normalizeDec (d, s), p)

    (* dead code -fp *)
    (* pre-Twelf 1.2 code walk Fri May  8 11:37:18 1998 *)
    (* not any more --cs Wed Jun 19 13:59:56 EDT 2002 *)
  and normalizeSub s =
    match s with
        (Shift _) -> s
      | (Dot (((Idx _) as ft), s)) ->
	  Dot (ft, normalizeSub (s))
      | (Dot (Exp u, s)) ->
	  (* changed to obtain pattern substitution if possible *)
	  (* Sat Dec  7 16:58:09 2002 -fp *)
	  (* Dot (Exp (normalizeExp (u, id)), normalizeSub s) *)
	  dotEta (Exp (normalizeExp (u, id)), normalizeSub s)


  let rec normalizeCtx arg =
    match arg with
        Null -> Null
      | (Decl (g, d)) -> 
          Decl (normalizeCtx g, normalizeDec (d, id))


    (* invert s = s'

       Invariant:
       If   G |- s : G'    (and s patsub)
       then G' |- s' : G
       s.t. s o s' = id  
    *)
  let invert s =
    let rec lookup args =
      match args with
	  (n, Shift _, p) -> None
	| (n, Dot (Undef, s'), p) -> lookup (n+1, s', p)
	| (n, Dot (Idx k, s'), p) -> 
	    if k = p then Some n 
	    else lookup (n+1, s', p)
    in
    let rec invert'' args =
      match args with
	  (0, si) -> si
	| (p, si) -> 
	    (match (lookup (1, s, p)) with
	         Some k -> invert'' (p-1, Dot (Idx k, si))
	       | None -> invert'' (p-1, Dot (Undef, si)))
    in	       
    let rec invert' args =
      match args with
	  (n, Shift p) -> invert'' (p, Shift n)
	| (n, Dot (_, s')) -> invert' (n+1, s')
    in
    invert' (0, s)
    
    
    (* strengthen (t, G) = G'

       Invariant:
       If   G'' |- t : G    (* and t strsub *)
       then G' |- t : G  and G' subcontext of G
    *)
  let rec strengthen args =
    match args with
        (Shift n (* = 0 *), Null) -> Null
      | (Dot (Idx k (* k = 1 *), t), Decl (g, d)) ->
	  let t' = comp (t, invShift) in
	  (* G |- D dec *)
          (* G' |- t' : G *)
	  (* G' |- D[t'] dec *)
          Decl (strengthen (t', g), decSub (d, t'))
      | (Dot (Undef, t), Decl (g, d)) -> 
          strengthen (t, g)
      | (Shift n, g) -> 
	  strengthen (Dot (Idx (n+1), Shift (n+1)), g)


    (* isId s = B
     
       Invariant:
       If   G |- s: G', s weakensub
       then B holds 
            iff s = id, G' = G
    *)
  let rec isId' args =
    match args with
	(Shift(k), k') -> (k = k')
      | (Dot (Idx(n), s'), k') ->
          n = k' && isId' (s', k'+1)
      | _ -> false
  let isId s = isId' (s, 0)

    (* cloInv (U, w) = U[w^-1]

       Invariant:
       If G |- U : V
          G |- w : G'  w weakening subst
          U[w^-1] defined (without pruning or constraints)

       then G' |- U[w^-1] : V[w^-1]
       Effects: None
    *)
  let cloInv (u, w) = EClo (u, invert w)

    (* cloInv (s, w) = s o w^-1

       Invariant:
       If G |- s : G1
          G |- w : G2  s weakening subst
          s o w^-1 defined (without pruning or constraints)

       then G2 |- s o w^-1 : G1
       Effects: None
    *)
  let compInv (s, w) = comp (s, invert w)

    (* functions previously in the Pattern functor *)
    (* eventually, they may need to be mutually recursive with whnf *)

    (* isPatSub s = B

       Invariant:
       If    G |- s : G' 
       and   s = n1 .. nm ^k
       then  B iff  n1, .., nm pairwise distinct
               and  ni <= k or ni = _ for all 1 <= i <= m
    *)
  let rec isPatSub arg =
    match arg with
        (Shift(k)) -> true
      | (Dot (Idx (n), s)) -> 
          let rec checkBVar arg =
	    (match arg with
	         (Shift(k)) -> (n <= k)
		| (Dot (Idx (n'), s)) -> 
	            n <> n' && checkBVar (s)
		| (Dot (Undef, s)) ->
		    checkBVar (s)
		| _ -> false)
	  in
	  checkBVar s && isPatSub s
      | (Dot (Undef, s)) -> isPatSub s
      | _ -> false
	(* Try harder, due to bug somewhere *)
	(* Sat Dec  7 17:05:02 2002 -fp *)
        (* false *)
      (* below does not work, because the patSub is lost *)
      (*
	  let let (U', s') = whnf (U, id)
	  in
	    isPatSub (Dot (Idx (etaContract' (U', s', 0)), s))
	    handle Eta -> false
	  end
      | isPatSub _ = false
      *)

    (* makePatSub s = Some(s') if s is convertible to a patSub
                      None otherwise

       Invariant:
       If    G |- s : G' 
       and   s = n1 .. nm ^k
       then  B iff  n1, .., nm pairwise distinct
               and  ni <= k or ni = _ for all 1 <= i <= m
    *)
  let rec mkPatSub s =
    match s with
        (Shift(k)) -> s
      | (Dot (Idx (n), s)) -> 
	  let s' = mkPatSub s in
	  let rec checkBVar arg =
	    (match arg with
	         (Shift(k)) -> (n <= k)
	       | (Dot (Idx (n'), s')) -> 
	           n <> n' && checkBVar (s')
	       | (Dot (Undef, s')) ->
	            checkBVar (s'))
	  in
	  let _ = checkBVar s' in
	  Dot (Idx (n), s')
      | (Dot (Undef, s)) -> Dot (Undef, mkPatSub s)
      | (Dot (Exp (u), s)) -> 
	  let (u', t') = whnf (u, id) in
	  let k = (etaContract' (u', t', 0)) in (* may raise Eta *)
	  Dot (Idx (k), mkPatSub s)
      | _ -> raise Eta

  let makePatSub (s) = try Some (mkPatSub (s))
		       with Eta -> None

  let etaContract = (fun u -> etaContract' (u, id, 0))

  let normalize = normalizeExp
