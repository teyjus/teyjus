  exception Error of string

    (*
       We write {{K}} for the context of K, where EVars, FVars, LVars have
       been translated to declarations and their occurrences to BVars.
       We write {{U}}_K, {{S}}_K for the corresponding translation of an
       expression or spine.

       Just like contexts G, any K is implicitly assumed to be
       well-formed and in dependency order.

       We write  K ||- U  if all EVars and FVars in U are collected in K.
       In particular, . ||- U means U contains no EVars or FVars.  Similarly,
       for spines K ||- S and other syntactic categories.

       Collection and abstraction raise Error if there are unresolved
       constraints after simplification.
    *)

    (* collectConstraints K = cnstrs
       where cnstrs collects all constraints attached to EVars in K
    *)
  let  collectConstraints c =
    match c with
        (IntSyn.Null) -> []
      | (IntSyn.Decl (G, FV _)) -> collectConstraints G
      | (IntSyn.Decl (G, EV (IntSyn.EVar (_, _, _, ref nil)))) -> collectConstraints G
      | (IntSyn.Decl (G, EV (IntSyn.EVar (_, _, _, ref cnstrL)))) ->
        (Constraints.simplify cnstrL) @ collectConstraints G
      | (IntSyn.Decl (G, LV _)) -> collectConstraints G

    (* checkConstraints (K) = ()
       Effect: raises Constraints.Error(C) if K contains unresolved constraints
    *)
  let  checkConstraints (K) =
    let constraints = collectConstraints K in
    let _ = match constraints with
	        [] -> ()
	      | _ ->> raise Constraints.Error (constraints)
    in
    ()

    (* checkEmpty Cnstr = ()
       raises Error exception if constraints Cnstr cannot be simplified
       to the empty constraint
    *)
    (*
    let  checkEmpty (nil) = ()
      | checkEmpty (Cnstr) =
        (case Constraints.simplify Cnstr
	   of nil => ()
	    | _ => raise Error "Typing ambiguous -- unresolved constraints")
    *)
    (* eqEVar X Y = B
       where B iff X and Y represent same variable
    *)
  let  eqEVar v1 v2 =
    match v1, v2 with
        (IntSyn.EVar (r1, _, _, _)), (EV (IntSyn.EVar (r2, _, _, _))) -> (r1 = r2)
      | _ -> false

    (* eqFVar F Y = B
       where B iff X and Y represent same variable
    *)
  let  eqFVar v1 v2 =
    match v1, v2 with
        (IntSyn.FVar (n1, _, _)), (FV (n2,  _)) -> (n1 = n2)
      | _ -> false

    (* eqLVar L Y = B
       where B iff X and Y represent same variable
    *)
  let  eqLVar v1 v2 =
    match v1, v2 with
        (IntSyn.LVar ((r1, _, _))), (LV (IntSyn.LVar ((r2, _, _)))) -> (r1 = r2)
      | _ -> false


    (* exists P K = B
       where B iff K = K1, Y, K2  s.t. P Y  holds
    *)
  let  exists P K =
    let  exists' arg =
      match arg with
          (IntSyn.Null) -> false
        | (IntSyn.Decl(K',Y)) -> P(Y) orelse exists' (K')
     in
     exists' K

    (* this should be non-strict *)
    (* perhaps the whole repeated traversal are now a performance
       bottleneck in PCC applications where logic programming search
       followed by abstraction creates certificates.  such certificates
       are large, so the quadratic algorithm is not really acceptable.
       possible improvement, collect, abstract, then traverse one more
       time to determine status of all variables.
    *)
    (* Wed Aug  6 16:37:57 2003 -fp *)
    (* !!! *)
  let  or args =
    match args with
        (IntSyn.Maybe, _) -> IntSyn.Maybe
      | (_, IntSyn.Maybe) -> IntSyn.Maybe
      | (IntSyn.Meta, _) -> IntSyn.Meta
      | (_, IntSyn.Meta) -> IntSyn.Meta
      | (IntSyn.No, IntSyn.No) -> IntSyn.No
      
    (* occursInExp (k, U) = DP, 

       Invariant:
       If    U in nf 
       then  DP = No      iff k does not occur in U
	     DP = Maybe   iff k occurs in U some place not as an argument to a Skonst
	     DP = Meta    iff k occurs in U and only as arguments to Skonsts
    *)
  let  occursInExp args 
    match args with
        (k, IntSyn.Uni _) -> IntSyn.No
      | (k, IntSyn.Pi (DP, V)) -> or (occursInDecP (k, DP), occursInExp (k+1, V))
      | (k, IntSyn.Root (H, S)) -> occursInHead (k, H, occursInSpine (k, S))
      | (k, IntSyn.Lam (D, V)) -> or (occursInDec (k, D), occursInExp (k+1, V))
      | (k, IntSyn.FgnExp csfe) =
	IntSyn.FgnExpStd.fold csfe (fn (U, DP) => or (DP, (occursInExp (k, Whnf.normalize (U, IntSyn.id))))) IntSyn.No

      (* no case for Redex, EVar, EClo *)

  and occursInHead args =
    match args with
        (k, IntSyn.BVar (k'), DP) -> 
          if (k = k') then IntSyn.Maybe
          else DP
      | (k, IntSyn.Const _, DP) -> DP
      | (k, IntSyn.Def _, DP) -> DP
      | (k, IntSyn.Proj _, DP) -> DP   
      | (k, IntSyn.FgnConst _, DP) -> DP
      | (k, IntSyn.Skonst _, IntSyn.No) -> IntSyn.No
      | (k, IntSyn.Skonst _, IntSyn.Meta) -> IntSyn.Meta
      | (k, IntSyn.Skonst _, IntSyn.Maybe) -> IntSyn.Meta
      (* no case for FVar *)

  and occursInSpine args =
    match args with
        (_, IntSyn.Nil) -> IntSyn.No
      | (k, IntSyn.App (U, S)) -> or (occursInExp (k, U), occursInSpine (k, S))
      (* no case for SClo *)

  and occursInDec (k, IntSyn.Dec (_, V)) = occursInExp (k, V)
  and occursInDecP (k, (D, _)) = occursInDec (k, D)

    (* piDepend ((D,P), V) = Pi ((D,P'), V)
       where P' = Maybe if D occurs in V, P' = No otherwise 
    *)
    (* optimize to have fewer traversals? -cs *)
    (* pre-Twelf 1.2 code walk Fri May  8 11:17:10 1998 *)
  let  piDepend dpv =
    match dpv with
        ((D, IntSyn.No), V) -> IntSyn.Pi dpv
      | ((D, IntSyn.Meta), V) -> IntSyn.Pi dpv
      | ((D, IntSyn.Maybe), V) -> 
	  IntSyn.Pi ((D, occursInExp (1, V)), V)
	
    (* raiseType (G, V) = {{G}} V

       Invariant:
       If G |- V : L
       then  . |- {{G}} V : L

       All abstractions are potentially dependent.
    *)
  let  raiseType args =
    match args with
        (IntSyn.Null, V) -> V
      | (IntSyn.Decl (G, D), V) -> raiseType (G, IntSyn.Pi ((D, IntSyn.Maybe), V))

    (* raiseTerm (G, U) = [[G]] U

       Invariant:
       If G |- U : V
       then  . |- [[G]] U : {{G}} V

       All abstractions are potentially dependent.
    *)
  let  raiseTerm args =
    match args with
        (IntSyn.Null, U) -> U
      | (IntSyn.Decl (G, D), U) -> raiseTerm (G, IntSyn.Lam (D, U))

    (* collectExpW (G, (U, s), K) = K'

       Invariant: 
       If    G |- s : G1     G1 |- U : V      (U,s) in whnf
       No circularities in U
             (enforced by extended occurs-check for FVars in Unify)
       and   K' = K, K''
	     where K'' contains all EVars and FVars in (U,s)
    *)
    (* Possible optimization: Calculate also the normal form of the term *)
  let  collectExpW args =
    match args with
        (G, (IntSyn.Uni L, s), K) -> K
      | (G, (IntSyn.Pi ((D, _), V), s), K) ->
          collectExp (IntSyn.Decl (G, IntSyn.decSub (D, s)), (V, IntSyn.dot1 s), collectDec (G, (D, s), K))
      | (G, (IntSyn.Root ((IntSyn.FVar (name, V, s') as F), S), s), K) ->
	  if exists (eqFVar F) K
	  then collectSpine (G, (S, s), K)
	  else (* s' = ^|G| *)
	    collectSpine (G, (S, s), IntSyn.Decl (collectExp (IntSyn.Null, (V, IntSyn.id), K), FV (name, V)))
      | (G, (IntSyn.Root (IntSyn.Proj ((IntSyn.LVar (ref NONE, sk, (l, t))) as L), i), S), s), K) -> 
          collectSpine (G, (S, s), collectBlock (G, IntSyn.blockSub (L, s), K))
	  (* BUG : We forget to deref L.  use collectBlock instead
	     FPCHECK
	     -cs Sat Jul 24 18:48:59 2010 
            was:
      | collectExpW (G, (IntSyn.Root (IntSyn.Proj (L as IntSyn.LVar (r, sk, (l, t)), i), S), s), K) -> 
	if exists (eqLVar L) K
	  (* note: don't collect t again below *)
	  (* was: collectSpine (G, (S, s), collectSub (IntSyn.Null, t, K)) *)
	  (* Sun Dec 16 10:54:52 2001 -fp !!! *)
	  then collectSpine (G, (S, s), K)
	else 
	  (* -fp Sun Dec  1 21:12:12 2002 *)
	(* collectSpine (G, (S, s), IntSyn.Decl (collectSub (G, IntSyn.comp(t,s), K), LV L)) *)
	(* was :
	 collectSpine (G, (S, s), collectSub (G, IntSyn.comp(t,s), IntSyn.Decl (K, LV L)))
	 July 22, 2010 -fp -cs
	 *)
	    collectSpine (G, (S, s), collectSub (G, IntSyn.comp(t,IntSyn.comp(sk,s)),
						 IntSyn.Decl (K, LV L)))
*)    | (G, (IntSyn.Root (_ , S), s), K) -> 
	  collectSpine (G, (S, s), K)
      | (G, (IntSyn.Lam (D, U), s), K) -> 
	  collectExp (IntSyn.Decl (G, IntSyn.decSub (D, s)), (U, IntSyn.dot1 s), collectDec (G, (D, s), K))
      | (G, (X as IntSyn.EVar (r, GX, V, cnstrs), s), K) ->
	  if exists (eqEVar X) K
	  then collectSub(G, s, K)
	  else 
	       (* let _ = checkEmpty !cnstrs *)
	    let V' = raiseType (GX, V) (* inefficient *) in
	    let K' = collectExp (IntSyn.Null, (V', IntSyn.id), K) in
	    collectSub(G, s, IntSyn.Decl (K', EV (X)))
      | (G, (IntSyn.FgnExp csfe, s), K) ->
	  IntSyn.FgnExpStd.fold csfe (fn (U, K) => collectExp (G, (U, s), K)) K
      (* No other cases can occur due to whnf invariant *)

    (* collectExp (G, (U, s), K) = K' 
       
       same as collectExpW  but  (U,s) need not to be in whnf 
    *) 
  and collectExp (G, Us, K) = collectExpW (G, Whnf.whnf Us, K)

    (* collectSpine (G, (S, s), K) = K' 

       Invariant: 
       If    G |- s : G1     G1 |- S : V > P
       then  K' = K, K''
       where K'' contains all EVars and FVars in (S, s)
     *)
  and collectSpine args =
    match args with
        (G, (IntSyn.Nil, _), K) -> K
      | (G, (IntSyn.SClo(S, s'), s), K) -> 
          collectSpine (G, (S, IntSyn.comp (s', s)), K)
      | (G, (IntSyn.App (U, S), s), K) ->
	  collectSpine (G, (S, s), collectExp (G, (U, s), K))

    (* collectDec (G, (x:V, s), K) = K'

       Invariant: 
       If    G |- s : G1     G1 |- V : L
       then  K' = K, K''
       where K'' contains all EVars and FVars in (V, s)
    *)
  and collectDec args =
    match args with
        (G, (IntSyn.Dec (_, V), s), K) ->
          collectExp (G, (V, s), K)
      | (G, (IntSyn.BDec (_, (_, t)), s), K) ->
	  (* . |- t : Gsome, so do not compose with s *)
	  (* Sat Dec  8 13:28:15 2001 -fp *)
	  (* was: collectSub (IntSyn.Null, t, K) *)
	  collectSub (G, IntSyn.comp(t,s), K)
      | (G, (IntSyn.NDec _, s), K) -> K

    (* collectSub (G, s, K) = K' 

       Invariant: 
       If    G |- s : G1    
       then  K' = K, K''
       where K'' contains all EVars and FVars in s
    *)
  and collectSub args =
    match args with
        (G, IntSyn.Shift _, K) -> K
      | (G, IntSyn.Dot (IntSyn.Idx _, s), K) -> collectSub (G, s, K)
      | (G, IntSyn.Dot (IntSyn.Exp (U), s), K) ->
	  collectSub (G, s, collectExp (G, (U, IntSyn.id), K))
      | (G, IntSyn.Dot (IntSyn.Block B, s), K) ->
	  collectSub (G, s, collectBlock (G, B, K))
    (* next case should be impossible *)
    (*
      | collectSub (G, IntSyn.Dot (IntSyn.Undef, s), K) =
          collectSub (G, s, K)
    *)

    (* collectBlock (G, B, K) where G |- B block *)
  and collectBlock args =
    match args with
        (G, IntSyn.LVar (ref (SOME B), sk , _), K) ->
          collectBlock (G, IntSyn.blockSub (B, sk), K)
          (* collectBlock (B, K) *)
          (* correct?? -fp Sun Dec  1 21:15:33 2002 *)
      | (G, (IntSyn.LVar (_, sk, (l, t)) as L), K) -> 
          if exists (eqLVar L) K
	  then collectSub (G, IntSyn.comp(t,sk), K)
	  else IntSyn.Decl (collectSub (G, IntSyn.comp(t,sk), K), LV L)
    (* was: t in the two lines above, July 22, 2010, -fp -cs *)
    (* | collectBlock (G, IntSyn.Bidx _, K) = K *)
    (* should be impossible: Fronts of substitutions are never Bidx *)
    (* Sat Dec  8 13:30:43 2001 -fp *)

    (* collectCtx (G0, G, K) = (G0', K')
       Invariant:
       If G0 |- G ctx,
       then G0' = G0,G
       and K' = K, K'' where K'' contains all EVars and FVars in G
    *)
  let  collectCtx args =
    match args with
        (G0, IntSyn.Null, K) -> (G0, K)
      | (G0, IntSyn.Decl (G, D), K) ->
	  let (G0', K') = collectCtx (G0, G, K) in
	  let K'' = collectDec (G0', (D, IntSyn.id), K') in
	  (IntSyn.Decl (G0, D), K'')
	
    (* collectCtxs (G0, Gs, K) = K'
       Invariant: G0 |- G1,...,Gn ctx where Gs = [G1,...,Gn]
       and K' = K, K'' where K'' contains all EVars and FVars in G1,...,Gn
    *)
  let  collectCtxs args =
    match args with
        (G0, nil, K) -> K
      | (G0, G::Gs, K) ->
	  let (G0', K') = collectCtx (G0, G, K) in
	  collectCtxs (G0', Gs, K')
	
    (* abstractEVar (K, depth, X) = C'
     
       Invariant:
       If   G |- X : V
       and  |G| = depth
       and  X occurs in K  at kth position (starting at 1)
       then C' = BVar (depth + k)
       and  {{K}}, G |- C' : V
    *)
  let  abstractEVar args =
    match args with
        (IntSyn.Decl (K', EV (IntSyn.EVar(r',_,_,_))), depth, (IntSyn.EVar (r, _, _, _) as X)) -> 
          if r = r' then IntSyn.BVar (depth+1)
	  else abstractEVar (K', depth+1, X)
(*      | abstractEVar (IntSyn.Decl (K', FV (n', _)), depth, X) -> 
	  abstractEVar (K', depth+1, X) remove later --cs*)
      | (IntSyn.Decl (K', _), depth, X) -> 
	  abstractEVar (K', depth+1, X)

    (* abstractFVar (K, depth, F) = C'
     
       Invariant:
       If   G |- F : V
       and  |G| = depth
       and  F occurs in K  at kth position (starting at 1)
       then C' = BVar (depth + k)
       and  {{K}}, G |- C' : V
    *)
  let  abstractFVar args =
    match args with
        (IntSyn.Decl(K', FV (n', _)), depth, (IntSyn.FVar (n, _, _) as F)) -> 
  	  if n = n' then IntSyn.BVar (depth+1)
	  else abstractFVar (K', depth+1, F)
(*      | abstractFVar (IntSyn.Decl(K', EV _), depth, F) =
  	  abstractFVar (K', depth+1, F) remove later --cs *)
      | (IntSyn.Decl(K', _), depth, F) ->
  	  abstractFVar (K', depth+1, F)
       
    (* abstractLVar (K, depth, L) = C'
     
       Invariant:
       If   G |- L : V
       and  |G| = depth
       and  L occurs in K  at kth position (starting at 1)
       then C' = Bidx (depth + k)
       and  {{K}}, G |- C' : V
    *)
  let  abstractLVar args =
    match args with
        (IntSyn.Decl(K', LV (IntSyn.LVar (r', _, _))), depth, ( IntSyn.LVar (r, _, _) as L)) -> 
	  if r = r' then IntSyn.Bidx (depth+1)
	  else abstractLVar (K', depth+1, L)
      | (IntSyn.Decl(K', _), depth, L) ->
  	  abstractLVar (K', depth+1, L)
      
    (* abstractExpW (K, depth, (U, s)) = U'
       U' = {{U[s]}}_K

       Invariant:
       If    G |- s : G1     G1 |- U : V    (U,s) is in whnf
       and   K is internal context in dependency order
       and   |G| = depth
       and   K ||- U and K ||- V
       then  {{K}}, G |- U' : V'
       and   . ||- U' and . ||- V'
       and   U' is in nf 
    *)
  let  abstractExpW args =
    match args with
        (K, depth, (U as IntSyn.Uni (L), s)) -> U
      | (K, depth, (IntSyn.Pi ((D, P), V), s)) ->
          piDepend ((abstractDec (K, depth, (D, s)), P), 
		    abstractExp (K, depth + 1, (V, IntSyn.dot1 s)))
      | (K, depth, (IntSyn.Root (F as IntSyn.FVar _, S), s)) ->
	  IntSyn.Root (abstractFVar (K, depth, F), 
		  abstractSpine (K, depth, (S, s)))
      | (K, depth, (IntSyn.Root (IntSyn.Proj (L as IntSyn.LVar _, i), S), s)) ->
	  IntSyn.Root (IntSyn.Proj (abstractLVar (K, depth, L), i),  
		  abstractSpine (K, depth, (S, s)))
      | (K, depth, (IntSyn.Root (H, S) ,s)) ->
	  IntSyn.Root (H, abstractSpine (K, depth, (S, s)))   
      | (K, depth, (IntSyn.Lam (D, U), s)) ->
          IntSyn.Lam (abstractDec (K, depth, (D, s)),
		 abstractExp (K, depth + 1, (U, IntSyn.dot1 s)))
      | (K, depth, (X as IntSyn.EVar _, s)) ->
 	  IntSyn.Root (abstractEVar (K, depth, X), 
		  abstractSub (K, depth, s, IntSyn.Nil))
      | (K, depth, (IntSyn.FgnExp csfe, s)) ->
          IntSyn.FgnExpStd.Map.apply csfe (fun U -> abstractExp (K, depth, (U, s)))

    (* abstractExp (K, depth, (U, s)) = U'
     
       same as abstractExpW, but (U,s) need not to be in whnf 
    *)
  and abstractExp (K, depth, Us) = abstractExpW (K, depth, Whnf.whnf Us)

    (* abstractSub (K, depth, s, S) = S'      (implicit raising)
       S' = {{s}}_K @@ S

       Invariant:
       If   G |- s : G1   
       and  |G| = depth
       and  K ||- s
       then {{K}}, G |- S' : {G1}.W > W   (for some W)
       and  . ||- S'
    *)
  and abstractSub args =
    match args with
        (K, depth, IntSyn.Shift (k), S) -> 
	  if k < depth
	  then abstractSub (K, depth, IntSyn.Dot (IntSyn.Idx (k+1), IntSyn.Shift (k+1)), S)
	  else (* k = depth *) S
      | (K, depth, IntSyn.Dot (IntSyn.Idx (k), s), S) ->
	  abstractSub (K, depth, s, IntSyn.App (IntSyn.Root (IntSyn.BVar (k), IntSyn.Nil), S))
      | (K, depth, IntSyn.Dot (IntSyn.Exp (U), s), S) ->
	  abstractSub (K, depth, s, IntSyn.App (abstractExp (K, depth, (U, IntSyn.id)), S))
 
    (* abstractSpine (K, depth, (S, s)) = S'
       where S' = {{S[s]}}_K

       Invariant:
       If   G |- s : G1     G1 |- S : V > P 
       and  K ||- S
       and  |G| = depth

       then {{K}}, G |- S' : V' > P'
       and  . ||- S'
    *)
  and abstractSpine args =
    match args with
        (K, depth, (IntSyn.Nil, _))  -> IntSyn.Nil 
      | (K, depth, (IntSyn.SClo (S, s'), s)) -> 
	  abstractSpine (K, depth, (S, IntSyn.comp (s', s)))
      | (K, depth, (IntSyn.App (U, S), s)) ->
	  IntSyn.App (abstractExp (K, depth, (U ,s)), 
		 abstractSpine (K, depth, (S, s)))

    (* abstractDec (K, depth, (x:V, s)) = x:V'
       where V = {{V[s]}}_K

       Invariant:
       If   G |- s : G1     G1 |- V : L
       and  K ||- V
       and  |G| = depth

       then {{K}}, G |- V' : L
       and  . ||- V'
    *)
  and abstractDec (K, depth, (IntSyn.Dec (x, V), s)) =
	  IntSyn.Dec (x, abstractExp (K, depth, (V, s)))

    (* abstractSOME (K, s) = s'
       s' = {{s}}_K

       Invariant:
       If    . |- s : Gsome    
       and   K is internal context in dependency order
       and   K ||- s
       then  {{K}} |- s' : Gsome  --- not changing domain of s'

       Update: modified for globality invariant of . |- t : Gsome
       Sat Dec  8 13:35:55 2001 -fp
       Above is now incorrect
       Sun Dec  1 22:36:50 2002 -fp
    *)
  let  abstractSOME args =
    match args with
        (K, IntSyn.Shift 0) -> (* n = 0 by invariant, check for now *)
          IntSyn.Shift (IntSyn.ctxLength(K))
      | (K, IntSyn.Shift (n)) -> (* n > 0 *)
	  IntSyn.Shift (IntSyn.ctxLength(K))
      | (K, IntSyn.Dot (IntSyn.Idx k, s)) -> 
          IntSyn.Dot (IntSyn.Idx k, abstractSOME (K, s))
      | (K, IntSyn.Dot (IntSyn.Exp U, s)) ->
	  IntSyn.Dot (IntSyn.Exp (abstractExp (K, 0, (U, IntSyn.id))), abstractSOME (K, s))
      | (K, IntSyn.Dot (IntSyn.Block (L as IntSyn.LVar _), s)) ->
	  IntSyn.Dot (IntSyn.Block (abstractLVar (K, 0, L)), abstractSOME (K, s))
      (* IntSyn.Block (IntSyn.Bidx _) should be impossible as head of substitutions *)

    (* abstractCtx (K, depth, G) = (G', depth')
       where G' = {{G}}_K

       Invariants:
       If G0 |- G ctx
       and K ||- G
       and |G0| = depth
       then {{K}}, G0 |- G' ctx
       and . ||- G'
       and |G0,G| = depth'
    *)
  let  abstractCtx args =
    match args with
        (K, depth, IntSyn.Null) -> (IntSyn.Null, depth)
      | (K, depth, IntSyn.Decl (G, D)) ->
	  let (G', depth') = abstractCtx (K, depth, G) in
	  let D' = abstractDec (K, depth', (D, IntSyn.id)) in
	  (IntSyn.Decl (G', D'), depth'+1)

    (* abstractCtxlist (K, depth, [G1,...,Gn]) = [G1',...,Gn']
       where Gi' = {{Gi}}_K

       Invariants:
       if G0 |- G1,...,Gn ctx 
       and K ||- G1,...,Gn
       and |G0| = depth
       then {{K}}, G0 |- G1',...,Gn' ctx
       and . ||- G1',...,Gn'
    *)
  let  abstractCtxlist args =
    match args with
        (K, depth, nil) -> nil
      | (K, depth, G::Gs) ->
	  let (G', depth') = abstractCtx (K, depth, G) in
	  let Gs' = abstractCtxlist (K, depth', Gs) in
	  G'::Gs'

    (* dead code under new reconstruction -kw
    (* getlevel (V) = L if G |- V : L

       Invariant: G |- V : L' for some L'
    *)
    let  getLevel (IntSyn.Uni _) = IntSyn.Kind 
      | getLevel (IntSyn.Pi (_, U)) = getLevel U
      | getLevel (IntSyn.Root _)  = IntSyn.Type
      | getLevel (IntSyn.Redex (U, _)) = getLevel U
      | getLevel (IntSyn.Lam (_, U)) = getLevel U
      | getLevel (IntSyn.EClo (U,_)) = getLevel U

    (* checkType (V) = () if G |- V : type

       Invariant: G |- V : L' for some L'
    *)
    let  checkType V = 
        (case getLevel V
	   of IntSyn.Type => ()
	    | _ => raise Error "Typing ambiguous -- free type variable")
    *)

    (* abstractKPi (K, V) = V'
       where V' = {{K}} V

       Invariant: 
       If   {{K}} |- V : L 
       and  . ||- V

       then V' = {{K}} V
       and  . |- V' : L
       and  . ||- V'
    *)
  let  abstractKPi args =
    match args with
        (IntSyn.Null, V) -> V
      | (IntSyn.Decl (K', EV (IntSyn.EVar (_, GX, VX, _))), V) ->
          let V' = raiseType (GX, VX) in
	  let V'' = abstractExp (K', 0, (V', IntSyn.id)) in
          (* enforced by reconstruction -kw
	  let _ = checkType V''	*)
	  abstractKPi (K', IntSyn.Pi ((IntSyn.Dec(NONE, V''), IntSyn.Maybe), V))
      | (IntSyn.Decl (K', FV (name,V')), V) ->
	  let V'' = abstractExp (K', 0, (V', IntSyn.id)) in
          (* enforced by reconstruction -kw
	  let _ = checkType V'' *)
	  abstractKPi (K', IntSyn.Pi ((IntSyn.Dec(SOME(name), V''), IntSyn.Maybe), V))
      | (IntSyn.Decl (K', LV (IntSyn.LVar (r, _, (l, t)))), V) ->
	  let t' = abstractSOME (K', t)	  in
	  abstractKPi (K', IntSyn.Pi ((IntSyn.BDec (NONE, (l, t')), IntSyn.Maybe), V))

    (* abstractKLam (K, U) = U'
       where U' = [[K]] U

       Invariant: 
       If   {{K}} |- U : V 
       and  . ||- U
       and  . ||- V

       then U' = [[K]] U
       and  . |- U' : {{K}} V
       and  . ||- U'
    *)
  let  abstractKLam args =
    match args with
        (IntSyn.Null, U) -> U
      | (IntSyn.Decl (K', EV (IntSyn.EVar (_, GX, VX, _))), U) ->
	  let V' = raiseType (GX, VX) in
          abstractKLam (K', IntSyn.Lam (IntSyn.Dec(NONE, abstractExp (K', 0, (V', IntSyn.id))), U))
      | (IntSyn.Decl (K', FV (name,V')), U) ->
 	  abstractKLam (K', IntSyn.Lam (IntSyn.Dec(SOME(name), abstractExp (K', 0, (V', IntSyn.id))), U))


  let  abstractKCtx arg =
    match arg with
        (IntSyn.Null) -> IntSyn.Null
      | (IntSyn.Decl (K', EV (IntSyn.EVar (_, GX, VX, _)))) ->
	  let V' = raiseType (GX, VX) in
	  let V'' = abstractExp (K', 0, (V', IntSyn.id)) in
          (* enforced by reconstruction -kw
	  let _ = checkType V''	*)
	  IntSyn.Decl (abstractKCtx K', IntSyn.Dec (NONE, V''))
      | (IntSyn.Decl (K', FV (name, V'))) ->
	  let V'' = abstractExp (K', 0, (V', IntSyn.id)) in
          (* enforced by reconstruction -kw
	  let _ = checkType V'' *)
	  IntSyn.Decl (abstractKCtx K', IntSyn.Dec (SOME(name), V''))
      | (IntSyn.Decl (K', LV (IntSyn.LVar (r, _, (l, t))))) ->
	  let t' = abstractSOME (K', t)	   in
	  IntSyn.Decl (abstractKCtx K', IntSyn.BDec (NONE, (l, t')))


    (* abstractDecImp V = (k', V')   (* rename --cs  (see above) *)

       Invariant: 
       If    . |- V : L
       and   K ||- V

       then  . |- V' : L
       and   V' = {{K}} V
       and   . ||- V'
       and   k' = |K|
    *)
  let  abstractDecImp V =
    let K = collectExp (IntSyn.Null, (V, IntSyn.id), IntSyn.Null) in
    let _ = checkConstraints (K) in
    (IntSyn.ctxLength K, abstractKPi (K, abstractExp (K, 0, (V, IntSyn.id))))

    (* abstractDef  (U, V) = (k', (U', V'))

       Invariant: 
       If    . |- V : L
       and   . |- U : V
       and   K1 ||- V
       and   K2 ||- U
       and   K = K1, K2

       then  . |- V' : L
       and   V' = {{K}} V
       and   . |- U' : V'
       and   U' = [[K]] U
       and   . ||- V'
       and   . ||- U'
       and   k' = |K|
    *)
  let  abstractDef (U, V) =
    let K = collectExp (IntSyn.Null, (U, IntSyn.id), collectExp (IntSyn.Null, (V, IntSyn.id), IntSyn.Null)) in
    let _ = checkConstraints K in
    (IntSyn.ctxLength K, (abstractKLam (K, abstractExp (K, 0, (U, IntSyn.id))), 
			   abstractKPi  (K, abstractExp (K, 0, (V, IntSyn.id)))))


  let  abstractSpineExt (S, s) =
    let K = collectSpine (IntSyn.Null, (S, s), IntSyn.Null) in
    let _ = checkConstraints (K) in
    let G = abstractKCtx (K) in
    let S = abstractSpine (K, 0, (S, s)) in
    (G, S)
	      
    (* abstractCtxs [G1,...,Gn] = G0, [G1',...,Gn']
       Invariants:
       If . |- G1,...,Gn ctx
          K ||- G1,...,Gn for some K
       then G0 |- G1',...,Gn' ctx for G0 = {{K}}
       and G1',...,Gn' nf
       and . ||- G1',...,Gn' ctx
    *)
  let  abstractCtxs (Gs) =
    let K = collectCtxs (IntSyn.Null, G, IntSyn.Null) in
    let _ = checkConstraints K in
    (abstractKCtx (K), abstractCtxlist (K, 0, Gs))

    (* closedDec (G, D) = true iff D contains no EVar or FVar *)
  let  closedDec (G, (IntSyn.Dec (_, V), s)) = 
      match collectExp (G, (V, s), IntSyn.Null) with
	  IntSyn.Null -> true
         | _ -> false

  let  closedSub args =
    match args with
        (G, IntSyn.Shift _) -> true
      | (G, IntSyn.Dot (IntSyn.Idx _, s)) -> closedSub (G, s)
      | (G, IntSyn.Dot (IntSyn.Exp U, s)) -> 
        (match collectExp (G, (U, IntSyn.id), IntSyn.Null) with
	     IntSyn.Null -> closedSub (G, s)
            | _ -> false)

  let  closedExp (G, (U, s)) = 
      match collectExp (G, (U, IntSyn.id), IntSyn.Null)
	  IntSyn.Null -> true
         | _ -> false

  let  closedCtx arg =
    match arg with
        IntSyn.Null -> true
      | (IntSyn.Decl (G, D)) ->
          closedCtx G andalso closedDec (G, (D, IntSyn.id))

  let  evarsToK l =
    match l with
        [] -> IntSyn.Null
      | (X::Xs) -> IntSyn.Decl (evarsToK (Xs), EV(X))

  let  KToEVars arg =
    match arg with
        (IntSyn.Null) -> []
      | (IntSyn.Decl (K, EV(X))) -> X::KToEVars (K)
      | (IntSyn.Decl (K, _)) -> KToEVars (K)

    (* collectEVars (G, U[s], Xs) = Xs'
       Invariants:
         G |- U[s] : V
         Xs' extends Xs by new EVars in U[s]
    *)
  let  collectEVars (G, Us, Xs) =
          KToEVars (collectExp (G, Us, evarsToK (Xs)))

  let  collectEVarsSpine (G, (S, s), Xs) =
          KToEVars (collectSpine (G, (S, s), evarsToK (Xs)))


  in
    let raiseType = raiseType
    let raiseTerm = raiseTerm

    let piDepend = piDepend
    let closedDec = closedDec
    let closedSub = closedSub
    let closedExp = closedExp 

    let abstractDecImp = abstractDecImp
    let abstractDef = abstractDef
    let abstractCtxs = abstractCtxs
    let abstractSpine = abstractSpineExt

    let collectEVars = collectEVars
    let collectEVarsSpine = collectEVarsSpine

    let closedCtx = closedCtx
  end
