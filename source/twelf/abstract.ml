  exception Error of string

    (* Intermediate Data Structure *)

    type eFLVar =
      EV of IntSyn.exp			(* Y ::= X         for  GX |- X : VX *)
    | FV of string * IntSyn.exp		(*     | (F, V)        if . |- F : V *)
    | LV of IntSyn.block                     (*     | L             if . |- L in W *) 
(*    | PV of T.Prg                       (*     | P                            *) *)


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
  let rec collectConstraints c =
    match c with
        (IntSyn.Null) -> []
      | (IntSyn.Decl (g, FV _)) -> collectConstraints g
      | (IntSyn.Decl (g, EV (IntSyn.EVar (_, _, _, r)))) when r = ref [] -> collectConstraints g
      | (IntSyn.Decl (g, EV (IntSyn.EVar (_, _, _, r)))) ->
          let cnstrL = !r in
          (Constraints.simplify cnstrL) @ collectConstraints g
      | (IntSyn.Decl (g, LV _)) -> collectConstraints g

    (* checkConstraints (K) = ()
       Effect: raises Constraints.Error(C) if K contains unresolved constraints
    *)
  let  checkConstraints (k) =
    let constraints = collectConstraints k in
    let _ = match constraints with
	        [] -> ()
	      | _ -> raise (Constraints.Error (constraints))
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
  let  exists p k =
    let rec exists' arg =
      match arg with
          (IntSyn.Null) -> false
        | (IntSyn.Decl(k',y)) -> p(y) || exists' (k')
     in
     exists' k

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
  let  or' args =
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
  let rec occursInExp args =
    match args with
        (k, IntSyn.Uni _) -> IntSyn.No
      | (k, IntSyn.Pi (dP, v)) -> or' (occursInDecP (k, dP), occursInExp (k+1, v))
      | (k, IntSyn.Root (h, s)) -> occursInHead (k, h, occursInSpine (k, s))
      | (k, IntSyn.Lam (d, v)) -> or' (occursInDec (k, d), occursInExp (k+1, v))
      | (k, IntSyn.FgnExp (i,f)) ->
          let csfe = (i,f) in
  	  IntSyn.FgnExpStd.fold csfe (fun (u, dP) -> or' (dP, (occursInExp (k, Whnf.normalize (u, IntSyn.id))))) IntSyn.No

      (* no case for Redex, EVar, EClo *)

  and occursInHead args =
    match args with
        (k, IntSyn.BVar (k'), dP) -> 
          if (k = k') then IntSyn.Maybe
          else dP
      | (k, IntSyn.Const _, dP) -> dP
      | (k, IntSyn.Def _, dP) -> dP
      | (k, IntSyn.Proj _, dP) -> dP   
      | (k, IntSyn.FgnConst _, dP) -> dP
      | (k, IntSyn.Skonst _, IntSyn.No) -> IntSyn.No
      | (k, IntSyn.Skonst _, IntSyn.Meta) -> IntSyn.Meta
      | (k, IntSyn.Skonst _, IntSyn.Maybe) -> IntSyn.Meta
      (* no case for FVar *)

  and occursInSpine args =
    match args with
        (_, IntSyn.Nil) -> IntSyn.No
      | (k, IntSyn.App (u, s)) -> or' (occursInExp (k, u), occursInSpine (k, s))
      (* no case for SClo *)

  and occursInDec (k, IntSyn.Dec (_, v)) = occursInExp (k, v)
  and occursInDecP (k, (d, _)) = occursInDec (k, d)

    (* piDepend ((D,P), V) = Pi ((D,P'), V)
       where P' = Maybe if D occurs in V, P' = No otherwise 
    *)
    (* optimize to have fewer traversals? -cs *)
    (* pre-Twelf 1.2 code walk Fri May  8 11:17:10 1998 *)
  let  piDepend dpv =
    match dpv with
        ((d, IntSyn.No) as a, v) 
      | ((d, IntSyn.Meta) as a, v) -> IntSyn.Pi (a, v)
      | ((d, IntSyn.Maybe), v) -> 
	  IntSyn.Pi ((d, occursInExp (1, v)), v)
	
    (* raiseType (g, V) = {{g}} V

       Invariant:
       If g |- V : L
       then  . |- {{g}} V : L

       All abstractions are potentially dependent.
    *)
  let rec raiseType args =
    match args with
        (IntSyn.Null, v) -> v
      | (IntSyn.Decl (g, d), v) -> raiseType (g, IntSyn.Pi ((d, IntSyn.Maybe), v))

    (* raiseTerm (G, U) = [[G]] U

       Invariant:
       If G |- U : V
       then  . |- [[G]] U : {{G}} V

       All abstractions are potentially dependent.
    *)
  let rec raiseTerm args =
    match args with
        (IntSyn.Null, u) -> u
      | (IntSyn.Decl (g, d), u) -> raiseTerm (g, IntSyn.Lam (d, u))

    (* collectExpW (G, (U, s), K) = K'

       Invariant: 
       If    G |- s : G1     G1 |- U : V      (U,s) in whnf
       No circularities in U
             (enforced by extended occurs-check for FVars in Unify)
       and   K' = K, K''
	     where K'' contains all EVars and FVars in (U,s)
    *)
    (* Possible optimization: Calculate also the normal form of the term *)
  let rec collectExpW args =
    match args with
        (g, (IntSyn.Uni l, s), k) -> k
      | (g, (IntSyn.Pi ((d, _), v), s), k) ->
          collectExp (IntSyn.Decl (g, IntSyn.decSub (d, s)), (v, IntSyn.dot1 s), collectDec (g, (d, s), k))
      | (g, (IntSyn.Root ((IntSyn.FVar (name, v, s') as f), ss), s), k) ->
	  (if exists (eqFVar f) k
	  then collectSpine (g, (ss, s), k)
	  else (* s' = ^|g| *)
	    collectSpine (g, (ss, s), IntSyn.Decl (collectExp (IntSyn.Null, (v, IntSyn.id), k), FV (name, v))))
      | (g, (IntSyn.Root (IntSyn.Proj (((IntSyn.LVar (r, sk, (l, t))) as ll), i), ss), s), k)  when r = ref None -> 
          collectSpine (g, (ss, s), collectBlock (g, IntSyn.blockSub (ll, s), k))
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
*)    | (g, (IntSyn.Root (_ , ss), s), k) -> 
	  collectSpine (g, (ss, s), k)
      | (g, (IntSyn.Lam (d, u), s), k) -> 
	  collectExp (IntSyn.Decl (g, IntSyn.decSub (d, s)), (u, IntSyn.dot1 s), collectDec (g, (d, s), k))
      | (g, (((IntSyn.EVar (r, gx, v, cnstrs)) as x), s), k) ->
	  if exists (eqEVar x) k
	  then collectSub(g, s, k)
	  else 
	       (* let _ = checkEmpty !cnstrs *)
	    let v' = raiseType (gx, v) (* inefficient *) in
	    let k' = collectExp (IntSyn.Null, (v', IntSyn.id), k) in
	    collectSub(g, s, IntSyn.Decl (k', EV (x)))
      | (g, (IntSyn.FgnExp (i,f), s), k) ->
          let csfe = (i,f) in
	  IntSyn.FgnExpStd.fold csfe (fun (u, k) -> collectExp (g, (u, s), k)) k
      (* No other cases can occur due to whnf invariant *)

    (* collectExp (G, (U, s), K) = K' 
       
       same as collectExpW  but  (U,s) need not to be in whnf 
    *) 
  and collectExp (g, us, k) = collectExpW (g, Whnf.whnf us, k)

    (* collectSpine (G, (S, s), K) = K' 

       Invariant: 
       If    G |- s : G1     G1 |- S : V > P
       then  K' = K, K''
       where K'' contains all EVars and FVars in (S, s)
     *)
  and collectSpine args =
    match args with
        (g, (IntSyn.Nil, _), k) -> k
      | (g, (IntSyn.SClo(ss, s'), s), k) -> 
          collectSpine (g, (ss, IntSyn.comp (s', s)), k)
      | (g, (IntSyn.App (u, ss), s), k) ->
	  collectSpine (g, (ss, s), collectExp (g, (u, s), k))

    (* collectDec (G, (x:V, s), K) = K'

       Invariant: 
       If    G |- s : G1     G1 |- V : L
       then  K' = K, K''
       where K'' contains all EVars and FVars in (V, s)
    *)
  and collectDec args =
    match args with
        (g, (IntSyn.Dec (_, v), s), k) ->
          collectExp (g, (v, s), k)
      | (g, (IntSyn.BDec (_, (_, t)), s), k) ->
	  (* . |- t : gsome, so do not compose with s *)
	  (* Sat Dec  8 13:28:15 2001 -fp *)
	  (* was: collectSub (IntSyn.Null, t, k) *)
	  collectSub (g, IntSyn.comp(t,s), k)
      | (g, (IntSyn.NDec _, s), k) -> k

    (* collectSub (G, s, K) = K' 

       Invariant: 
       If    G |- s : G1    
       then  K' = K, K''
       where K'' contains all EVars and FVars in s
    *)
  and collectSub args =
    match args with
        (g, IntSyn.Shift _, k) -> k
      | (g, IntSyn.Dot (IntSyn.Idx _, s), k) -> collectSub (g, s, k)
      | (g, IntSyn.Dot (IntSyn.Exp (u), s), k) ->
	  collectSub (g, s, collectExp (g, (u, IntSyn.id), k))
      | (g, IntSyn.Dot (IntSyn.Block b, s), k) ->
	  collectSub (g, s, collectBlock (g, b, k))
    (* next case should be impossible *)
    (*
      | collectSub (G, IntSyn.Dot (IntSyn.Undef, s), K) =
          collectSub (G, s, K)
    *)

    (* collectBlock (G, B, K) where G |- B block *)
  and collectBlock args =
    match args with
        (g, IntSyn.LVar (r, sk , _), k) when Option.isSome (!r) ->
          collectBlock (g, IntSyn.blockSub (Option.get (!r), sk), k)
          (* collectBlock (B, K) *)
          (* correct?? -fp Sun Dec  1 21:15:33 2002 *)
      | (g, (IntSyn.LVar (_, sk, (l, t)) as ll), k) -> 
          if exists (eqLVar ll) k
	  then collectSub (g, IntSyn.comp(t,sk), k)
	  else IntSyn.Decl (collectSub (g, IntSyn.comp(t,sk), k), LV ll)
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
  let rec collectCtx args =
    match args with
        (g0, IntSyn.Null, k) -> (g0, k)
      | (g0, IntSyn.Decl (g, d), k) ->
	  let (g0', k') = collectCtx (g0, g, k) in
	  let k'' = collectDec (g0', (d, IntSyn.id), k') in
	  (IntSyn.Decl (g0, d), k'')
	
    (* collectCtxs (g0, gs, K) = K'
       Invariant: G0 |- G1,...,Gn ctx where Gs = [G1,...,Gn]
       and K' = K, K'' where K'' contains all EVars and FVars in G1,...,Gn
    *)
  let rec collectCtxs args =
    match args with
        (g0, [], k) -> k
      | (g0, g::gs, k) ->
	  let (g0', k') = collectCtx (g0, g, k) in
	  collectCtxs (g0', gs, k')
	
    (* abstractEVar (K, depth, X) = C'
     
       Invariant:
       If   G |- X : V
       and  |G| = depth
       and  X occurs in K  at kth position (starting at 1)
       then C' = BVar (depth + k)
       and  {{K}}, G |- C' : V
    *)
  let rec abstractEVar args =
    match args with
        (IntSyn.Decl (k', EV (IntSyn.EVar(r',_,_,_))), depth, (IntSyn.EVar (r, _, _, _) as x)) -> 
          if r = r' then IntSyn.BVar (depth+1)
	  else abstractEVar (k', depth+1, x)
(*      | abstractEVar (IntSyn.Decl (K', FV (n', _)), depth, x) -> 
	  abstractEVar (K', depth+1, x) remove later --cs*)
      | (IntSyn.Decl (k', _), depth, x) -> 
	  abstractEVar (k', depth+1, x)

    (* abstractFVar (K, depth, F) = C'
     
       Invariant:
       If   G |- F : V
       and  |G| = depth
       and  F occurs in K  at kth position (starting at 1)
       then C' = BVar (depth + k)
       and  {{K}}, G |- C' : V
    *)
  let rec abstractFVar args =
    match args with
        (IntSyn.Decl(k', FV (n', _)), depth, (IntSyn.FVar (n, _, _) as f)) -> 
  	  if n = n' then IntSyn.BVar (depth+1)
	  else abstractFVar (k', depth+1, f)
(*      | abstractFVar (IntSyn.Decl(K', EV _), depth, f) =
  	  abstractFVar (K', depth+1, f) remove later --cs *)
      | (IntSyn.Decl(k', _), depth, f) ->
  	  abstractFVar (k', depth+1, f)
       
    (* abstractLVar (K, depth, L) = C'
     
       Invariant:
       If   G |- L : V
       and  |G| = depth
       and  L occurs in K  at kth position (starting at 1)
       then C' = Bidx (depth + k)
       and  {{K}}, G |- C' : V
    *)
  let rec abstractLVar args =
    match args with
        (IntSyn.Decl(k', LV (IntSyn.LVar (r', _, _))), depth, ( IntSyn.LVar (r, _, _) as l)) -> 
	  if r = r' then IntSyn.Bidx (depth+1)
	  else abstractLVar (k', depth+1, l)
      | (IntSyn.Decl(k', _), depth, l) ->
  	  abstractLVar (k', depth+1, l)
      
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
  let rec abstractExpW args =
    match args with
        (k, depth, (((IntSyn.Uni (l)) as u), s)) -> u
      | (k, depth, (IntSyn.Pi ((d, p), v), s)) ->
          piDepend ((abstractDec (k, depth, (d, s)), p), 
		    abstractExp (k, depth + 1, (v, IntSyn.dot1 s)))
      | (k, depth, (IntSyn.Root (((IntSyn.FVar _) as f), ss), s)) ->
	  IntSyn.Root (abstractFVar (k, depth, f), 
		  abstractSpine (k, depth, (ss, s)))
      | (k, depth, (IntSyn.Root (IntSyn.Proj (((IntSyn.LVar _) as l), i), ss), s)) ->
	  IntSyn.Root (IntSyn.Proj (abstractLVar (k, depth, l), i),  
		  abstractSpine (k, depth, (ss, s)))
      | (k, depth, (IntSyn.Root (h, ss) ,s)) ->
	  IntSyn.Root (h, abstractSpine (k, depth, (ss, s)))   
      | (k, depth, (IntSyn.Lam (d, u), s)) ->
          IntSyn.Lam (abstractDec (k, depth, (d, s)),
		 abstractExp (k, depth + 1, (u, IntSyn.dot1 s)))
      | (k, depth, (((IntSyn.EVar _) as x), s)) ->
 	  IntSyn.Root (abstractEVar (k, depth, x), 
		  abstractSub (k, depth, s, IntSyn.Nil))
      | (k, depth, (IntSyn.FgnExp (i,f), s)) ->
          let csfe = (i,f) in
          IntSyn.FgnExpStd.Map.apply csfe (fun u -> abstractExp (k, depth, (u, s)))

    (* abstractExp (K, depth, (U, s)) = U'
     
       same as abstractExpW, but (U,s) need not to be in whnf 
    *)
  and abstractExp (k, depth, us) = abstractExpW (k, depth, Whnf.whnf us)

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
        (kk, depth, IntSyn.Shift (k), s) -> 
	  if k < depth
	  then abstractSub (kk, depth, IntSyn.Dot (IntSyn.Idx (k+1), IntSyn.Shift (k+1)), s)
	  else (* k = depth *) s
      | (kk, depth, IntSyn.Dot (IntSyn.Idx (k), s), ss) ->
	  abstractSub (kk, depth, s, IntSyn.App (IntSyn.Root (IntSyn.BVar (k), IntSyn.Nil), ss))
      | (kk, depth, IntSyn.Dot (IntSyn.Exp (u), s), ss) ->
	  abstractSub (kk, depth, s, IntSyn.App (abstractExp (kk, depth, (u, IntSyn.id)), ss))
 
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
        (k, depth, (IntSyn.Nil, _))  -> IntSyn.Nil 
      | (k, depth, (IntSyn.SClo (ss, s'), s)) -> 
	  abstractSpine (k, depth, (ss, IntSyn.comp (s', s)))
      | (k, depth, (IntSyn.App (u, ss), s)) ->
	  IntSyn.App (abstractExp (k, depth, (u ,s)), 
		 abstractSpine (k, depth, (ss, s)))

    (* abstractDec (K, depth, (x:V, s)) = x:V'
       where V = {{V[s]}}_K

       Invariant:
       If   G |- s : G1     G1 |- V : L
       and  K ||- V
       and  |G| = depth

       then {{K}}, G |- V' : L
       and  . ||- V'
    *)
  and abstractDec (k, depth, (IntSyn.Dec (x, v), s)) =
	  IntSyn.Dec (x, abstractExp (k, depth, (v, s)))

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
  let rec abstractSOME args =
    match args with
        (kk, IntSyn.Shift 0) -> (* n = 0 by invariant, check for now *)
          IntSyn.Shift (IntSyn.ctxLength(kk))
      | (kk, IntSyn.Shift (n)) -> (* n > 0 *)
	  IntSyn.Shift (IntSyn.ctxLength(kk))
      | (kk, IntSyn.Dot (IntSyn.Idx k, s)) -> 
          IntSyn.Dot (IntSyn.Idx k, abstractSOME (kk, s))
      | (kk, IntSyn.Dot (IntSyn.Exp u, s)) ->
	  IntSyn.Dot (IntSyn.Exp (abstractExp (kk, 0, (u, IntSyn.id))), abstractSOME (kk, s))
      | (kk, IntSyn.Dot (IntSyn.Block (((IntSyn.LVar _) as l)), s)) ->
	  IntSyn.Dot (IntSyn.Block (abstractLVar (kk, 0, l)), abstractSOME (kk, s))
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
  let rec abstractCtx args =
    match args with
        (k, depth, IntSyn.Null) -> (IntSyn.Null, depth)
      | (k, depth, IntSyn.Decl (g, d)) ->
	  let (g', depth') = abstractCtx (k, depth, g) in
	  let d' = abstractDec (k, depth', (d, IntSyn.id)) in
	  (IntSyn.Decl (g', d'), depth'+1)

    (* abstractCtxlist (K, depth, [G1,...,Gn]) = [G1',...,Gn']
       where Gi' = {{Gi}}_K

       Invariants:
       if G0 |- G1,...,Gn ctx 
       and K ||- G1,...,Gn
       and |G0| = depth
       then {{K}}, G0 |- G1',...,Gn' ctx
       and . ||- G1',...,Gn'
    *)
  let rec abstractCtxlist args =
    match args with
        (k, depth, []) -> []
      | (k, depth, g::gs) ->
	  let (g', depth') = abstractCtx (k, depth, g) in
	  let gs' = abstractCtxlist (k, depth', gs) in
	  g'::gs'

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
  let rec abstractKPi args =
    match args with
        (IntSyn.Null, v) -> v
      | (IntSyn.Decl (k', EV (IntSyn.EVar (_, gx, vx, _))), v) ->
          let v' = raiseType (gx, vx) in
	  let v'' = abstractExp (k', 0, (v', IntSyn.id)) in
          (* enforced by reconstruction -kw
	  let _ = checkType v''	*)
	  abstractKPi (k', IntSyn.Pi ((IntSyn.Dec(None, v''), IntSyn.Maybe), v))
      | (IntSyn.Decl (k', FV (name,v')), v) ->
	  let v'' = abstractExp (k', 0, (v', IntSyn.id)) in
          (* enforced by reconstruction -kw
	  let _ = checkType v'' *)
	  abstractKPi (k', IntSyn.Pi ((IntSyn.Dec(Some(name), v''), IntSyn.Maybe), v))
      | (IntSyn.Decl (k', LV (IntSyn.LVar (r, _, (l, t)))), v) ->
	  let t' = abstractSOME (k', t)	  in
	  abstractKPi (k', IntSyn.Pi ((IntSyn.BDec (None, (l, t')), IntSyn.Maybe), v))

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
  let rec abstractKLam args =
    match args with
        (IntSyn.Null, u) -> u
      | (IntSyn.Decl (k', EV (IntSyn.EVar (_, gx, vx, _))), u) ->
	  let v' = raiseType (gx, vx) in
          abstractKLam (k', IntSyn.Lam (IntSyn.Dec(None, abstractExp (k', 0, (v', IntSyn.id))), u))
      | (IntSyn.Decl (k', FV (name,v')), u) ->
 	  abstractKLam (k', IntSyn.Lam (IntSyn.Dec(Some(name), abstractExp (k', 0, (v', IntSyn.id))), u))


  let rec abstractKCtx arg =
    match arg with
        (IntSyn.Null) -> IntSyn.Null
      | (IntSyn.Decl (k', EV (IntSyn.EVar (_, gx, vx, _)))) ->
	  let v' = raiseType (gx, vx) in
	  let v'' = abstractExp (k', 0, (v', IntSyn.id)) in
          (* enforced by reconstruction -kw
	  let _ = checkType v''	*)
	  IntSyn.Decl (abstractKCtx k', IntSyn.Dec (None, v''))
      | (IntSyn.Decl (k', FV (name, v'))) ->
	  let v'' = abstractExp (k', 0, (v', IntSyn.id)) in
          (* enforced by reconstruction -kw
	  let _ = checkType v'' *)
	  IntSyn.Decl (abstractKCtx k', IntSyn.Dec (Some(name), v''))
      | (IntSyn.Decl (k', LV (IntSyn.LVar (r, _, (l, t))))) ->
	  let t' = abstractSOME (k', t)	   in
	  IntSyn.Decl (abstractKCtx k', IntSyn.BDec (None, (l, t')))


    (* abstractDecImp V = (k', V')   (* rename --cs  (see above) *)

       Invariant: 
       If    . |- V : L
       and   K ||- V

       then  . |- V' : L
       and   V' = {{K}} V
       and   . ||- V'
       and   k' = |K|
    *)
  let  abstractDecImp v =
    let k = collectExp (IntSyn.Null, (v, IntSyn.id), IntSyn.Null) in
    let _ = checkConstraints (k) in
    (IntSyn.ctxLength k, abstractKPi (k, abstractExp (k, 0, (v, IntSyn.id))))

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
  let  abstractDef (u, v) =
    let k = collectExp (IntSyn.Null, (u, IntSyn.id), collectExp (IntSyn.Null, (v, IntSyn.id), IntSyn.Null)) in
    let _ = checkConstraints k in
    (IntSyn.ctxLength k, (abstractKLam (k, abstractExp (k, 0, (u, IntSyn.id))), 
			   abstractKPi  (k, abstractExp (k, 0, (v, IntSyn.id)))))


  let  abstractSpineExt (ss, s) =
    let k = collectSpine (IntSyn.Null, (ss, s), IntSyn.Null) in
    let _ = checkConstraints (k) in
    let g = abstractKCtx (k) in
    let s = abstractSpine (k, 0, (ss, s)) in
    (g, ss)
	      
    (* abstractCtxs [G1,...,Gn] = G0, [G1',...,Gn']
       Invariants:
       If . |- G1,...,Gn ctx
          K ||- G1,...,Gn for some K
       then G0 |- G1',...,Gn' ctx for G0 = {{K}}
       and G1',...,Gn' nf
       and . ||- G1',...,Gn' ctx
    *)
  let  abstractCtxs (gs) =
    let k = collectCtxs (IntSyn.Null, gs, IntSyn.Null) in
    let _ = checkConstraints k in
    (abstractKCtx (k), abstractCtxlist (k, 0, gs))

    (* closedDec (g, D) = true iff D contains no EVar or FVar *)
  let  closedDec (g, (IntSyn.Dec (_, v), s)) = 
      match collectExp (g, (v, s), IntSyn.Null) with
	  IntSyn.Null -> true
         | _ -> false

  let rec closedSub args =
    match args with
        (g, IntSyn.Shift _) -> true
      | (g, IntSyn.Dot (IntSyn.Idx _, s)) -> closedSub (g, s)
      | (g, IntSyn.Dot (IntSyn.Exp u, s)) -> 
        (match collectExp (g, (u, IntSyn.id), IntSyn.Null) with
	     IntSyn.Null -> closedSub (g, s)
            | _ -> false)

  let  closedExp (g, (u, s)) = 
      match collectExp (g, (u, IntSyn.id), IntSyn.Null) with
	  IntSyn.Null -> true
         | _ -> false

  let rec closedCtx arg =
    match arg with
        IntSyn.Null -> true
      | (IntSyn.Decl (g, d)) ->
          closedCtx g && closedDec (g, (d, IntSyn.id))

  let rec evarsToK l =
    match l with
        [] -> IntSyn.Null
      | (x::xs) -> IntSyn.Decl (evarsToK (xs), EV(x))

  let rec kToEVars arg =
    match arg with
        (IntSyn.Null) -> []
      | (IntSyn.Decl (k, EV(x))) -> x::kToEVars (k)
      | (IntSyn.Decl (k, _)) -> kToEVars (k)

    (* collectEVars (G, U[s], Xs) = Xs'
       Invariants:
         G |- U[s] : V
         Xs' extends Xs by new EVars in U[s]
    *)
  let  collectEVars (g, us, xs) =
          kToEVars (collectExp (g, us, evarsToK (xs)))

  let  collectEVarsSpine (g, (ss, s), xs) =
          kToEVars (collectSpine (g, (ss, s), evarsToK (xs)))

  let  abstractSpine = abstractSpineExt
