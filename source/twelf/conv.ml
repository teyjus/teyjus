   open IntSyn

    (* eqUni (L1, L2) = B iff L1 = L2 *)
   let eqUni args =
     match args with
         (Type, Type) -> true
        | (Kind, Kind) -> true
        |  _ -> false

    (* convExpW ((U1, s1), (U2, s2)) = B

       Invariant:
       If   G |- s1 : G1    G1 |- U1 : V1    (U1,s1) in whnf
            G |- s2 : G2    G2 |- U2 : V2    (U2,s2) in whnf
            G |- V1[s1] == V2[s2] == V : L
       then B iff G |- U1[s1] == U2[s2] : V

       Effects: EVars may be lowered
    *)
  let rec convExpW (us1, us2) =
    match (us1, us2) with
        ((Uni(l1), _), (Uni(l2), _)) ->
            eqUni (l1, l2)
      | ((Root (h1, ss1), s1),(Root (h2, ss2), s2)) ->
          (* s1 = s2 = id by whnf invariant *)
          (* order of calls critical to establish convSpine invariant *)
          (match (h1, h2) with
               (BVar(k1), BVar(k2)) ->
                 (k1 = k2) && convSpine ((ss1, s1), (ss2, s2))
             | (Const(c1), Const(c2)) ->
                 (c1 = c2) && convSpine ((ss1, s1), (ss2, s2))
             | (Skonst c1, Skonst c2) ->
                 (c1 = c2) && convSpine ((ss1, s1), (ss2, s2))
             | (Proj (Bidx v1, i1), Proj (Bidx v2, i2)) ->
                 (v1 = v2) && (i1 = i2) && convSpine ((ss1, s1), (ss2, s2))
             | (FVar (n1,_,s1'), FVar (n2,_,s2')) ->
                 (* s1' = s2' = ^|G| *)
                 (n1 = n2) && convSpine ((ss1, s1), (ss2, s2))
             | (FgnConst (cs1, cD1), FgnConst (cs2, cD2)) ->
                 (* they must have the same string representation *)
                 (cs1 = cs2) && (conDecName (cD1) = conDecName (cD2))
                 && convSpine ((ss1, s1), (ss2, s2))
(*             | (Def (d1), Def (d2)) ->
                 (* because of strict *)
                 ((d1 = d2) && convSpine ((ss1, s1), (ss2, s2)))
                 || convExpW (Whnf.expandDef (us1), Whnf.expandDef (us2))
             | (Def (d1), _) -> convExpW (Whnf.expandDef us1, us2)
             | (_, Def(d2)) -> convExpW (us1, Whnf.expandDef us2) *)
             | _ -> false)

      | ((Pi (dp1, v1), s1), (Pi (dp2, v2), s2)) ->
          convDecP ((dp1, s1), (dp2, s2))
          && convExp ((v1, dot1 s1), (v2, dot1 s2))
(*
      | ((Pi _, _), (Root (Def _, _), _)) ->
          convExpW (us1, Whnf.expandDef us2)

      | ((Root (Def _, _), _), (Pi _, _)) ->
          convExpW (Whnf.expandDef us1, us2)
*)
      | ((Lam (d1, u1), s1), (Lam (d2, u2), s2)) ->
        (* G |- D1[s1] = D2[s2] by typing invariant *)
          convExp ((u1, dot1 s1),  (u2, dot1 s2))

      | ((Lam (d1, u1), s1), (u2, s2)) ->
          convExp ((u1, dot1 s1),
                   (Redex (EClo (u2, shift),
                           App (Root (BVar (1), Nil), Nil)), dot1 s2))

      | ((u1,s1), (Lam(d2, u2) ,s2)) ->
          convExp ((Redex (EClo (u1, shift),
                           App (Root (BVar (1), Nil), Nil)), dot1 s1),
                   (u2, dot1 s2))

      | ((FgnExp (i1,csfe1), s1), (us2, us2')) -> (* s1 = id *)
          FgnExpStd.EqualTo.apply (i1,csfe1) (EClo (us2, us2'))

      | ((us1, us1'), (FgnExp (i2,csfe2), s2)) -> (* s2 = id *)
          FgnExpStd.EqualTo.apply (i2,csfe2) (EClo (us1, us1'))

      | ((EVar (r1, _, _, _), s1), (EVar(r2, _, _, _), s2)) ->
          (r1 = r2) && convSub (s1, s2)

      (* ABP -- 2/18/03 Added missing case*)
      (* Note that under Head, why is NSDef never used?? *)
      | _ -> false
        (* Possible are:
           L <> Pi D. V   Pi D. V <> L
           X <> U         U <> X
        *)

    (* convExp ((U1, s1), (U2, s2)) = B

       Invariant:
       as above, but (U1, s1), (U2, s2) need not to be in whnf
       Effects: EVars may be lowered
    *)
    and convExp (us1, us2) = convExpW (Whnf.whnf us1, Whnf.whnf us2)

    (* convSpine ((S1, s1), (S2, s2)) = B

       Invariant:
       If   G |- s1 : G1     G1 |- S1 : V1 > W1
            G |- s2 : G2    G2 |- S2 : V2 > W2
            G |- V1[s1] = V2[s2] = V : L
            G |- W1[s1] = W2[s2] = W : L
       then B iff G |- S1 [s1] = S2 [s2] : V > W

       Effects: EVars may be lowered
    *)
    and convSpine args =
      match args with
          ((Nil, _), (Nil, _)) -> true
      | ((App (u1, ss1), s1), (App (u2, ss2), s2)) -> 
          convExp ((u1, s1), (u2, s2)) && convSpine ((ss1, s1), (ss2, s2))
      | ((SClo (ss1, s1'), s1), ss2) -> 
	  convSpine ((ss1, comp (s1', s1)), ss2)
      | (ss1, (SClo (ss2, s2'), s2)) -> 
	  convSpine (ss1, (ss2, comp (s2', s2)))
      | (_ , _) -> false (* bp*)
    (* no others are possible due to typing invariants *)

    (* convSub (s1, s2) = B

     Invariant:
     If  G |- s1 : G'
         G |- s2 : G'
     then B iff G |- s1 = s2 : G'
     Effects: EVars may be lowered
    *)
    and convSub (s1, s2) =
      match (s1, s2) with
	  (Shift(n), Shift(m)) -> true (* n = m by invariants *)
        | (Shift(n), Dot _) ->
            convSub (Dot(Idx(n+1), Shift(n+1)), s2)
        | (Dot _, Shift(m)) ->
            convSub (s1, Dot(Idx(m+1), Shift(m+1)))
        | (Dot(ft1,s1), Dot(ft2,s2)) ->
            (match (ft1, ft2) with
                 (Idx (n1), Idx (n2)) -> (n1 = n2)
               | (Exp (u1), Exp (u2)) -> convExp ((u1, id), (u2, id))
               | (Block (Bidx k1), Block (Bidx k2)) -> (k1 = k2) (* other block cases don't matter -cs 2/18/03 *)
               | (Exp (u1), Idx (n2)) -> convExp ((u1, id), (Root (BVar (n2), Nil), id))
               | (Idx (n1), Exp (u2)) -> convExp ((Root (BVar (n1), Nil), id), (u2, id))
               | (Undef, Undef) -> true
               | _ -> false)
            && convSub (s1, s2)

    (* convDec ((x1:V1, s1), (x2:V2, s2)) = B

       Invariant:
       If   G |- s1 : G'     G'  |- V1 : L
            G |- s2 : G''    G'' |- V2 : L
       then B iff G |- V1 [s1]  = V2 [s2] : L
       Effects: EVars may be lowered
    *)
    and convDec args =
      match args with
	  ((Dec (_, v1), s1), (Dec (_, v2), s2)) ->  convExp ((v1, s1), (v2, s2))
        | ((BDec(_, (c1, s1)), t1), (BDec (_, (c2, s2)), t2)) ->
          c1 = c2 && convSub (comp (s1, t1), comp (s2, t2))

    (* convDecP see convDec *)
    and convDecP (((d1, p1), s1), ((d2, p2), s2)) =  convDec ((d1, s1), (d2, s2))


let conv = convExp
