(* This is taken from the Twelf implementation *)

    let headConDec arg =
      match arg with
          (IntSyn.Const c) -> IntSyn.sgnLookup c
        | (IntSyn.Skonst c) -> IntSyn.sgnLookup c
        | (IntSyn.Def d) -> IntSyn.sgnLookup d
        | (IntSyn.NSDef d) -> IntSyn.sgnLookup d
        | (IntSyn.FgnConst (_, cd)) -> cd
          (* others impossible by invariant *)

  (* The approximate language is based on the idea of erasure.  The
     erasure of a term is defined as follows:

       c- = c
       d- = d
       type- = type
       kind- = kind
       ({x:A} B)- = A- -> B-
       ([x:A] M)- = M-    
       (M N)- = M-

       x- undefined
       X- undefined

     Note that erasure is always defined on well-typed terms at type
     family or kind level.  Also, if G |- U1 = U2 : V and U1,U2 are at
     type family or kind level, then U1- and U2- are defined and
     equal.  We can define the approximate typing judgment
             
       G |- U ~:~ V
                  
     by replacing appeals to equality in the usual presentation of the
     LF type theory with appeals to

       G |- U1 = U2 ~:~ V,

     which is defined to mean
           G |- U1 ~:~ V  and  G |- U2 ~:~ V  and  U1- = U2-
                                                         
     This is a mutual recursion between the two judgments, just as for
     the standard LF type theory.

     There is also a typing judgment on approximate terms

       |- u : v

     defined in the obvious way.  If |- u : v : l then for any
     well-formed G there are most general U, V such that G |- U : V
     and U- = u and V- = v.  *)
                                        
  (* The approximate language *)

    type uni =
        Level of int (* 1 = type, 2 = kind, 3 = hyperkind, etc. *)
      | Next of uni
      | LVar of uni option ref
    
    type exp =
        Uni of uni
      | Arrow of exp * exp
      | Const of IntSyn.head (* Const/Def/NSDef *)
      | CVar of exp option ref
      | Undefined


    let rec exp_to_string e =
      match e with
        Uni l ->
          (match l with
               Level(i) -> "uni("^(string_of_int i)^")"
             | Next(_) -> "next"
             | LVar x -> "uni(LVar)")
      | Arrow(e1,e2) -> "arrow("^(exp_to_string e1)^", "^(exp_to_string e2)^")"
      | Const(IntSyn.Const(i)) -> "const("^(IntSyn.conDecName (IntSyn.sgnLookup i))^")"
      | Const _ -> "const"
      | CVar(r) -> 
          (match (!r) with 
               Some(e) -> "cvar("^(exp_to_string e)^")"
             | None -> "cvar(None)")
      | Undefined -> "undefined" 




  let rec exp_equal e1 e2 =
    match e1, e2 with
      Uni l1, Uni l2 -> uni_equal l1 l2
    | Arrow(l1,r1), Arrow(l2,r2) -> exp_equal l1 l2 && exp_equal r1 r2
    | Const(h1), Const(h2) -> h1 = h2
    | CVar(r1), CVar(r2) -> 
        (match !r1, !r2 with
             Some(e1'), Some(e2') -> exp_equal e1' e2'
           | _ -> false )
    | Undefined, Undefined -> true
    | _ -> false 
  and uni_equal u1 u2 =
    match u1, u2 with
        Level i1, Level i2 -> i1 = i2
      | Next u1', Next u2' -> uni_equal u1' u2'
      | LVar op1, LVar op2 -> 
          (match !op1, !op2 with
               Some(u1'), Some(u2') -> uni_equal u1' u2'
             | _ -> false)


  (* Because approximate type reconstruction uses the pattern G |- U
     ~:~ V ~:~ L and universe unification on L, if U is to be an
     arbitrary input expression, there must be an internal universe
     Hyperkind such that |- Type ~:~ Kind ~:~ Hyperkind.  The
     Hyperkind universe is used only during the approximate phase of
     reconstruction.  The invariants established by
     ReconTerm.filterLevel ensure that Hyperkind will never appear
     elsewhere. *)

    let aType = Level 1
    let aKind = Level 2
    let aHyperkind = Level 3

    let newLVar () = LVar (ref None)
    let newCVar () = CVar (ref None)

    (* whnfUni (l) = l'
       where l = l' and l' is in whnf *)
    let rec whnfUni arg =
      match arg with
	  (Next l) ->
            (match whnfUni l with
                 Level i -> Level (i+1)
               | l' -> Next l')
        | (LVar r) when Option.isSome (!r) -> whnfUni (Option.get (!r))
        | l -> l

    (* whnf (u) = u'
       where u = u' and u' is in whnf *)
    let rec whnf arg =
      match arg with
	  (CVar v) when Option.isSome (!v) -> whnf (Option.get (!v))
        | v -> v
        
      (* just a little list since these are only for printing errors *)
    type varEntry = (exp * exp * uni) * string
    let varList : varEntry list ref = ref []
    let varReset () = (varList := [])
    let varLookupRef r = try Some(List.find (fun ((CVar r', _, _), _) -> r = r') (!varList))
                         with Not_found -> None
    let varLookupName name = try Some(List.find (fun (_, name') -> name = name') (!varList))
                             with Not_found -> None
    let varInsert ((u, v, l), name) = (varList := ((u, v, l), name)::(!varList))

    exception Ambiguous

      (* getReplacementName (u, v, l, allowed) = name
         if u : v : l
         and u is a CVar at type family or kind level *)
    let getReplacementName ((CVar r) as u, v, l, allowed) =
     (match varLookupRef r with
          Some (_, name) -> name
        | None ->
            let _ = if allowed then () else raise Ambiguous in
            let pref = (match whnfUni l with
                            Level 2 -> "A"
                          | Level 3 -> "K") in
                                  (* others impossible by invariant *)
            let rec tri i =
              let name = "%" ^ pref ^ (string_of_int i) ^ "%" in
              (match varLookupName name with
                   None -> (varInsert ((u, v, l), name); name)
                 | Some _ -> tri (i+1))
            in
            tri 1)

      (* findByReplacementName (name) = (u, v, l)
         if getReplacementName (u, v, l, allowed) = name was already called
         then u : v : l *)
    let findByReplacementName name =
      (match varLookupName name with
           Some (uvl, _) -> uvl
                (* must be in list by invariant *))
   

  (* converting exact terms to approximate terms *)

  (* uniToApx (L) = L- *)
  let uniToApx arg =
    match arg with
        (IntSyn.Type) -> aType
      | (IntSyn.Kind) -> aKind

  (* expToApx (U) = (U-, V-)
     if G |- U : V
     or G |- U ":" V = "hyperkind" *)
  let rec expToApx arg =
    match arg with
        (IntSyn.Uni l) ->
          let l' = uniToApx l in
          (Uni l', Uni (whnfUni (Next l')))
      | (IntSyn.Pi ((IntSyn.Dec (_, v1), _), v2)) ->
          let (v1', _ (* Type *)) = expToApx (v1) in
          let (v2', l') = expToApx (v2) in
          (Arrow (v1', v2'), l')
      | (IntSyn.Root (IntSyn.FVar (name, _, _), _)) ->
      (* must have been created to represent a CVar *)
          let (u, v, l) = findByReplacementName (name) in
          (u, v)
      | (IntSyn.Root (h (* Const/Def/NSDef *), _)) ->
        (* are we sure Skonst/FgnConst are never types or kinds? *)
          (Const h, Uni aType)
      | (IntSyn.Redex (u, _)) -> expToApx u
      | (IntSyn.Lam (_, u)) -> expToApx u
      | (IntSyn.EClo (u, _)) -> expToApx u

  (* classToApx (V) = (V-, L-)
     if G |- V : L
     or G |- V ":" L = "hyperkind" *)
  let classToApx (v) =
    let (v', l') = expToApx (v) in
    let Uni l'' = whnf l' in
    (v', l'')

  (* exactToApx (U, v) = (U-, v-)
     if G |- U : v *)
  let exactToApx (u, v) =
    let (v', l') = classToApx (v) in
      match whnfUni l' with
          Level 1 (* Type *) -> (Undefined, v', l')
        | _ (* Kind/Hyperkind *) ->
            let (u', _ (* v' *)) = expToApx (u) in
            (u', v', l')
(*
  (* constDefApx (d) = v-
     if |- d = v : type *)
  let constDefApx d =
    (match IntSyn.sgnLookup d with
         IntSyn.ConDef (_, _, _, u, _, _, _) ->
           let (v', _ (* Uni Type *)) = expToApx u in
           v'
       | IntSyn.AbbrevDef (_, _, _, u, _, _) ->
           let (v', _ (* Uni Type *)) = expToApx u in
           v')
*)
  (* converting approximate terms to exact terms *)

  (* apxToUni (L-) = L *)
  let apxToUniW arg =
    match arg with
        (Level 1) -> IntSyn.Type
      | (Level 2) -> IntSyn.Kind
      (* others impossible by invariant *)
  let apxToUni l = apxToUniW (whnfUni l)

  (* apxToClass (G, v, L-, allowed) = V
     pre: L is ground and <= Hyperkind,
          and if L is Hyperkind then the target classifier
          of v is ground
          v : L-
     post: V is most general such that V- = v and G |- V : L *)
  let rec apxToClassW (g, v, l, allowed) =
    match (g, v, l, allowed) with
	(g, Uni l, _ (* Next l *), allowed) ->
          IntSyn.Uni (apxToUni l)
      | (g, Arrow (v1, v2), l, allowed) ->
      (* this is probably very bad -- it should be possible to infer
         more accurately which pis can be dependent *)
      (* also, does the name of the bound variable here matter? *)
          let v1' = apxToClass (g, v1, aType, allowed) in
          let d = IntSyn.Dec (None, v1') in
          let v2' = apxToClass (IntSyn.Decl (g, d), v2, l, allowed) in
          IntSyn.Pi ((d, IntSyn.Maybe), v2')
      | (g, CVar r, l (* Type or Kind *), allowed) ->
      (* convert undetermined CVars to FVars *)
          let name = getReplacementName (v, Uni l, Next l, allowed) in
          let s = IntSyn.Shift (IntSyn.ctxLength (g)) in
          IntSyn.Root (IntSyn.FVar (name, IntSyn.Uni (apxToUni l), s), IntSyn.Nil)
      | (g, Const h, l (* Type *), allowed) ->
          IntSyn.Root (h, Whnf.newSpineVar (g, (IntSyn.conDecType (headConDec h), IntSyn.id)))
      (* Undefined case impossible *)
  and apxToClass (g, v, l, allowed) = apxToClassW (g, whnf v, l, allowed)

  (* apxToExact (G, u, (V, s), allowed) = U
     if u : V-
     and G' |- V : L and G |- s : G'
     then U- = u and G |- U : V[s] and U is the most general such *)
  let rec apxToExactW (g, u, vs, allowed) =
    match (g, u, vs, allowed) with
        (g, u, (IntSyn.Pi ((d, _), v), s), allowed) ->
          let d' = IntSyn.decSub (d, s) in
          IntSyn.Lam (d', apxToExact (IntSyn.Decl (g, d'), u, (v, IntSyn.dot1 s), allowed))
      | (g, u, (IntSyn.Uni l, s), allowed) ->
          apxToClass (g, u, uniToApx l, allowed)
      | (g, u, (IntSyn.Root (IntSyn.FVar (name, _, _), _), s), allowed) ->
          let (v, l, _ (* Next L *)) = findByReplacementName (name) in
          let Uni l = whnf l in
          (match whnfUni l with
               Level 1 -> let (v, s) = vs in IntSyn.newEVar (g, IntSyn.EClo (v,s))
             | Level 2 ->
             (* U must be a CVar *)
                 let name' = getReplacementName (whnf u, v, Level 2, allowed) in
               (* NOTE: V' differs from Vs by a Shift *)
               (* probably could avoid the following call by removing the
                  substitutions in Vs instead *)
                 let v' = apxToClass (IntSyn.Null, v, Level 2, allowed) in
                 let s' = IntSyn.Shift (IntSyn.ctxLength (g)) in
                 IntSyn.Root (IntSyn.FVar (name', v', s'), IntSyn.Nil))
      | (g, u, (v,s) (* an atomic type, not Def *), allowed) ->
          IntSyn.newEVar (g, IntSyn.EClo (v,s))
  and apxToExact (g, u, vs, allowed) = apxToExactW (g, u, Whnf.whnfExpandDef vs, allowed)

  (* matching for the approximate language *)

  exception Unify of string

    (* occurUni (r, l) = ()
       iff r does not occur in l,
       otherwise raises Unify *)
  let rec occurUniW args =
    match args with
        (r, Next l) -> occurUniW (r, l)
      | (r, LVar r') ->
          if r == r'
          then raise (Unify "Level circularity")
          else ()
(*
          (match (!r, !r') with
               Some(v), Some(v') when v = v' ->
                 raise (Unify "Level circularity")
             | _ -> ())
*)
      | (r, _) -> ()
    let occurUni (r, l) = occurUniW (r, whnfUni l)

    (* matchUni (l1, l2) = ()
       iff l1<I> = l2<I> for some most general instantiation I
       effect: applies I
       otherwise raises Unify *)
    let rec matchUniW args =
      match args with
	  (Level i1, Level i2) ->
            if i1 = i2 then () else raise (Unify "Level clash")
        | (Level i1, Next l2) ->
            if i1 > 1 then matchUniW (Level (i1-1), l2)
            else raise (Unify "Level clash")
        | (Next l1, Level i2) ->
            if i2 > 1 then matchUniW (l1, Level (i2-1))
            else raise (Unify "Level clash")
        | (Next l1, Next l2) ->
            matchUniW (l1, l2)
        | (LVar r1, ((LVar r2) as l2)) ->
            if r1 == r2 
            then ()
            else r1 := Some l2
(*
            (match (!r1,!r2) with
                 (Some(e1),Some(e2)) when uni_equal e1 e2 -> ()
                | _ -> r1 := Some l2)
*)
        | (LVar r1, l2) ->
            (occurUniW (r1, l2);
             r1 := Some l2)
        | (l1, LVar r2) ->
            (occurUniW (r2, l1);
             r2 := Some l1)
    let matchUni (l1, l2) = matchUniW (whnfUni l1, whnfUni l2)

    (* occur (r, u) = ()
       iff r does not occur in u,
       otherwise raises Unify *)
    let rec occurW args =
      match args with
	  (r, Arrow (v1, v2)) -> (occur (r, v1); occur (r, v2))
        | (r, CVar r') ->
            if r == r' 
            then raise (Unify "Type/kind variable occurrence")
            else ()
(*
            (match (!r, !r') with
                 Some(e1), Some(e2) when exp_equal e1 e2 ->
                   raise (Unify "Type/kind variable occurrence (somes)")
               | _ -> ())
*)
        | (r, _) -> ()
    and occur (r, u) = occurW (r, whnf u)

    (* matchFun (u1, u2) = ()
       iff u1<I> = u2<I> : v for some most general instantiation I
       effect: applies I
       otherwise raises Unify *)
    let rec matchW (v1, v2) =
      match (v1, v2) with
	  (Uni l1, Uni l2) -> matchUni (l1, l2)
        | (Const h1, Const h2) ->
            (match (h1, h2) with
                 (IntSyn.Const(c1), IntSyn.Const(c2)) ->
                   if c1 = c2 then ()
                   else raise (Unify "Type/kind constant clash")
(*               | (IntSyn.Def(d1), IntSyn.Def(d2)) ->
                 if d1 = d2 then ()
                 else matchFun (constDefApx d1, constDefApx d2)
               | (IntSyn.Def(d1), _) -> matchFun (constDefApx d1, v2)
               | (_, IntSyn.Def(d2)) -> matchFun (v1, constDefApx d2)
              (* strictness is irrelevant to matching on approximate types *)
               | (IntSyn.NSDef(d1), IntSyn.NSDef(d2)) ->
                 if d1 = d2 then ()
                 else matchFun (constDefApx d1, constDefApx d2)
               | (IntSyn.NSDef(d1), _) -> matchFun (constDefApx d1, v2)
               | (_, IntSyn.NSDef(d2)) -> matchFun (v1, constDefApx d2))
              (* others cannot occur by invariant *)
*) )
        | (Arrow (v1, v2), Arrow (v3, v4)) ->
             (try matchFun (v1, v3)
              with e -> (matchFun (v2, v4); raise e));
             matchFun (v2, v4)
(*        | (Arrow _, Const(IntSyn.Def(d2))) ->
            matchFun (v1, constDefApx d2)
        | (Const(IntSyn.Def(d1)), Arrow _) ->
            matchFun (constDefApx d1, v2)
        | (Arrow _, Const(IntSyn.NSDef(d2))) ->
            matchFun (v1, constDefApx d2)
        | (Const(IntSyn.NSDef(d1)), Arrow _) ->
            matchFun (constDefApx d1, v2)
*)
        | (CVar r1, ((CVar r2) as u2)) ->
            if r1 == r2
            then ()
            else r1 := Some u2
(*
            (match (!r1, !r2) with
                 Some(e1), Some(e2) when exp_equal e1 e2 -> ()
               | _ -> r1 := Some u2)
*)
        | (CVar r1, u2) ->
            (occurW (r1, u2);
             r1 := Some u2)
        | (u1, CVar r2) ->
            (occurW (r2, u1);
            r2 := Some u1)
        |  _ -> raise (Unify "Type/kind expression clash")
    and matchFun (u1, u2) = matchW (whnf u1, whnf u2)

    let matchable (u1, u2) = try (matchFun (u1, u2); true)
                             with Unify _ -> false

    let rec makeGroundUni arg =
      match arg with
	  (Level _) -> false
      | (Next l) -> makeGroundUni l
      | (LVar r) when Option.isSome (!r) -> makeGroundUni (Option.get (!r))
      | (LVar r) when r = ref None -> (r := Some (Level 1);
                                                true)


