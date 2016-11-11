  type cid = int			(* Constant identifier        *)
  type mid = int                        (* Stsructure identifier       *)
  type csid = int                       (* CS module identifier       *)


  type fgnExp = exn                     (* foreign expression representation *)
  exception UnexpectedFgnExp of fgnExp
                                        (* raised by a constraint solver
					   if passed an incorrect arg *)
  type fgnCnstr = exn                   (* foreign constraint representation *)
  exception UnexpectedFgnCnstr of fgnCnstr
                                        (* raised by a constraint solver
                                           if passed an incorrect arg *)

  (* Contexts *)

  type 'a ctx =			(* Contexts                   *)
    Null				(* G ::= .                    *)
  | Decl of 'a ctx * 'a			(*     | G, D                 *)
    
  (* ctxPop (G) => G'
     Invariant: G = G',D
  *)
  let ctxPop (Decl (g, d)) = g

  (* ctxLookup (G, k) = D, kth declaration in G from right to left
     Invariant: 1 <= k <= |G|, where |G| is length of G
  *)

  let rec ctxLookup args =
    match args with
        (Decl (g', d), 1) -> d
      | (Decl (g', _), k') -> ctxLookup (g', k'-1)
(*    | ctxLookup (Null, k') = (print ("Looking up k' = " ^ Int.toString k' ^ "\n"); raise Error "Out of Bounce\n")*)
    (* ctxLookup (Null, k')  should not occur by invariant *)

  (* ctxLength G = |G|, the number of declarations in G *)
  let ctxLength g =
    let rec ctxLength' args =
      match args with
          (Null, n) -> n
	| (Decl(g, _), n)-> ctxLength' (g, n+1)
    in
    ctxLength' (g, 0)
    
  type depend =                     (* dependency information     *)
    No                                  (* P ::= No                   *)
  | Maybe                               (*     | Maybe                *)
  | Meta				(*     | Meta                 *)

  (* expressions *)

  type uni =			(* Universes:                 *)
    Kind				(* L ::= Kind                 *)
  | Type				(*     | Type                 *)

  type exp =			(* Expressions:               *)
    Uni   of uni			(* U ::= L                    *)
  | Pi    of (dec * depend) * exp	(*     | Pi (D, P). V         *)
  | Root  of head * spine		(*     | H @ S                *)
  | Redex of exp * spine		(*     | U @ S                *)
  | Lam   of dec * exp			(*     | lam D. U             *)
  | EVar  of exp option ref * dec ctx * exp * (cnstr ref) list ref
                                        (*     | X<I> : G|-V, cnstr   *)
  | EClo  of exp * sub			(*     | U[s]                 *)
  | AVar  of exp option ref             (*     | A<I>                 *)

  | FgnExp of csid * fgnExp             (*     | (foreign expression) *)

  | NVar  of int			(*     | n (linear, 
                                               fully applied variable
                                               used in indexing       *)

  and head =				(* Head:                      *)
    BVar  of int			(* H ::= k                    *)
  | Const of cid			(*     | c                    *)
  | Proj  of block * int		(*     | #k(b)                *)
  | Skonst of cid			(*     | c#                   *)
  | Def   of cid			(*     | d (strict)           *)
  | NSDef of cid			(*     | d (non strict)       *)
  | FVar  of string * exp * sub		(*     | F[s]                 *)
  | FgnConst of csid * conDec           (*     | (foreign constant)   *)

  and spine =				(* Spines:                    *)
    Nil					(* S ::= Nil                  *)
  | App   of exp * spine		(*     | U ; S                *)
  | SClo  of spine * sub		(*     | S[s]                 *)

  and sub =				(* Explicit substitutions:    *)
    Shift of int			(* s ::= ^n                   *)
  | Dot   of front * sub		(*     | Ft.s                 *)

  and front =				(* Fronts:                    *)
    Idx of int				(* Ft ::= k                   *)
  | Exp of exp				(*     | U                    *)
  | Axp of exp				(*     | U                    *)
  | Block of block			(*     | _x                   *)
  | Undef				(*     | _                    *)

  and dec =				(* Declarations:              *)
    Dec of string option * exp		(* D ::= x:V                  *)
  | BDec of string option * (cid * sub)	(*     | v:l[s]               *)
  | ADec of string option * int	        (*     | v[^-d]               *)
  | NDec of string option 

  and block =				(* Blocks:                    *)
    Bidx of int				(* b ::= v                    *)
  | LVar of block option ref * sub * (cid * sub)
                                        (*     | L(l[^k],t)           *)
  | Inst of exp list                    (*     | U1, ..., Un          *)
  (* It would be better to consider having projections count
     like substitutions, then we could have Inst of sub here, 
     which would simplify a lot of things.  

     I suggest however to wait until the next big overhaul 
     of the system -- cs *)


(*  | BClo of block * sub                 (*     | b[s]                 *) *)

  (* constraints *)

  and cnstr =				(* Constraint:                *)
    Solved                      	(* Cnstr ::= solved           *)
  | Eqn      of dec ctx * exp * exp     (*         | G|-(U1 == U2)    *)
  | FgnCnstr of csid * fgnCnstr         (*         | (foreign)        *)

  and status =                          (* Status of a constant:      *)
    Normal                              (*   inert                    *)
  | Constraint of csid * (dec ctx * spine * int -> exp option)
                                        (*   acts as constraint       *)
  | Foreign of csid * (spine -> exp)    (*   is converted to foreign  *)

  and fgnUnify =                        (* Result of foreign unify    *)
    Succeed of fgnUnifyResidual list
    (* succeed with a list of residual operations *)
  | Fail

  and fgnUnifyResidual =
    Assign of dec ctx * exp * exp * sub
    (* perform the assignment G |- X = U [ss] *)
  | Delay of exp * cnstr ref
    (* delay cnstr, associating it with all the rigid EVars in U  *)

  (* Global signature *)

  and conDec =			        (* Constant declaration       *)
    ConDec of string * mid option * int * status
                                        (* a : K : kind  or           *)
              * exp * uni	        (* c : A : type               *)
(*  | ConDef of string * mid option * int	(* a = A : K : kind  or       *)
              * exp * exp * uni		(* d = M : A : type           *)
              * ancestor                (* Ancestor info for d or a   *)
  | AbbrevDef of string * mid option * int
                                        (* a = A : K : kind  or       *)
              * exp * exp * uni		(* d = M : A : type           *)
  | BlockDec of string * mid option     (* %block l : Some G1 PI G2   *)
              * dec ctx * dec list
  | BlockDef of string * mid option * cid list
                                        (* %block l = (l1 | ... | ln) *)
  | SkoDec of string * mid option * int	(* sa: K : kind  or           *)
              * exp * uni	        (* sc: A : type               *) *)

  and ancestor =			(* Ancestor of d or a         *)
    Anc of cid option * int * cid option (* head(expand(d)), height, head(expand[height](d)) *)
                                        (* None means expands to {x:A}B *)

  type strDec =                     (* Structure declaration      *)
      StrDec of string * mid option

  (* Form of constant declaration *)
  type conDecForm =
    FromCS				(* from constraint domain *)
  | Ordinary				(* ordinary declaration *)
  | Clause				(* %clause declaration *)

  (* Type abbreviations *)
  type dctx = dec ctx			(* G = . | G,D                *)
  type eclo = exp * sub   		(* Us = U[s]                  *)
  type bclo = block * sub   		(* Bs = B[s]                  *)
  type cnstrRef = cnstr ref

  exception Error of string		(* raised if out of space     *)

  (* standard operations on foreign expressions *)
  module FgnExpStd = struct
    (* convert to internal syntax *)
    module ToInternal = Fgnopn.FgnOpnTable (struct
					     type arg = unit
					     type result = exp
					    end)

    (* apply function to subterms *)
    module Map = Fgnopn.FgnOpnTable (struct
					     type arg = exp -> exp
					     type result = exp
					    end)

    (* apply function to subterms, for effect *)
    module App = Fgnopn.FgnOpnTable (struct
					     type arg = exp -> unit
					     type result = unit
					    end)

    (* test for equality *)
    module EqualTo = Fgnopn.FgnOpnTable (struct
					     type arg = exp
					     type result = bool
				       end)
				       
    (* unify with another term *)
    module UnifyWith = Fgnopn.FgnOpnTable (struct
					     type arg = dec ctx * exp
					     type result = fgnUnify
					    end)

    (* fold a function over the subterms *)
    let fold csfe f b = 
      let r = ref b in
      let g u = r := f (u,!r) in
      App.apply csfe g ; !r

  end

  (* standard operations on foreign constraints *)
  module FgnCnstrStd = struct
    (* convert to internal syntax *)
    module ToInternal = Fgnopn.FgnOpnTable (struct
					     type arg = unit
					     type result = (dec ctx * exp) list
					    end)

    (* awake *)
    module Awake = Fgnopn.FgnOpnTable (struct
					     type arg = unit
					     type result = bool
					    end)

    (* simplify *)
    module Simplify = Fgnopn.FgnOpnTable (struct
					     type arg = unit
					     type result = bool
					    end)
  end


  let conDecName c =
    match c with
        (ConDec (name, _, _, _, _, _)) -> name
 (*     | (ConDef (name, _, _, _, _, _, _)) -> name
      | (AbbrevDef (name, _, _, _, _, _)) -> name
      | (SkoDec (name, _, _, _, _)) -> name
      | (BlockDec (name, _, _, _)) -> name
      | (BlockDef (name, _, _)) -> name
  *)
					    
  let conDecParent c =
    match c with
        (ConDec (_, parent, _, _, _, _)) -> parent
(*      | (ConDef (_, parent, _, _, _, _, _)) -> parent
      | (AbbrevDef (_, parent, _, _, _, _)) -> parent
      | (SkoDec (_, parent, _, _, _)) -> parent
      | (BlockDec (_, parent, _, _)) -> parent
      | (BlockDef (_, parent, _)) -> parent
 *)  

  (* conDecImp (CD) -> k

     Invariant:
     If   CD is either a declaration, definition, abbreviation, or 
          a Skolem constant
     then k stands for the number of implicit elements.
  *)
  let conDecImp c = 
    match c with
        (ConDec (_, _, i, _, _, _)) -> i
 (*     | (ConDef (_, _, i, _, _, _, _)) -> i
      | (AbbrevDef (_, _, i, _, _, _)) -> i
      | (SkoDec (_, _, i, _, _)) -> i
      | (BlockDec (_, _,  _, _)) -> 0   (* watch out -- carsten *)
  *)
  let conDecStatus c =
    match c with
        (ConDec (_, _, _, status, _, _)) -> status
      | _ -> Normal

  (* conDecType (CD) ->  V

     Invariant:
     If   CD is either a declaration, definition, abbreviation, or 
          a Skolem constant
     then V is the respective type
  *)
  let conDecType c =
    match c with
        (ConDec (_, _, _, _, v, _)) -> v
 (*     | (ConDef (_, _, _, _, v, _, _)) -> v
      | (AbbrevDef (_, _, _, _, v, _)) -> v
      | (SkoDec (_, _, _, v, _)) -> v
  *)

  (* conDecBlock (CD) ->  (Gsome, Lpi)

     Invariant:
     If   CD is block definition
     then Gsome is the context of some variables
     and  Lpi is the list of pi variables
  *)
  (* let conDecBlock (BlockDec (_, _, Gsome, Lpi)) = (Gsome, Lpi) *)

  (* conDecUni (CD) ->  L

     Invariant:
     If   CD is either a declaration, definition, abbreviation, or 
          a Skolem constant
     then L is the respective universe
  *)
  let conDecUni c =
    match c with
        (ConDec (_, _, _, _, _, l)) -> l
   (*   | (ConDef (_, _, _, _, _, l, _)) -> l
      | (AbbrevDef (_, _, _, _, _, l)) -> l
      | (SkoDec (_, _, _, _, l)) -> l
    *)

  let strDecName (StrDec (name, _)) = name

  let strDecParent (StrDec (_, parent)) = parent

  let maxCid = Global.maxCid
  let dummyEntry = ConDec("", None, 0, Normal, Uni (Kind), Kind)
  let sgnArray = Array.make (maxCid+1) dummyEntry 
  let nextCid  = ref(0)

  let maxMid = Global.maxMid
  let sgnStructArray = Array.make (maxMid+1) (StrDec("", None))
  let nextMid = ref (0)

					    

    (* Invariants *)
    (* Constant declarations are all well-typed *)
    (* Constant declarations are stored in beta-normal form *)
    (* All definitions are strict in all their arguments *)
    (* If Const(cid) is valid, then sgnArray(cid) = ConDec _ *)
    (* If Def(cid) is valid, then sgnArray(cid) = ConDef _ *)

  let rec sgnClean (i) = if i >= !nextCid then ()
                     else Array.set sgnArray i dummyEntry;
                          sgnClean (i+1)

  let sgnReset () = ((* Fri Dec 20 12:04:24 2002 -fp *)
                       (* this circumvents a space leak *)
                     sgnClean (0);
                     nextCid := 0; nextMid := 0)
  let sgnSize () = (!nextCid, !nextMid)

  let sgnAdd (conDec) =
    let cid = !nextCid in
      if cid > maxCid
      then raise (Error ("Global signature size " ^ (string_of_int (maxCid+1)) ^ " exceeded"))
      else 
        Array.set sgnArray cid conDec;
        nextCid := cid + 1;
        cid 
    (* 0 <= cid < !nextCid *)
  let sgnLookup (cid) = Array.get sgnArray cid

  let sgnApp (f) =
    let rec sgnApp' (cid) = 
       if cid = !nextCid then () else (f cid; sgnApp' (cid+1))
    in
    sgnApp' (0)

  let sgnStructAdd (strDec) =
    let mid = !nextMid in
      if mid > maxMid
      then raise (Error ("Global signature size " ^ (string_of_int (maxMid+1)) ^ " exceeded"))
      else (Array.set sgnStructArray mid strDec ;
                nextMid := mid + 1;
                mid)

    (* 0 <= mid < !nextMid *)
  let sgnStructLookup (mid) = Array.get sgnStructArray mid

    (* A hack used in Flit - jcreed 6/05 *)
  let rename (cid, n) =
    let newConDec =
      match sgnLookup cid with
           ConDec (n,m,i,s,e,u) -> ConDec(n,m,i,s,e,u)
(*         | ConDef (n,m,i,e,e',u,a) -> ConDef(n,m,i,e,e',u,a)
         | AbbrevDef (n,m,i,e,e',u) -> AbbrevDef (n,m,i,e,e',u)
         | BlockDec (n,m,d,d') -> BlockDec (n,m,d,d')
         | SkoDec (n,m,i,e,u) -> SkoDec (n,m,i,e,u) *)
   in
   Array.set sgnArray cid newConDec

(*
  let constDef (d) =
    (match sgnLookup (d) with
         ConDef(_, _, _, u,_, _, _) -> u
       | AbbrevDef (_, _, _, u,_, _) -> u)
 *)
  let constType (c) = conDecType (sgnLookup c)
  let constImp (c) = conDecImp (sgnLookup c)
  let constUni (c) = conDecUni (sgnLookup c)
  (*  let constBlock (c) = conDecBlock (sgnLookup c) *)

  let constStatus (c) =
    (match sgnLookup (c) with
     (*         ConDec (_, _, _, status, _, _) -> status *)
       | _ -> Normal)


(* Explicit Substitutions *)

  (* id = ^0

     Invariant:
     G |- id : G        id is patsub
  *)
  let id = Shift(0)

  (* shift = ^1

     Invariant:
     G, V |- ^ : G       ^ is patsub
  *)
  let shift = Shift(1)

  (* invShift = ^-1 = _.^0
     Invariant:
     G |- ^-1 : G, V     ^-1 is patsub
  *)
  let invShift = Dot(Undef, id)


  (* comp (s1, s2) = s'

     Invariant:
     If   G'  |- s1 : G
     and  G'' |- s2 : G'
     then s'  = s1 o s2
     and  G'' |- s1 o s2 : G

     If  s1, s2 patsub
     then s' patsub
   *)
  let rec comp args =
    match args with
        (Shift (0), s) -> s
    (* next line is an optimization *)
    (* roughly 15% on standard suite for Twelf 1.1 *)
    (* Sat Feb 14 10:15:16 1998 -fp *)
      | (s, Shift (0)) -> s
      | (Shift (n), Dot (ft, s)) -> comp (Shift (n-1), s)
      | (Shift (n), Shift (m)) -> Shift (n+m)
      | (Dot (ft, s), s') -> Dot (frontSub (ft, s'), comp (s, s'))

  (* bvarSub (n, s) = Ft'

      Invariant:
     If    G |- s : G'    G' |- n : V
     then  Ft' = Ftn         if  s = Ft1 .. Ftn .. ^k
       or  Ft' = ^(n+k)     if  s = Ft1 .. Ftm ^k   and m<n
     and   G |- Ft' : V [s]
  *)
  and bvarSub args =
    match args with
        (1, Dot(ft, s)) -> ft
      | (n, Dot(ft, s)) -> bvarSub (n-1, s)
      | (n, Shift(k))  -> Idx (n+k)
 (* blockSub (B, s) = B'

     Invariant:
     If   G |- s : G'
     and  G' |- B block
     then G |- B' block
     and  B [s] == B'
  *)
  (* in front of substitutions, first case is irrelevant *)
  (* Sun Dec  2 11:56:41 2001 -fp *)
  and blockSub args =
    match args with
        (Bidx k, s) ->
          (match bvarSub (k, s) with
               Idx k' -> Bidx k'
             | Block b -> b)
      | (LVar (r, sk, _), s) when Option.isSome (!r) ->
          blockSub ((Option.get (!r)), comp (sk, s))
    (* -fp Sun Dec  1 21:18:30 2002 *)
    (* --cs Sun Dec  1 11:25:41 2002 *)
    (* Since always . |- t : Gsome, discard s *)
    (* where is this needed? *)
    (* Thu Dec  6 20:30:26 2001 -fp !!! *)
      | (LVar (r, sk, (l, t)), s) when r = (ref None) ->
          LVar(r, comp(sk, s), (l, t))
      (* was:
        LVar (r, comp(sk, s), (l, comp (t, s)))
        July 22, 2010 -fp -cs
       *)
        (* comp(^k, s) = ^k' for some k' by invariant *)
      | ((Inst uls), s') -> Inst (List.map (fun u -> EClo (u, s')) uls)
    (* this should be right but somebody should verify *)

  (* frontSub (Ft, s) = Ft'

     Invariant:
     If   G |- s : G'     G' |- Ft : V
     then Ft' = Ft [s]
     and  G |- Ft' : V [s]

     NOTE: EClo (U, s) might be undefined, so if this is ever
     computed eagerly, we must introduce an "Undefined" exception,
     raise it in whnf and handle it here so Exp (EClo (U, s)) -> Undef
  *)
  and frontSub args =
    match args with
        (Idx (n), s) -> bvarSub (n, s)
      | (Exp (u), s) -> Exp (EClo (u, s))
      | (Undef, s) -> Undef
      | (Block (b), s) -> Block (blockSub (b, s))
 (* decSub (x:V, s) = D'

     Invariant:
     If   G  |- s : G'    G' |- V : L
     then D' = x:V[s]
     and  G  |- V[s] : L
  *)
  (* First line is an optimization suggested by cs *)
  (* D[id] = D *)
  (* Sat Feb 14 18:37:44 1998 -fp *)
  (* seems to have no statistically significant effect *)
  (* undo for now Sat Feb 14 20:22:29 1998 -fp *)
  (*
  let decSub (D, Shift(0)) = D
    | decSub (Dec (x, V), s) = Dec (x, EClo (V, s))
  *)
  let decSub args =
    match args with
        (Dec (x, v), s) -> Dec (x, EClo (v, s))
      | (NDec x, s) -> NDec x
      | (BDec (n, (l, t)), s) -> BDec (n, (l, comp (t, s)))

  (* dot1 (s) = s'

     Invariant:
     If   G |- s : G'
     then s' = 1. (s o ^)
     and  for all V s.t.  G' |- V : L
          G, V[s] |- s' : G', V

     If s patsub then s' patsub
  *)
  (* first line is an optimization *)
  (* roughly 15% on standard suite for Twelf 1.1 *)
  (* Sat Feb 14 10:16:16 1998 -fp *)
  let dot1 s =
    match s with
        Shift (0) -> s
      | s -> Dot (Idx(1), comp(s, shift))
(* invDot1 (s) = s'
     invDot1 (1. s' o ^) = s'

     Invariant:
     s = 1 . s' o ^
     If G' |- s' : G
     (so G',V[s] |- s : G,V)
  *)
  let invDot1 (s) = comp (comp(shift, s), invShift)

(* EVar related functions *)

  (* newEVar (G, V) = newEVarCnstr (G, V, nil) *)
  let newEVar (g,v) = EVar(ref None, g, v, ref [])

  (* newAVar G = new AVar (assignable variable) *)
  (* AVars carry no type, ctx, or cnstr *)
  let newAVar () = AVar(ref None)

  (* newTypeVar (G) = X, X new
     where G |- X : type
  *)
  let newTypeVar (g) = EVar(ref None, g, Uni(Type), ref [])

  (* newLVar (l, s) = (l[s]) *)
  let newLVar (sk, (cid, t)) = LVar (ref None, sk, (cid, t))



(* Declaration Contexts *)

  (* ctxDec (G, k) = x:V
     Invariant:
     If      |G| >= k, where |G| is size of G,
     then    G |- k : V  and  G |- V : L
  *)
  let ctxDec (g, k) =
    let rec ctxDec' args =
      match args with
	  (Decl (g', Dec (x, v')), 1) -> Dec (x, EClo (v', Shift (k)))
        | (Decl (g', BDec (n, (l, s))), 1) -> BDec (n, (l, comp (s, Shift (k))))
        | (Decl (g', _), k') -> ctxDec' (g', k'-1)
         (* ctxDec' (Null, k')  should not occur by invariant *)
    in
    ctxDec' (g, k)
    
  (* blockDec (G, v, i) = V

     Invariant:
     If   G (v) = l[s]
     and  Sigma (l) = Some Gsome BLOCK Lblock
     and  G |- s : Gsome
     then G |- pi (v, i) : V
  *)

(*  let blockDec (g, (Bidx k) as v, i) =
    let BDec (_, (l, s)) = ctxDec (g, k)in
    (* G |- s : Gsome *)
    let (gsome, lblock) = conDecBlock (sgnLookup l) in
    let rec blockDec' args =
      match args with
	  (t, d :: l, 1, j) -> decSub (d, t)
        | (t, _ :: l, n, j) ->
            blockDec' (Dot (Exp (Root (Proj (v, j), Nil)), t),
                          l, n-1, j+1)
    in
    blockDec' (s, lblock, i, 1)
 *)   				    

				    
(* Definition related functions *)
  (* headOpt (U) = Some(H) or None, U should be strict, normal *)
  let rec headOpt arg =
    match arg with
        (Root (h, _)) -> Some(h)
      | (Lam (_, u)) -> headOpt u
      | _ -> None

  let ancestor' arg =
    match arg with
        (None) -> Anc(None, 0, None)
      | (Some(Const(c))) -> Anc(Some(c), 1, Some(c))
(*      | (Some(Def(d))) ->
          (match sgnLookup(d) with
               ConDef(_, _, _, _, _, _, Anc(_, height, cOpt)) -> Anc(Some(d), height+1, cOpt)) *)
      | (Some _) -> (* FgnConst possible, BVar impossible by strictness *)
          Anc(None, 0, None)
  (* ancestor(U) = ancestor info for d = U *)
  let ancestor (u) = ancestor' (headOpt u)

(*
  (* defAncestor(d) = ancestor of d, d must be defined *)
  let defAncestor (d) =
    (match sgnLookup(d) with
         ConDef(_, _, _, _, _, _, anc) -> anc)
*)

 (* Type related functions *)

  (* targetHeadOpt (V) = Some(H) or None
     where H is the head of the atomic target type of V,
     None if V is a kind or object or have variable type.
     Does not expand type definitions.
  *)
  (* should there possibly be a FgnConst case? also targetFamOpt -kw *)
  let rec targetHeadOpt arg =
    match arg with
        (Root (h, _)) -> Some(h)
      | (Pi(_, v)) -> targetHeadOpt v
      | (Redex (v, s)) -> targetHeadOpt v
      | (Lam (_, v)) -> targetHeadOpt v
      | (EVar (r,_,_,_)) when Option.isSome (!r) -> targetHeadOpt (Option.get (!r))
      | (EClo (v, s)) -> targetHeadOpt v
      | _ -> None
      (* Root(Bvar _, _), Root(FVar _, _), Root(FgnConst _, _),
         EVar(ref None,..), Uni, FgnExp _
      *)
      (* Root(Skonst _, _) can't occur *)
  (* targetHead (A) = a
     as in targetHeadOpt, except V must be a valid type
  *)
  let targetHead (a) = Option.get (targetHeadOpt a)

  (* targetFamOpt (V) = Some(cid) or None
     where cid is the type family of the atomic target type of V,
     None if V is a kind or object or have variable type.
     Does expand type definitions.
  *)
  let rec targetFamOpt arg =
    match arg with
        (Root (Const(cid), _)) -> Some(cid)
      | (Pi(_, v)) -> targetFamOpt v
      (*      | (Root (Def(cid), _)) -> targetFamOpt (constDef cid) *)
      | (Redex (v, s)) -> targetFamOpt v
      | (Lam (_, v)) -> targetFamOpt v
      | (EVar (r,_,_,_)) when Option.isSome (!r) -> targetFamOpt (Option.get (!r))
      | (EClo (v, s)) -> targetFamOpt v
      | _ -> None
      (* Root(Bvar _, _), Root(FVar _, _), Root(FgnConst _, _),
         EVar(ref None,..), Uni, FgnExp _
      *)
      (* Root(Skonst _, _) can't occur *)
  (* targetFam (A) = a
     as in targetFamOpt, except V must be a valid type
  *)
  let targetFam (a) = Option.get (targetFamOpt a)

  let rec exp_to_string e =
    match e with
        Uni u -> (match u with Type -> "uni(type)" | Kind -> "uni(kind)")
      | Pi((Dec(None,t),_),b) -> "pi("^(exp_to_string t)^" -> "^(exp_to_string b)^")"
      | Pi((Dec(Some(n),t),_),b) -> "pi("^n^" : "^(exp_to_string t)^". "^(exp_to_string b)^")"
      | Pi _ -> "pi"
      | Root(h,s)  ->
          let rec argStr s = 
            (match s with
                 Nil -> ""
               | App(e, Nil) -> exp_to_string e
               | App(e, s') -> (exp_to_string e) ^ ", "^ (argStr s')
               | SClo(s', Shift(0)) -> argStr s')
          in
          "root( "^ 
          (match h with
               BVar _ -> "bvar"
             | Const i -> "const("^(conDecName (sgnLookup i))^")"
             | Proj _ -> "proj"
             | Skonst _ -> "skonst"
             | Def _ -> "def"
             | NSDef _ -> "nsdef"
             | FVar (n,e,s) when s = id -> "fvar("^n^", "^(exp_to_string e)^", id)"
             | FVar (n,e,sub) -> "fvar("^n^", "^(exp_to_string e)^", "^(sub_to_string sub)^")"
             | FgnConst _ -> "fgnconst"
          )^(argStr s)^")"
      | Redex _ -> "redex"
      | Lam _ -> "lam"
      | EVar(r,ctx,e',c) ->
          "evar("^
          (match !r with
               Some(e'') -> "Some("^(exp_to_string e'')^")"
             | None -> "None"
          )^", ["^(ctx_to_string ctx)^"], "^(exp_to_string e')^", "^
          (match !c with
               [] -> "[]"
             | _ -> "cnstrs"
          )^")"
      | EVar (r,_,e',c) when !r = None && !c = [] -> "evar(None, "^(exp_to_string e')^")"
      | EVar (r,_,e',c) when Option.isSome !r && !c = [] -> "evar(Some("^(exp_to_string (Option.get !r))^"), "^(exp_to_string e')^")" 
      | EVar _ -> "evar"
      | EClo (e', sub) -> 
          "eclo("^(exp_to_string e')^", "^(sub_to_string sub)^")" 
      | AVar _ -> "avar"
      | FgnExp _ -> "fgnexp" 
      | NVar _ -> "nvar"

  and sub_to_string s =
    match s with
        Shift(k) -> "shift("^(string_of_int k)^")"
      | Dot(f, s') ->
          "dot("^
            (match f with 
                 Idx(k) -> "idx("^(string_of_int k)^")"
               | Exp(e) -> "exp("^(exp_to_string e)^")"
            ) ^", "^(sub_to_string s')^")"

  and ctx_to_string c =
    match c with
        Null -> "."
      | Decl(c', Dec(name_op,e)) -> 
          (match name_op with
               Some(name) -> name^" : "
             | None -> ""
          )^(exp_to_string e)^", "^(ctx_to_string c')
