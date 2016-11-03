  type cid = int			(* Constant identifier        *)
  type mid = int                        (* Structure identifier       *)
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
  let ctxPop (Decl (G, D)) = G

  (* ctxLookup (G, k) = D, kth declaration in G from right to left
     Invariant: 1 <= k <= |G|, where |G| is length of G
  *)

  let ctxLookup args =
    match args with
        (Decl (G', D), 1) -> D
      | ctxLookup (Decl (G', _), k') -> ctxLookup (G', k'-1)
(*    | ctxLookup (Null, k') = (print ("Looking up k' = " ^ Int.toString k' ^ "\n"); raise Error "Out of Bounce\n")*)
    (* ctxLookup (Null, k')  should not occur by invariant *)

  (* ctxLength G = |G|, the number of declarations in G *)
  let ctxLength G =
    let ctxLength' args =
      match args with
          (Null, n) -> n
	| (Decl(G, _), n)-> ctxLength' (G, n+1)
    in
    ctxLength' (G, 0)
    
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
  | BlockDec of string * mid option     (* %block l : SOME G1 PI G2   *)
              * dec ctx * dec list
  | BlockDef of string * mid option * cid list
                                        (* %block l = (l1 | ... | ln) *)
  | SkoDec of string * mid option * int	(* sa: K : kind  or           *)
              * exp * uni	        (* sc: A : type               *) *)

  and ancestor =			(* Ancestor of d or a         *)
    Anc of cid option * int * cid option (* head(expand(d)), height, head(expand[height](d)) *)
                                        (* NONE means expands to {x:A}B *)

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
  type cnstr = cnstr ref

  exception Error of string		(* raised if out of space     *)
(*
  (* standard operations on foreign expressions *)
  module FgnExpStd : struct
    (* convert to internal syntax *)
    module ToInternal = FgnOpnTable (type arg = unit type result = Exp)

    (* apply function to subterms *)
    module Map : FGN_OPN where type arg = exp -> exp
	                 where type result = exp

    (* apply function to subterms, for effect *)
    module App : FGN_OPN where type arg = exp -> unit
	                 where type result = unit

    (* test for equality *)
    module EqualTo : FGN_OPN where type arg = exp
                             where type result = bool

    (* unify with another term *)
    module UnifyWith : FGN_OPN where type arg = dec ctx * exp
                               where type result = fgnUnify

    (* fold a function over the subterms *)
    val fold : (csid * fgnExp) -> (exp * 'a -> 'a) -> 'a -> 'a
  end

  (* standard operations on foreign constraints *)
  module FgnCnstrStd : sig
    (* convert to internal syntax *)
    module ToInternal : FGN_OPN where type arg = unit
                                where type result = (dec ctx * exp) list

    (* awake *)
    module Awake : FGN_OPN where type arg = unit
                           where type result = bool

    (* simplify *)
    module Simplify : FGN_OPN where type arg = unit
                              where type result = bool
  end
*)

  let conDecName c =
    match c with
        (ConDec (name, _, _, _, _, _)) -> name
      | (ConDef (name, _, _, _, _, _, _)) -> name
      | (AbbrevDef (name, _, _, _, _, _)) -> name
      | (SkoDec (name, _, _, _, _)) -> name
      | (BlockDec (name, _, _, _)) -> name
      | (BlockDef (name, _, _)) -> name

  let conDecParent c =
    match c with
        (ConDec (_, parent, _, _, _, _)) -> parent
      | (ConDef (_, parent, _, _, _, _, _)) -> parent
      | (AbbrevDef (_, parent, _, _, _, _)) -> parent
      | (SkoDec (_, parent, _, _, _)) -> parent
      | (BlockDec (_, parent, _, _)) -> parent
      | (BlockDef (_, parent, _)) -> parent
   

  (* conDecImp (CD) -> k

     Invariant:
     If   CD is either a declaration, definition, abbreviation, or 
          a Skolem constant
     then k stands for the number of implicit elements.
  *)
  let conDecImp c = 
    match c with
        (ConDec (_, _, i, _, _, _)) -> i
      | (ConDef (_, _, i, _, _, _, _)) -> i
      | (AbbrevDef (_, _, i, _, _, _)) -> i
      | (SkoDec (_, _, i, _, _)) -> i
      | (BlockDec (_, _,  _, _)) -> 0   (* watch out -- carsten *)

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
        (ConDec (_, _, _, _, V, _)) -> V
      | (ConDef (_, _, _, _, V, _, _)) -> V
      | (AbbrevDef (_, _, _, _, V, _)) -> V
      | (SkoDec (_, _, _, V, _)) -> V


  (* conDecBlock (CD) ->  (Gsome, Lpi)

     Invariant:
     If   CD is block definition
     then Gsome is the context of some variables
     and  Lpi is the list of pi variables
  *)
  let conDecBlock (BlockDec (_, _, Gsome, Lpi)) = (Gsome, Lpi)

  (* conDecUni (CD) ->  L

     Invariant:
     If   CD is either a declaration, definition, abbreviation, or 
          a Skolem constant
     then L is the respective universe
  *)
  let conDecUni c =
    match c with
        (ConDec (_, _, _, _, _, L)) -> L
      | (ConDef (_, _, _, _, _, L, _)) -> L
      | (AbbrevDef (_, _, _, _, _, L)) -> L
      | (SkoDec (_, _, _, _, L)) -> L


  let strDecName (StrDec (name, _)) = name

  let strDecParent (StrDec (_, parent)) = parent

  val sgnReset     : unit -> unit
  val sgnSize      : unit -> cid * mid

  val sgnAdd   : conDec -> cid
  val sgnLookup: cid -> conDec
  val sgnApp   : (cid -> unit) -> unit

  val sgnStructAdd    : strDec -> mid
  val sgnStructLookup : mid -> strDec

  val constType   : cid -> exp		(* type of c or d             *)
  val constDef    : cid -> exp		(* definition of d            *)
  val constImp    : cid -> int
  val constStatus : cid -> status
  val constUni    : cid -> uni
  val constBlock  : cid -> dctx * dec list

  (* Declaration Contexts *)

  val ctxDec    : dctx * int -> dec	(* get variable declaration   *)
  val blockDec  : dctx * block * int -> dec 

  (* Explicit substitutions *)

  val id        : sub			(* id                         *)
  val shift     : sub			(* ^                          *)
  val invShift  : sub                   (* ^-1                        *)

  val bvarSub   : int * sub -> front    (* k[s]                       *)
  val frontSub  : front * sub -> front	(* H[s]                       *)
  val decSub    : dec * sub -> dec	(* x:V[s]                     *)
  val blockSub  : block * sub -> block  (* B[s]                       *)

  val comp      : sub * sub -> sub	(* s o s'                     *)
  val dot1      : sub -> sub		(* 1 . (s o ^)                *)
  val invDot1   : sub -> sub		(* (^ o s) o ^-1)             *)

  (* EVar related functions *)

  val newEVar    : dctx * exp -> exp	(* creates X:G|-V, []         *) 
  val newAVar    : unit ->  exp	        (* creates A (bare)           *) 
  val newTypeVar : dctx -> exp		(* creates X:G|-type, []      *)
  val newLVar    : sub * (cid * sub) -> block	
					(* creates B:(l[^k],t)        *) 

  (* Definition related functions *)
  val headOpt : exp -> head option
  val ancestor : exp -> ancestor
  val defAncestor : cid -> ancestor

  (* Type related functions *)

  (* Not expanding type definitions *)
  val targetHeadOpt : exp -> head option (* target type family or NONE *)
  val targetHead : exp -> head           (* target type family         *)

  (* expanding type definitions *)
  val targetFamOpt : exp -> cid option  (* target type family or NONE *)
  val targetFam : exp -> cid            (* target type family         *)

  (* Used in Flit *)
  val rename : cid * string -> unit

