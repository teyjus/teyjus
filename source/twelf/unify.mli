(* This is taken from the Twelf implementation *)

(* type unifTrail *)

(*	 
  (* suspending and resuming trailing *)
  val suspend : unit -> unifTrail
  val resume : unifTrail  -> unit

  (* trailing of variable instantiation *)

  val reset       : unit -> unit
  val mark   : unit -> unit
  val unwind : unit -> unit
*)
	  
  val instantiateEVar : IntSyn.exp option ref * IntSyn.exp * IntSyn.cnstrRef list -> unit
  val instantiateLVar : IntSyn.block option ref * IntSyn.block -> unit

  val resetAwakenCnstrs : unit -> unit
  val nextCnstr : unit -> IntSyn.cnstrRef option
  val addConstraint : IntSyn.cnstrRef list ref * IntSyn.cnstrRef -> unit
  val solveConstraint : IntSyn.cnstrRef -> unit

  val delay : IntSyn.eclo * IntSyn.cnstrRef -> unit

  (* unification *)

  val intersection : IntSyn.sub * IntSyn.sub -> IntSyn.sub

  exception Unify of string

  val unify : IntSyn.dctx * IntSyn.eclo * IntSyn.eclo -> unit   (* raises Unify *)
  val unifyW : IntSyn.dctx * IntSyn.eclo * IntSyn.eclo -> unit (* raises Unify *)

  val unifyBlock : IntSyn.dctx * IntSyn.block * IntSyn.block -> unit (* raises Unify *)

  val unifySub : IntSyn.dctx * IntSyn.sub * IntSyn.sub -> unit  (* raises Unify *)


  val invertible : IntSyn.dctx * IntSyn.eclo * IntSyn.sub * IntSyn.exp option ref -> bool
  val invertSub : IntSyn.dctx * IntSyn.sub * IntSyn.sub * IntSyn.exp option ref -> IntSyn.sub

  (* unifiable (G, Us,Us') will instantiate EVars as an effect *)
  val unifiable : IntSyn.dctx * IntSyn.eclo * IntSyn.eclo -> bool

  (* unifiable' (G, Us,Us') is like unifiable, but returns NONE for
     success and SOME(msg) for failure *)
  val unifiable' : IntSyn.dctx * IntSyn.eclo * IntSyn.eclo -> string option
