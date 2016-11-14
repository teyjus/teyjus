(* This is taken from the Twelf implementation *)

  (* Patterns *)
  val isPatSub : IntSyn.sub -> bool
  val makePatSub : IntSyn.sub -> IntSyn.sub option
  val dotEta   : IntSyn.front * IntSyn.sub -> IntSyn.sub

  exception Eta
  val etaContract : IntSyn.exp -> int   (* can raise Eta *)

  (* Weak head normalization *)
  val whnf : IntSyn.eclo -> IntSyn.eclo
(*  val expandDef : IntSyn.eclo -> IntSyn.eclo *)
  val whnfExpandDef : IntSyn.eclo -> IntSyn.eclo
  val etaExpandRoot : IntSyn.exp -> IntSyn.exp
  val whnfEta : (IntSyn.eclo * IntSyn.eclo) -> (IntSyn.eclo * IntSyn.eclo)
  val lowerEVar : IntSyn.exp -> IntSyn.exp

  val newLoweredEVar : IntSyn.dctx * IntSyn.eclo -> IntSyn.exp
  val newSpineVar : IntSyn.dctx * IntSyn.eclo -> IntSyn.spine
  val spineToSub : IntSyn.spine * IntSyn.sub -> IntSyn.sub

  (* Full normalization *)
  val normalize: IntSyn.eclo -> IntSyn.exp
  val normalizeDec: IntSyn.dec * IntSyn.sub -> IntSyn.dec
  val normalizeCtx: IntSyn.dctx -> IntSyn.dctx

  (* Inverting substitutions *)
  val invert : IntSyn.sub -> IntSyn.sub
  val strengthen: IntSyn.sub * IntSyn.dctx -> IntSyn.dctx
  val isId : IntSyn.sub -> bool

  val cloInv : IntSyn.exp * IntSyn.sub -> IntSyn.exp
  val compInv : IntSyn.sub * IntSyn.sub -> IntSyn.sub
