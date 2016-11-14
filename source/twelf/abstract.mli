(* This is taken from the Twelf implementation *)

  exception Error of string

  val piDepend  : (IntSyn.dec * IntSyn.depend) * IntSyn.exp -> IntSyn.exp
  val closedDec : IntSyn.dec IntSyn.ctx * (IntSyn.dec * IntSyn.sub) -> bool
  val closedSub : IntSyn.dec IntSyn.ctx * IntSyn.sub -> bool
  val closedExp : IntSyn.dec IntSyn.ctx * (IntSyn.exp * IntSyn.sub) -> bool
  val closedCtx : IntSyn.dec IntSyn.ctx -> bool

  val abstractDecImp : IntSyn.exp  -> (int * IntSyn.exp)
  val abstractDef : (IntSyn.exp * IntSyn.exp)
                     -> (int * (IntSyn.exp * IntSyn.exp))
  val abstractCtxs : (IntSyn.dec IntSyn.ctx) list
                     -> (IntSyn.dec IntSyn.ctx) * (IntSyn.dec IntSyn.ctx) list
  val abstractSpine : IntSyn.spine * IntSyn.sub -> (IntSyn.dctx * IntSyn.spine)

  val collectEVars : IntSyn.dctx * IntSyn.eclo * IntSyn.exp list -> IntSyn.exp list
  val collectEVarsSpine : IntSyn.dctx * (IntSyn.spine * IntSyn.sub) * IntSyn.exp list -> IntSyn.exp list
                         

  val raiseTerm    : IntSyn.dctx * IntSyn.exp -> IntSyn.exp
  val raiseType    : IntSyn.dctx * IntSyn.exp -> IntSyn.exp
