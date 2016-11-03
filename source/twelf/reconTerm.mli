  exception Error of string
  val resetErrors : string -> unit      (* filename -fp *)
  val checkErrors : Paths.region -> unit

  type traceMode = Progressive | Omniscient
  val trace : bool ref
  val traceMode : traceMode ref

  (* Reconstruction jobs *)
  type job

  val jnothing : job
  val jand : job * job -> job
  val jwithctx : dec IntSyn.Ctx * job -> job
  val jterm : term -> job
  val jclass : term -> job
  val jof : term * term -> job
  val jof' : term * IntSyn.Exp -> job

  type job' =
      JNothing'
    | JAnd of job' * job'
    | JWithCtx of IntSyn.Dec IntSyn.Ctx * job
    | JTerm of (IntSyn.Exp * Paths.occExp) * IntSyn.Exp * IntSyn.Uni
    | JClass of (IntSyn.Exp * Paths.occExp) * IntSyn.Uni
    | JOf of (IntSyn.Exp * Paths.occExp) * (IntSyn.Exp * Paths.occExp) * IntSyn.Uni

  val recon : job -> job'
  val reconQuery : job -> job'
  val reconWithCtx : IntSyn.dctx * job -> job'
  val reconQueryWithCtx : IntSyn.dctx * job -> job'

  val termRegion : term -> Paths.region
  val decRegion : dec -> Paths.region
  val ctxRegion : dec IntSyn.Ctx -> Paths.region option
                  
  (* unimplemented for the moment *)
  val internalInst : 'a -> 'b
  val externalInst : 'a -> 'b
