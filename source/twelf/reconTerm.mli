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
  val jwithctx : ExtSyn.dec IntSyn.ctx * job -> job
  val jterm : ExtSyn.term -> job
  val jclass : ExtSyn.term -> job
  val jof : ExtSyn.term * ExtSyn.term -> job
  val jof' : ExtSyn.term * IntSyn.exp -> job

  type job' =
      JNothing
    | JAnd of job' * job'
    | JWithCtx of IntSyn.dec IntSyn.ctx * job'
    | JTerm of (IntSyn.exp * Paths.occExp) * IntSyn.exp * IntSyn.uni
    | JClass of (IntSyn.exp * Paths.occExp) * IntSyn.uni
    | JOf of (IntSyn.exp * Paths.occExp) * (IntSyn.exp * Paths.occExp) * IntSyn.uni

  val recon : job -> job'
  val reconQuery : job -> job'
