  val chatter : int ref
  val style : int ref
  val maxCid : int
  val maxMid : int
  val maxCSid : int
  val doubleCheck : bool ref
  val unsafe : bool ref
  val autoFreeze : bool ref
  val chPrint : int -> (unit -> string) -> unit
  val chMessage : int -> (unit -> string) -> (string -> unit) -> unit
(*  val timeLimit : (Span.t option) ref   (* in seconds *) *)
