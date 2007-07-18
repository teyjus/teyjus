(***************************************************************************)
(* Implement a mapping from code offset to labels                          *)
(***************************************************************************)
type label = string

val label : int -> label
val assignLabel : int -> string -> unit
val addLabel : int -> unit
val findLabel : int -> string
