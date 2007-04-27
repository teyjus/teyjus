(****************************************************************************)
(* the input channel                                                        *)
(****************************************************************************)
val openInChannel : string -> unit
val closeInChannel : unit  -> unit

(***************************************************************************)
(* read functions                                                          *)
(***************************************************************************)
val readOneByte :  unit -> int    (* read one byte *)
val readTwoBytes : unit -> int    (* read two bytes *)
val readWord :     unit -> int    (* read a word *)
val readString :   unit -> string (* read a string *)

val skipNBytes :   int -> unit    (* skip n bytes *)
val skipNWords :   int -> unit    (* skip n words *)
