(**********************************************************************
*Errormsg Module:
*	In the short run this is used simply to output meaningful error
*	messages with respect to character and line position.  In the long
*	run this will eventually allow for such things as specifying warnings
*	to ignore and the like.
*
*	This has not yet been tested almost at all.
**********************************************************************)
exception InternalError

type pos = int

val newLine : int -> unit
val anyErrors : bool ref
val fileName : string ref
val linePos : int list ref
val log : int -> string -> unit
val warning : int -> string -> unit
val error : int -> string -> unit
val impossible : int -> string -> 'a   (* raises InternalError *)
val reset : unit -> unit
