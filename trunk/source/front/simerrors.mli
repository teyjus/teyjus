exception Abort         (* exit the executable immediately *)
exception Exit          (* exit *)
exception TopLevel      (* return to the toplevel *)
exception Query         (* abort solving the query *)
exception QueryResult   (* query is solved; print answer *)
exception Fail          (* fail to simulator level *)
exception FatalError   

(* translate the simulator exceptions (c) into system exceptions (ocaml) *)
(* the expNumbers have to agree with those defined in system/error.h     *)
val handleSimExceptions : int -> unit
