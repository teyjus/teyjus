exception Abort         (* exit the executable immediately *)
exception Exit          (* exit *)
exception TopLevel      (* return to the toplevel *)
exception Query         (* abort solving the query *)
exception QueryResult   (* query is solved; print answer *)
exception Fail          (* fail to simulator level *)
exception FatalError   


val handleSimExceptions : int -> int
