exception Abort         (* exit the executable immediately *)
exception Exit          (* exit *)
exception TopLevel      (* return to the toplevel *)
exception Query         (* abort solving the query *)
exception QueryResult   (* query is solved; print answer *)
exception Fail          (* fail to simulator level *)

exception FatalError   

(* translate the simulator exceptions (c) into system exceptions (ocaml) *)
(* the expNumbers have to agree with those defined in system/error.h     *)
let handleSimExceptions expNumber =
  if expNumber = 0 then () (* no error *)
  else if expNumber = 1 then raise Abort
  else if expNumber = 2 then raise Exit
  else if expNumber = 3 then raise TopLevel
  else if expNumber = 4 then raise Query 
  else if expNumber = 5 then raise QueryResult
  else if expNumber = 6 then raise Fail
  else raise FatalError
