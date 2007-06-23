type tjcommand =
	Tjcc
  | Tjsim
  | Tjdis
  | Teyjus

val outputFileName : string ref
val inputName : string ref

(* needed for tjdis *)
val tablesOnly : bool ref
val instrOnly : bool ref

(* needed for tjsim *)
val heapSize : int ref


(* parse command function *)
val parseArgs : tjcommand -> bool
