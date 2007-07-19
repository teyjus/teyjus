type tjcommand =
	Tjcc
  | Tjsim
  | Tjdis
  | Teyjus

(* parse command function *)
val parseArgs : tjcommand -> bool



val outputFileName : string ref
val inputName : string ref

(** ******************************************************************* **)
(**                            tjdis                                    **)
(** ******************************************************************* **)
val tablesOnly : bool ref
val instrOnly  : bool ref
val linkedFile : bool ref

(** ******************************************************************* **)
(**                            tjsim                                    **)
(** ******************************************************************* **)
(* heap size *)
val heapSize : int ref
(* maximum number of solutions *)
val maxSolutions : int ref
(* minimum number of solutions *)
val minSolutions : int ref 
(* batch *)
val batch : bool ref
(* quiet *)
val quiet : bool ref

(* queries *)
val popQuery : unit -> string


