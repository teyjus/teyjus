(****************************************************************************)
(* process clause representations in a module, annotate type and term       *)
(* variables as temporary or permanent, and decide offset for permanent     *)
(* variables. Fill in variable related information for clause               *)
(* representations: hasenv, cutvar and goal-environment size association.   *)
(****************************************************************************)
val processClauses: Absyn.amodule -> unit
