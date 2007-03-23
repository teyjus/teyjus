(****************************************************************************)
(* process clause representations in a module, annotate type and term       *)
(* variables as temporary or permanent, and decide offset for permanent     *)
(* variables. Enhance clause representations with variable related          *)
(* information.                                                             *)
(****************************************************************************)
(*val processClauses: Absyn.amodule -> Absyn.amodule*)
val processClauses: Absyn.amodule -> unit
