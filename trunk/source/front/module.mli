(************************************************************************)
(*                     global module table                              *)
(************************************************************************)
type moduletableentry = ModuleTableEntry of (string * Absyn.amodule)

(* top-level module *)
val topModule : moduletableentry

(***************************************************************************)
(*                       current module                                    *)
(***************************************************************************)


(*(* load a module *)
val loadModule : string -> unit

(* unload a module *)
val unloadModule : string -> unit

(* open a module context *)
(* val openModule : string -> unit *)
*)
