(***************************************************************************)
(* This module specifies the global module table used for the compiler.    *)
(***************************************************************************)

val initModuleTable : unit -> unit
val enterModuleTable : Absyn.amodule -> unit
val findModule : string -> Absyn.amodule
val removeModule : string -> unit
