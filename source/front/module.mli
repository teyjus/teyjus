(***************************************************************************)
(*            link and load a module                                       *)
(***************************************************************************)
val moduleLoad : string -> unit
val setPath    : string -> unit

(***************************************************************************)
(*       install and open a module context                                 *)
(* 1. search the ocaml module table to find the module and its index;      *)
(* 2. register the module as current module;                               *)
(* 3. invoke C functions to install the module and open its context for the*)
(*    simulator.                                                           *)
(***************************************************************************)
val moduleInstall     : string -> unit
val initModuleContext : unit   -> unit
val getCurrentModule  : unit   -> Absyn.amodule
