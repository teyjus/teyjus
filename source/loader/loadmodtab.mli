(************************************************************************)
(* The functions in this module are in charge of setting up a compiler  *)
(* module abstract syntax (constant and kind symbol tables) from a      *)
(* bytecode file upon loading.                                          *)
(************************************************************************) 
val loadModuleTable : string -> Absyn.amodule
