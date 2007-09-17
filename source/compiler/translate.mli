(**********************************************************************
*Translate:
**********************************************************************)

(**********************************************************************
*translate:
* Given a preabsyn module and a preabsyn signature, translates the
* kind and constant declarations in the module and signature and
* produces absyn module and signature representations containing the
* kind and constant tables.
**********************************************************************)
val translate : Preabsyn.pmodule -> Preabsyn.pmodule -> (Absyn.amodule * Absyn.amodule)

(**********************************************************************
*translateType:
* Given a preabsyn type and an absyn module containing kind and constant
* tables, produces an absyn representation of the type.
**********************************************************************)
val translateType : Preabsyn.ptype -> Absyn.amodule -> Absyn.atype