type typeandenvironment =
  TypeAndEnvironment of (Absyn.atype * int)

val translate : Preabsyn.pmodule -> Preabsyn.pmodule -> (Absyn.amodule * Absyn.amodule)
val translateType : Preabsyn.ptype -> Absyn.amodule -> Absyn.atype