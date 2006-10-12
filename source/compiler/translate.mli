type typeandenvironment =
  TypeAndEnvironment of (Absyn.atype * int * bool)

val translate : Preabsyn.pmodule -> Preabsyn.pmodule -> Absyn.amodule
val translateType : Preabsyn.ptype -> Absyn.amodule -> Absyn.atype