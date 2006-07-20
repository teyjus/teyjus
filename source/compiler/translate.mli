type pos = Errormsg.pos

type typeandenvironment =
  TypeAndEnvironment of (Absyn.atype * int * bool)

val translate : Preabsyn.pmodule -> Preabsyn.pmodule -> Absyn.amodule
