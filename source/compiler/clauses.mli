type symbol = Symbol.symbol
type pos = Errormsg.pos

type closeddefinition = (Absyn.aconstant * Absyn.aterm) list	

val translateClauses : Preabsyn.pmodule -> Absyn.amodule -> 
  (Absyn.amodule * Absyn.aterm list * Absyn.aterm list * closeddefinition)


val printTranslatedClauses : Absyn.aterm list -> Absyn.aterm list -> 
  out_channel -> unit
val unitTests : unit -> unit
