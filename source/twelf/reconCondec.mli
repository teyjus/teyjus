  exception Error of string

  val condecToConDec : ExtSyn.condec * Paths.location * bool -> IntSyn.conDec option * Paths.occConDec option
                     (* optional ConDec is absent for anonymous definitions *)
                     (* bool = true means that condec is an abbreviation *)
