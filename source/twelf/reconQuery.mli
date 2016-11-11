(* This is taken from the Twelf implementation *)

  exception Error of string

  val queryToQuery : ExtSyn.query * Paths.location
                     -> IntSyn.exp * string option * (IntSyn.exp * string) list
                     (* (A, SOME("X"), [(Y1, "Y1"),...] *)
		     (* where A is query type, X the optional proof term variable name *)
		     (* Yi the EVars in the query and "Yi" their names *)
