(* This is taken from the Twelf implementation *)


  exception Error of string

  (* error (r, msg) raises a syntax error within region r with text msg *)
  let error (r, msg) = raise (Error (Paths.wrap (r, msg)))

  (* freeVar (XOpt, [(X1,"X1"),...,(Xn,"Xn")]) = true
     iff XOpt = SOME("Xi"), false otherwise
  *)
  let freeVar args =
    match args with 
      (Some(name), xs) ->
        List.exists (fun (_, name') -> name = name') xs
    | _ -> false


  (* queryToQuery (q) = (V, XOpt, [(X1,"X1"),...,(Xn,"Xn")])
     where XOpt is the optional proof term variable
           X1,...,Xn are the free EVars in the terms with their names
 
     Free variables in q are interpreted existentially (as EVars).

     Only works properly when the Vars parameter structure
     is instantiated to EVars, not FVars.
  *)
  (* call TypeCheck... if !doubleCheck = true? *)
  (* Wed May 20 08:00:28 1998 -fp *)
  let queryToQuery (ExtSyn.Query (optName, tm), Paths.Loc (fileName, r)) = 
        (* construct an external term for the result of the query
        val res = (case optName
                     of NONE => ReconTerm.omitted (r)
                      | SOME name => ReconTerm.evar (name, r)) *)
    let _ = Names.varReset IntSyn.Null in
    let _ = ReconTerm.resetErrors fileName in
    let ReconTerm.JClass ((v, oc), l) =
          ReconTerm.reconQuery (ReconTerm.jclass tm) in
    let _ = ReconTerm.checkErrors (r) in
    let _ = (match l with
                  IntSyn.Type -> ()
                | _ -> error (r, "Query was not a type"))
    in
    let xs = Names.namedEVars () in
         (* ??? Since the reconstruction of a query is subject to constraints,
           couldn't optName "occur" in a constraint involving the type
           without being detected by this test?  -kw *)
    let _ = if freeVar (optName, xs)
            then error (r,
	                "Proof term variable " ^ Option.get optName
		        ^ " occurs in type")
	    else ()
    in
    (v, optName, xs)
      
