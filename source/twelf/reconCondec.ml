(* This is taken from the Twelf implementation *)

  exception Error of string

  (* error (r, msg) raises a syntax error within region r with text msg *)
  fun error (r, msg) = raise Error (Paths.wrap (r, msg))

    (* condecToConDec (condec, r) = (SOME(cd), SOME(ocd))
     if condec is a named constant declaration with occurrence tree ocd,
     NONE if name or occurrence tree is missing

     Free variables in condec are interpreted universally (as FVars)
     then abstracted as implicit parameters.

     Only works properly when the declaration contains no EVars.
  *)
  (* should printing of result be moved to frontend? *)
  (* Wed May 20 08:08:50 1998 -fp *)
  let condecToConDec c =
    match c with 
        (condec(name, tm), Paths.Loc (fileName, r), abbFlag) ->
          let _ = Names.varReset IntSyn.Null in
	  let _ = ReconTerm.resetErrors fileName in
          let ReconTerm.JClass ((V, oc), L) =
(* Not implementing timers yet so call reconstruction directly *)
(*              (Timers.time Timers.recon ReconTerm.recon) (ReconTerm.jclass tm) in *)
            RecoNTerm.recon (ReconTerm.jclass tm) in
	  let _ = ReconTerm.checkErrors (r) in
(* again, not implementing timers yet *)
(*          let (i, V') = try (Timers.time Timers.abstract Abstract.abstractDecImp) V
	                with Abstract.Error (msg)
			       -> raise Abstract.Error (Paths.wrap (r, msg)) in *)
          let (i, V') = try Abstract.abstractDecImp V
                        with Abstract.Error (msg) -> raise Abstract.Error (Paths.wrap (r, msg)) in
	  let cd = Names.nameConDec (IntSyn.ConDec (name, NONE, i, IntSyn.Normal, V', L)) in
	  let ocd = Paths.dec (i, oc) in
(*	  let _ = if !Global.chatter >= 3
		  then Msg.message ((Timers.time Timers.printing Print.conDecToString) cd ^ "\n")
		else () in *)
(* no printing ... yet! *)
(*          let _ = if !Gloval.chatter >= 3
                  then Msg.message ((Print.conDecToString cd) ^ "\n") *)
(* Not implementing the extra type checking pass yet *)
(*	  let _ = if !Global.doubleCheck
		  then (Timers.time Timers.checking TypeCheck.check) (V', IntSyn.Uni L)
		else () in *)
	  (SOME(cd), SOME(ocd))
