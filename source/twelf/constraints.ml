(* This is taken from the Twelf implementation *)

  (*! structure IntSyn = IntSyn' !*)

  exception Error of IntSyn.cnstrRef list

  (*
     Constraints cnstr are of the form (X<I>[s] = U).
     Invariants:
       G |- s : G'  G' |- X<I> : V
       G |- U : W
       G |- V[s] == W : L
       (X<>,s) is whnf, but X<>[s] is not a pattern
     If X<I> is uninstantiated, the constraint is inactive.
     If X<I> is instantiated, the constraint is active.

     Constraints are attached directly to the EVar X
     or to a descendent  -fp?
  *)
  
    (* simplify cnstrs = cnstrs'
       Effects: simplifies the constraints in cnstrs by removing constraints
         of the form U = U' where G |- U == U' : V (mod beta/eta)
         Neither U nor U' needs to be a pattern
	 *)
  let rec simplify l =
    match l with
        [] -> []
      | (r :: cnstrs) when (!r) = IntSyn.Solved ->
          simplify cnstrs
      | (c :: cnstrs) ->
	  (match (!c) with 
  	    IntSyn.Eqn(g,u1,u2) ->
              if Conv.conv ((u1, IntSyn.id), (u2, IntSyn.id))
              then simplify cnstrs
              else c :: (simplify cnstrs)
	  | (IntSyn.FgnCnstr (id,csfc)) ->
              if IntSyn.FgnCnstrStd.Simplify.apply (id,csfc) ()
              then simplify cnstrs
              else c :: (simplify cnstrs))

  let rec namesToString l =
    match l with
        (name :: []) -> name ^ "."
      | (name::names) -> name ^ ", " ^ namesToString names

  let warnConstraints l =
    match l with 
        [] -> ()
      | names -> print_string ("Constraints remain on " ^ namesToString names ^ "\n")


