  (*! structure IntSyn = IntSyn' !*)

  exception Error of IntSyn.cnstr list

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
  let simplify l =
    match l with
        [] -> []
      | ((ref IntSyn.Solved) :: cnstrs) =
          simplify cnstrs
      | ((Eqn as ref (IntSyn.Eqn (G, U1, U2))) :: cnstrs) =
        if Conv.conv ((U1, IntSyn.id), (U2, IntSyn.id))
        then simplify cnstrs
        else Eqn :: (simplify cnstrs)
      | ((FgnCnstr as ref (IntSyn.FgnCnstr csfc)) :: cnstrs) =
        if IntSyn.FgnCnstrStd.Simplify.apply csfc ()
        then simplify cnstrs
        else FgnCnstr :: (simplify cnstrs)

  let namesToString l =
    match l with
        (name :: []) -> name ^ "."
      | (name::names) -> name ^ ", " ^ namesToString names

  let warnConstraints l =
    match l with 
        [] -> ()
      | names -> print_string ("Constraints remain on " ^ namesToString names ^ "\n")


