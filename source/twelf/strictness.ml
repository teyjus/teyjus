(** Implements the strictness check for translating LF types. *)

(** Check that the argument list contains only unique bound 
    variables. *)
let rec check_args tmlist bndrlist =
  let same_name id1 id2 = (Lfabsyn.get_id_name id1) = (Lfabsyn.get_id_name id2) in
  let match_term tm1 tm2 = 
     match tm1, tm2 with
         Lfabsyn.IdTerm(id1), Lfabsyn.IdTerm(id2)
       | Lfabsyn.IdTerm(id1), Lfabsyn.AppTerm(id2,[])
       | Lfabsyn.AppTerm(id1,[]), Lfabsyn.IdTerm(id2)
       | Lfabsyn.AppTerm(id1,[]), Lfabsyn.AppTerm(id2,[]) -> same_name id1 id2
       | _ -> false
  in
  match tmlist with
      [] -> true
    | ((Lfabsyn.IdTerm(id) as tm) :: tms)
    | ((Lfabsyn.AppTerm(id,[]) as tm) :: tms) -> 
         (not (List.exists (match_term tm) tms)) && (List.exists (same_name id) bndrlist)
                                 && (check_args tms bndrlist)
    | _ -> false

(** Look for strict occurences of the given variable in the given 
    type. Collects bound variables in third argument. *)
let rec appears_strict_ty id ty bndrs =
  match ty with
      Lfabsyn.PiType(name,typ,body) ->
        (appears_strict_ty id typ bndrs) || 
        (appears_strict_ty id body (name :: bndrs))
    | Lfabsyn.AppType(name, tms) ->
        let check =
          if (Lfabsyn.get_id_name name) = (Lfabsyn.get_id_name id)
          then check_args tms bndrs
          else false
        in
        check ||
          (List.exists (fun x -> appears_strict_tm id x bndrs) tms)
    | Lfabsyn.ImpType(l,r) ->
        (appears_strict_ty id l bndrs) ||
        (appears_strict_ty id r bndrs)
    | Lfabsyn.IdType(name) ->
        match (name, id) with
             (Lfabsyn.Var(v1,_), Lfabsyn.Var(v2,_)) 
           | (Lfabsyn.LogicVar(v1,_), Lfabsyn.LogicVar(v2,_)) when (v1 = v2) ->
              true
          | _ ->
              false

(** Look for strict occurences of the given variable in the given 
    term. Collects bound variables in third argument. *)
and appears_strict_tm id tm bndrs =
  match tm with
      Lfabsyn.AbsTerm(name,ty,body) ->
        appears_strict_tm id body (name :: bndrs)
    | Lfabsyn.AppTerm(name,tms) ->
        let check =
          if (Lfabsyn.get_id_name name) = (Lfabsyn.get_id_name id)
          then check_args tms bndrs
          else false
        in
        check ||
          (List.exists (fun x -> appears_strict_tm id x bndrs) tms)
    | Lfabsyn.IdTerm(name) ->
        match (name, id) with
             (Lfabsyn.Var(v1,_), Lfabsyn.Var(v2,_)) 
           | (Lfabsyn.LogicVar(v1,_), Lfabsyn.LogicVar(v2,_)) when (v1 = v2) ->
               true
           | _ ->
             false

(** Checks if the given variable appears strictly in the given
    type. *)
let appears_strict id ty =
  match id with
      Lfabsyn.Const(_) ->
        Errormsg.warning Errormsg.none 
                         ("Attempting to check strictness of a constant " ^ (Lfabsyn.string_of_id id) ^ 
                          " in term " ^ (Lfabsyn.string_of_typ ty)); true
    | Lfabsyn.Var(name,_)
    | Lfabsyn.LogicVar(name,_) ->
        appears_strict_ty id ty []
      
