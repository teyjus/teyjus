(** Implements the strictness check for translating LF types. *)

(** Check that the argument list contains only unique bound 
    variables. *)
let rec check_args tmlist bndrlist =
  let same_name id1 id2 = (Lfabsyn.get_id_name id1) = (Lfabsyn.get_id_name id2) in
  let match_term tm1 tm2 = 
     match tm1, tm2 with
         Lfabsyn.IdTerm(id1,_), Lfabsyn.IdTerm(id2,_)
       | Lfabsyn.IdTerm(id1,_), Lfabsyn.AppTerm(id2,[],_)
       | Lfabsyn.AppTerm(id1,[],_), Lfabsyn.IdTerm(id2,_)
       | Lfabsyn.AppTerm(id1,[],_), Lfabsyn.AppTerm(id2,[],_) -> same_name id1 id2
       | _ -> false
  in
  match tmlist with
      [] -> true
    | ((Lfabsyn.IdTerm(id,_) as tm) :: tms)
    | ((Lfabsyn.AppTerm(id,[],_) as tm) :: tms) -> 
         (not (List.exists (match_term tm) tms)) && (List.exists (same_name id) bndrlist)
                                 && (check_args tms bndrlist)
    | _ -> false

(** Look for strict occurences of the given variable in the given 
    type. Collects bound variables in third argument. *)
let rec appears_strict_ty id ty bndrs =
  match ty with
      Lfabsyn.PiType(name,typ,body,_) ->
        (appears_strict_ty id typ bndrs) || 
        (appears_strict_ty id body (name :: bndrs))
    | Lfabsyn.AppType(name, tms,_) ->
        let check =
          if (Lfabsyn.get_id_name name) = (Lfabsyn.get_id_name id)
          then check_args tms bndrs
          else false
        in
        check ||
          (List.exists (fun x -> appears_strict_tm id x bndrs) tms)
    | Lfabsyn.ImpType(l,r,_) ->
        (appears_strict_ty id l bndrs) ||
        (appears_strict_ty id r bndrs)
    | Lfabsyn.IdType(name,_) ->
        match (name, id) with
            (Lfabsyn.LogicVar(v1,_,_), Lfabsyn.LogicVar(v2,_,_)) when (v1 = v2) ->
              true
          | _ ->
              false

(** Look for strict occurences of the given variable in the given 
    term. Collects bound variables in third argument. *)
and appears_strict_tm id tm bndrs =
  match tm with
      Lfabsyn.AbsTerm(name,ty,body,_) ->
        appears_strict_tm id body (name :: bndrs)
    | Lfabsyn.AppTerm(name,tms,_) ->
        let check =
          if (Lfabsyn.get_id_name name) = (Lfabsyn.get_id_name id)
          then check_args tms bndrs
          else false
        in
        check ||
          (List.exists (fun x -> appears_strict_tm id x bndrs) tms)
    | Lfabsyn.IdTerm(name,_) ->
        match (name, id) with
             (Lfabsyn.LogicVar(v1,_,_), Lfabsyn.LogicVar(v2,_,_)) when (v1 = v2) ->
               true
           | _ ->
             false

(** Checks if the given variable appears strictly in the given
    type. *)
let appears_strict id ty =
  match id with
      Lfabsyn.Const(_,_) ->
        Errormsg.warning Errormsg.none 
                         ("Attempting to check strictness of a constant " ^ (Lfabsyn.string_of_id id) ^ 
                          " in term " ^ (Lfabsyn.string_of_typ ty)); true
    | Lfabsyn.Var(name,_,_)
    | Lfabsyn.LogicVar(name,_,_) ->
        appears_strict_ty id ty []
      
