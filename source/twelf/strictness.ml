(** Implements the strictness check for translating LF types. *)

(** Check that the argument list contains only unique bound 
    variables. *)
let rec check_args tmlist bndrlist =
  match tmlist with
      [] -> true
    | ((Lfabsyn.IdTerm(id,_) as tm) :: tms) -> 
         (not (List.mem tm tms)) && (List.mem id bndrlist)
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
          (match (name, id) with
              (Lfabsyn.Var(v1,_), Lfabsyn.Var(v2,_)) when (v1 = v2)->
                 check_args tms bndrs
             | _ -> false)
        in check ||
           (List.exists (fun x -> appears_strict_tm id x bndrs) tms)
    | Lfabsyn.ImpType(l,r,_) ->
        (appears_strict_ty id l bndrs) ||
        (appears_strict_ty id r bndrs)
    | Lfabsyn.IdType(name,_) ->
        match (name, id) with
            (Lfabsyn.Var(v1,_), Lfabsyn.Var(v2,_)) when (v1 = v2) ->
               true
           | _ ->
             false

(** Look for strict occurences of the given variable in the given 
    term. Collects bound variables in third argument. *)
and appears_strict_tm id tm bndrs =
  match tm with
      Lfabsyn.AbsTerm(name,ty,body,_) ->
        false
    | Lfabsyn.AppTerm(name,tms,_) ->
        let check =
          (match (name, id) with
              (Lfabsyn.Var(v1,_), Lfabsyn.Var(v2,_)) when (v1 = v2) ->
                 check_args tms bndrs
             | _ -> false)
        in check ||
           (List.exists (fun x -> appears_strict_tm id x bndrs) tms)
    | Lfabsyn.IdTerm(name,_) ->
        match (name, id) with
            (Lfabsyn.Var(v1,_), Lfabsyn.Var(v2,_)) when (v1 = v2) ->
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
    | Lfabsyn.Var(name,_) ->
        appears_strict_ty id ty []
