(** a solution consists of a substitution and a disagreement set. *)
type lpsolution = (Absyn.atypesymbol * Absyn.aterm) list * (Absyn.aterm * Absyn.aterm) list


(** generate a new name for a bound variable.
    checks for clashes with 
      - constants in LF sig
      - logic variables in query
**)
let bvar_namegen_count = ref 0
 (* free vars must start with uppercase letter in twelf,
    so we actually don't need to check against the fvars *)
let bvar_namegen_base = ref "x"
let generate_name metadata fvars =
  let isName name = Metadata.isName metadata name
    (*if (Metadata.isName metadata name)
    then true
    else Option.isSome (Table.find (Symbol.symbol name) fvars)*)
  in
  let rec generate_aux () =
    let name = bvar_namegen_count := (!bvar_namegen_count) + 1;
               (!bvar_namegen_base) ^ (string_of_int (!bvar_namegen_count)) in
    if isName name
    then generate_aux ()
    else name
  in
  generate_aux ()
let reset_namegen_count () = bvar_namegen_count := 0


let invert (Lfsig.Signature(_,types)) metadata fvars (subst, disprs) =
  (* collect all the object constants into a table for easy lookup
     ****Move this to somewhere else so it doesn't have to be done
         every time we invert; the signature does not change so can 
         be done once.**** *)
  let constants =
    let addObjects table objlist =
      List.fold_left (fun t o -> Symboltable.insert t (Symb.symbol (Lfabsyn.get_obj_name (!o))) (!o)) table objlist
    in
    Symboltable.fold types (fun symb (Lfabsyn.TypeFam(_,_,_,_,_,objsref,_)) t -> addObjects t (!objsref)) Symboltable.empty
  in
  (* apply_subst : Lfabsyn.typ -> (Lfabsyn.id * Lfabsyn.term) list -> Lfabsyn.typ
     Apply the given substitution to the LF type. *)
  let rec apply_subst lftype substitution =
    let rec reduce head args =
      match head, args with
          Lfabsyn.IdTerm(id), _ ->
            Lfabsyn.AppTerm(id, args)
        | Lfabsyn.AppTerm(id, args'), _ ->
            Lfabsyn.AppTerm(id, List.append args' args)
        | Lfabsyn.AbsTerm(id, typ, tm), (a :: args') ->
            let tm' = sub_tm id a tm in
            reduce tm' args'
    and sub_tm id tm t =
      match t with
          Lfabsyn.AbsTerm(n,ty,_) 
            when (Lfabsyn.get_id_name id) = (Lfabsyn.get_id_name n) ->
            let newname = generate_name metadata fvars in
            let Lfabsyn.AbsTerm(n',ty',body') = 
              sub_tm id (Lfabsyn.IdTerm(Lfabsyn.Var(newname,ty))) t 
            in
            Lfabsyn.AbsTerm(n', subst id tm ty', sub_tm id tm body')
        | Lfabsyn.AbsTerm(n,ty,body) ->
            let ty' = subst id tm ty in
            let body' = sub_tm id tm body in
            Lfabsyn.AbsTerm(n,ty',body')
        | Lfabsyn.AppTerm(head,args) ->
            let args' = List.map (sub_tm id tm) args in
            if (Lfabsyn.get_id_name id) = (Lfabsyn.get_id_name head)
            then
              (match tm with
                   Lfabsyn.IdTerm(ident) ->
                     Lfabsyn.AppTerm(ident,args')
                 | Lfabsyn.AppTerm(ident,newargs) ->
                     Lfabsyn.AppTerm(ident, List.append newargs args')
                 | Lfabsyn.AbsTerm(_,_,_) when (List.length args) = 0 ->
                     tm
                 | Lfabsyn.AbsTerm(_,_,_) ->
                     Errormsg.warning Errormsg.none 
                                    ("Warning: apply_subst: creating beta-redex when substituting "^
                                     (Lfabsyn.string_of_term tm)^ " for "^(Lfabsyn.string_of_id id)^
                                     " in " ^ (Lfabsyn.string_of_term t));
                     reduce tm args )
            else
              Lfabsyn.AppTerm(head, args')
        | Lfabsyn.IdTerm(n) ->
            if (Lfabsyn.get_id_name id) = (Lfabsyn.get_id_name n)
            then tm
            else t
    (* this is very inefficient (walks through the type for each piece of the substitution)
       but is correct and will work. Should be fixed later though. *)
    and subst id tm ty =
      match ty with
          Lfabsyn.PiType(n,t,_) 
            when (Lfabsyn.get_id_name id) = (Lfabsyn.get_id_name n) -> 
            ty
        | Lfabsyn.PiType(n,t,b) ->
            Lfabsyn.PiType(n, subst id tm t, subst id tm b)
        | Lfabsyn.ImpType(l,r) ->
            Lfabsyn.ImpType(subst id tm l, subst id tm r)
        | Lfabsyn.AppType(h,args) ->
            let args' = List.map (sub_tm id tm) args in
            Lfabsyn.AppType(h,args')
        | Lfabsyn.IdType(_) -> ty
    in
    match substitution with
        ((id,tm) :: subs) ->
          let lftype' = subst id tm lftype in
          apply_subst lftype' subs
      | [] -> lftype
  in
  (* get_type : Lfabsyn.typ Table.SybolTable.t -> (string * Lfabsyn.typ) list -> Absyn.aterm -> Lfabsyn.typ option
     determine the LF type for an LP term. Assumes all logic variables have known types. *)
  let get_type fvars bvars lpterm =
    let rec getty lpterm =
      match lpterm with
          Absyn.ConstantTerm(c,_,_) ->
            (match (Metadata.getLF metadata (Absyn.getConstantSymbol c)) with
                 Some(symb) ->
                   (match (Symboltable.lookup constants symb) with
                        Some(c') -> Some(Lfabsyn.get_obj_typ c')
                      | None ->
                          Errormsg.error Errormsg.none ("No entry found in LF signature for constant "^(Symb.printName symb));
                          None)
               | None ->
                   Errormsg.error Errormsg.none ("No mapping found for LP constant "^(Absyn.getConstantName c));
                   None)
          (* I believe bdindex starts at 1,
             but undexing into bvar list starts at 0. *)
        | Absyn.BoundVarTerm(Absyn.DBIndex(i),_) ->
            Some(snd (List.nth bvars (i-1)))
        | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tysymb),_) ->
            (match (Table.find (Absyn.getTypeSymbolSymbol tysymb) fvars) with
                 Some(ty) -> Some(ty)
               | None ->
                   Errormsg.error Errormsg.none ("No type found for logic variable "^(Absyn.getTypeSymbolName tysymb));
                   None)
        | Absyn.ApplicationTerm(abstm,_) ->
            (** To apply the type for the head to the arguments we would actually need to
                translate the args. *)
            None
        | Absyn.AbstractionTerm(_,_) -> (* We are unable to determine a type for an abstraction *)
	    None
    in
    getty lpterm
  in
  (** invert_term : Lfabsyn.typ Table.SymbolTable.t -> (string * Lfabsyn.typ) list -> (Lfabsyn.typ * (Lfabsyn.id * Lfabsyn.term) list) -> Absyn.aterm -> (Lfabsyn.term option * Lfabsyn.typ Table.SymbolTable.t)
      Invert a single term, return None if there is an error *)
  let rec invert_term fvars bvars (lftype, subst) lpterm =
    match lpterm with
        Absyn.ConstantTerm(c,_,p) ->
          (match (Metadata.getLF metadata (Absyn.getConstantSymbol c)) with
               Some(symb) ->
                 (match (Symboltable.lookup constants symb) with
                      Some(c') -> (Some(Lfabsyn.IdTerm(Lfabsyn.Const(Lfabsyn.get_obj_name c'))), fvars)
                    | None ->
                        Errormsg.error Errormsg.none ("No entry found in LF signature for constant "^(Symb.printName symb));
                        (None, fvars))
             | None ->
                 Errormsg.error Errormsg.none ("No mapping found for LP constant "^(Absyn.getConstantName c));
                 (None, fvars))
        (* I believe bdindex starts at 1,
             but undexing into bvar list starts at 0. *)
      | Absyn.BoundVarTerm(Absyn.DBIndex(i),p) ->
          let (name, ty) = List.nth bvars (i-1) in
          (Some(Lfabsyn.IdTerm(Lfabsyn.Var(name, ty))), fvars)
      | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tysymb), p) ->
          let (ty, fvars'') =
            (match (Table.find (Absyn.getTypeSymbolSymbol tysymb) fvars) with
                 Some(ty) ->
                   (ty, fvars)
               | None -> (* if this is a new variable, add to free var types *)
                   let ty = apply_subst lftype subst in
                   let fvars' = Table.add (Absyn.getTypeSymbolSymbol tysymb) ty fvars in
                   (ty, fvars'))
          in
          (Some(Lfabsyn.IdTerm(Lfabsyn.LogicVar(Absyn.string_of_term lpterm, ty))), fvars'')
      | Absyn.ApplicationTerm(abstm,p) ->
          let (head, args) = Absyn.getTermApplicationHeadAndArguments lpterm in
          if (Absyn.isTermFreeVariable head && 
              Option.isNone(Table.find (Absyn.getTypeSymbolSymbol (Absyn.getTermFreeVariableTypeSymbol head)) fvars))
          then
            (** new logic var, need to construct type and add to fvars *)
	    let target_ty = apply_subst lftype subst in
	    (** new logic vars appear as patterns so args are all bound vars, and so types can be found by get_type *)
	    let arg_tys = List.map (fun x -> Option.get (get_type fvars bvars x)) args in
            (** again, because new logic vars appear as patterns no need to worry about changes to fvars while inverting args *)
	    let args' = List.map2 (fun x y -> Option.get (fst (invert_term fvars bvars (x,[]) y))) arg_tys args in
	    (** b/c all args are bound variables we know they are of the correct form when inverted *)
            let h_ty = 
              List.fold_left (fun body (Lfabsyn.IdTerm((Lfabsyn.Var(_,ty) as id))) -> 
                                    Lfabsyn.PiType(id,ty,body)) 
                             target_ty 
                             (List.rev args') in
            let fvars' = Table.add (Absyn.getTypeSymbolSymbol (Absyn.getTermFreeVariableTypeSymbol head)) h_ty fvars in
	    let (h', fvars'') = invert_term fvars' bvars (h_ty, []) head in
            if Option.isNone h'
            then
              (None, fvars'')
            else
              (match Option.get h' with
                   Lfabsyn.IdTerm(id) ->
   	             (Some(Lfabsyn.AppTerm(id, args')), fvars'')
                 | Lfabsyn.AppTerm(id,newargs) ->
                     (Some(Lfabsyn.AppTerm(id, List.append newargs args')), fvars'')
                 | Lfabsyn.AbsTerm(_,_,_) ->
                     Errormsg.error Errormsg.none
                                    ("Error: invert_term: beta-redexes found in term when translating "^
                                     (Absyn.string_of_term lpterm)^" .");
                     (None, fvars''))
          else (*not a new logic var*)
            let h_ty = get_type fvars bvars head in
            if Option.isNone h_ty
            then 
              (Errormsg.error Errormsg.none 
                              ("Error: invert_term: Could not determine a type for application term head: "^
                               (Absyn.string_of_term head));
              (None, fvars))
            else
	      let (head', fvars') = invert_term fvars bvars (Option.get h_ty, []) head in
	      let rec trans_args bty subst fvars args =
	        let trans_arg bty subst fvars arg =
                  (match bty with
		       Lfabsyn.PiType(id,ty,body) ->
  		         let (a',fvars') = invert_term fvars bvars (ty, subst) arg in
		         (body, (id, Option.get a') :: subst, fvars', a')
                     | Lfabsyn.ImpType(l,r) ->
                         let (a', fvars') = invert_term fvars bvars (l, subst) arg in
                         (r, subst, fvars', a')
		     | _ -> 
                       Errormsg.error Errormsg.none ("Error: invert_term: Type of head does not match number of arguments.");
                       (bty, subst, fvars, None))
	        in
	        (match args with
		     (a :: args') ->
		       let (bty', subst', fvars', arg') = trans_arg bty subst fvars a in
	               let (rest, fvars'') = trans_args bty' subst' fvars' args' in
                       if Option.isSome arg'
                       then
		         ((Option.get arg') :: rest, fvars'')
                       else
                         (rest, fvars'')
	           | [] -> ([], fvars))
	      in
	      let (args', fvars'') = trans_args (Option.get h_ty) [] fvars' args in
              if Option.isNone head'
              then
                (None, fvars'')
              else
                (match Option.get head' with
                     Lfabsyn.IdTerm(id) ->
	               (Some(Lfabsyn.AppTerm(id, args')), fvars'')
                   | Lfabsyn.AppTerm(id,newargs) ->
                       (Some(Lfabsyn.AppTerm(id, List.append newargs args')), fvars'')
                   | Lfabsyn.AbsTerm(_,_,_) ->
                       Errormsg.error Errormsg.none
                                      ("Error: invert_term: beta-redexes found in term when translating "^
                                       (Absyn.string_of_term lpterm)^" .");
                       (None, fvars''))
      | Absyn.AbstractionTerm(Absyn.NestedAbstraction(_,body),p) ->
          (** generate name for bound var
              add to bvars
              recurse to body of abstraction *)
          let bvar_name = generate_name metadata fvars in
          (match apply_subst lftype subst with
               Lfabsyn.PiType(id, ty, tybody) ->
                 let bvars' = List.append [(bvar_name, ty)] bvars in
                 let bvar_term = Lfabsyn.IdTerm(Lfabsyn.Var(bvar_name, ty)) in
	         let (body', fvars') = invert_term fvars bvars' (tybody,[id, bvar_term]) body in
                 if (Option.isSome body')
                 then
  	           (Some(Lfabsyn.AbsTerm(Lfabsyn.Var(bvar_name, ty),ty,Option.get body')), fvars')
                 else
                   (*error, try to continue *)
                   (None, fvars')
             | Lfabsyn.ImpType(l, r) ->
                 let bvars' = List.append [(bvar_name, l)] bvars in
                 let bvar_term = Lfabsyn.IdTerm(Lfabsyn.Var(bvar_name, l)) in
                 let (body', fvars') = invert_term fvars bvars' (r, []) body in
                 if (Option.isSome body')
                 then
                   (Some(Lfabsyn.AbsTerm(Lfabsyn.Var(bvar_name, l),l,Option.get body')), fvars')
                 else
                   (*error, try to continue *)
                   (None, fvars')
             | _ ->
               Errormsg.error Errormsg.none ("Error: invert_term: Inverting abstraction with non-function type.");
               (None, fvars))
  in
  (** first work through substitution, then disagreement pairs.
      assumption: order in the substitution is in dependency order.
                  i.e. the type of the first var in the substitution
                       depends on no other vars, the second only the first,
                       and so on. The substitution for the proof term variable
                       should thus come last. *) 
  let rec trans_subst fvars subst=
    match subst with
        ((tysymb,tm) :: subst') ->
          let ty = 
            (match Table.find (Absyn.getTypeSymbolSymbol tysymb) fvars with
                 Some(t) -> t
               | None ->
                   (** this should really never happen*)
                   Errormsg.error Errormsg.none ("No entry in free vars found for "^(Absyn.getTypeSymbolName tysymb));
                   (** try to continue by getting a type *)
                   Option.get (get_type fvars [] tm))
          in
          let (lftm, fvars') = invert_term fvars [] (ty, []) tm in
          if Option.isSome lftm 
          then
            let id = Lfabsyn.LogicVar(Absyn.getTypeSymbolName tysymb, ty) in
            (* this piece of the substitution may need to be applied to the types for other variables.
               note that we know the find will succeed because we are using table.fold over the free vars. *)
            let fvars'' = Table.fold (fun symb lftyp fvs -> Table.add symb 
                                                                      (apply_subst (Option.get (Table.find symb fvs)) 
                                                                                   [id, Option.get lftm])
                                                                      fvs) 
                                     fvars' 
                                     fvars' in
            let (lfsubst, fvars''') = trans_subst fvars'' subst' in
            ((id, Option.get lftm) :: lfsubst, fvars''')
          else
            (* there was an error in translating, but try to continue. *)
            trans_subst fvars' subst'
      | [] -> ([], fvars)
  in
  (** lp terms can have the following forms:
         1. c t1 t2 ... tn
         2. X t1 t2 ... tn
         3. x t1 t2 ... tn
         4. lam. t
      We are able to determine a type for cases 1 & 2, but 
      not 3 & 4. In a disagreement pair at least one of the 
      two terms *must* be of form 1 or form 2; otherwise 
      another unification step could have been performed. 

      When translating a disagreement pair we first find
      a term of form 1 or 2 and get the type for the head 
      of the term. We can then translate t1,...,tn using 
      this type. Then we can determine a type for the full
      appliction term and then can translate the other term
      of the pair. **)
  let rec trans_disprs fvars disprs =
    let rec trans_dispr fvars (t1,t2) =
      let ty = 
        match (get_type fvars [] t1) with
            Some(ty) -> Some(ty)
          | None -> (get_type fvars [] t2) in
      if Option.isSome ty
      then (* simple case where one term is just a constant or free var *)
        let thetype = Option.get ty in
        let (t1', fvars') = reset_namegen_count ();
                            invert_term fvars [] (thetype,[]) t1 in
        let (t2', fvars'') = reset_namegen_count ();
                             invert_term fvars' [] (thetype,[]) t2 in
        ((t1',t2'), fvars'')
      else
        (* hasConstOrFreeVarHead : Absyn.aterm -> bool 
           assumes the given term is an application term. *)
        let hasConstOrFreeVarHead t =
          let (head, args) = Absyn.getTermApplicationHeadAndArguments t in
          (Absyn.isTermConstant head) or (Absyn.isTermFreeVariable head)
        in
        match (t1,t2) with
            (Absyn.ApplicationTerm(_,_),_) when hasConstOrFreeVarHead t1 ->
              let (head, args) = Absyn.getTermApplicationHeadAndArguments t1 in
              (* this must work now because know the head is a constant or free var *)
              let h_ty = Option.get (get_type fvars [] head) in 
              let (head', fvars') = reset_namegen_count ();
                                    invert_term fvars [] (h_ty, []) head in
              let trans_arg (bty, sub) fvars arg =
                (match bty with
                     Lfabsyn.PiType(id,ty,body) ->
                       let (arg', fvars') = reset_namegen_count ();
                                            invert_term fvars [] (ty, sub) arg in
                       if Option.isSome arg'
                       then
                         (arg', fvars', (body, ((id, Option.get arg')::sub)))
                       else
                         (* something went wrong, but try to continue *)
                         (None, fvars', (body, sub))
                   | Lfabsyn.ImpType(l,r) ->
                       let (arg', fvars') = reset_namegen_count ();
                                            invert_term fvars [] (l, sub) arg in
                         (arg', fvars', (r, sub))
                   | _ ->
                     Errormsg.error Errormsg.none 
                                    ("Error: trans_disprs: Type of head does not match number of arguments in term "^
                                     (Absyn.string_of_term t1));
                     (None, fvars, (bty, sub)))
              in
              let rec trans_args (ty, subst) fvars args =
                (match args with
                     (a :: args') ->
                       let (a', fvars', (ty', subst')) = trans_arg (ty, subst) fvars a in
                       let (transargs, subst'', fvars'') = trans_args (ty',subst') fvars' args' in
                       if Option.isSome a'
                       then
                         (List.append [Option.get a'] transargs, subst'', fvars'')
                       else
                         (transargs, subst'', fvars'')
                   | [] -> ([], subst, fvars))
              in
              let (args', subst, fvars'') = trans_args (h_ty, []) fvars' args in
              let term_ty = apply_subst h_ty subst in
              let t1' = 
                if Option.isNone head' 
                then
                  None
                else
                  (match Option.get head' with
                       Lfabsyn.IdTerm(ident) ->
                         Some(Lfabsyn.AppTerm(ident, args'))
                     (* these two cases shouldn't happen, so maybe change to just an error? *)
                     | Lfabsyn.AppTerm(ident,newargs) ->
                         Some(Lfabsyn.AppTerm(ident, List.append newargs args'))
                     | Lfabsyn.AbsTerm(_,_,_) ->
                         Errormsg.error Errormsg.none 
                                        ("Error: trans_disprs: This should not happen."^
                                         " Translation of constant or free var cannot be an abstraction.");
                         None)
              in
              let (t2', fvars''') = reset_namegen_count ();
                                    invert_term fvars'' [] (term_ty, []) t2 in
              ((t1',t2'),fvars''')
          | (_,Absyn.ApplicationTerm(_,_)) when hasConstOrFreeVarHead t2 ->
              let (head, args) = Absyn.getTermApplicationHeadAndArguments t2 in
              (* this must work now because know the head is a constant or free var *)
              let h_ty = Option.get (get_type fvars [] head) in 
              let (head', fvars') = reset_namegen_count ();
                                    invert_term fvars [] (h_ty, []) head in
              let trans_arg (bty, sub) fvars arg =
                (match bty with
                     Lfabsyn.PiType(id,ty,body) ->
                       let (arg', fvars') = reset_namegen_count ();
                                            invert_term fvars [] (ty, sub) arg in
                       if Option.isSome arg'
                       then
                         (arg', fvars', (body, ((id, Option.get arg')::sub)))
                       else
                         (* something went wrong, but try to continue *)
                         (None, fvars', (body, sub))
                   | Lfabsyn.ImpType(l,r) ->
                       let (arg', fvars') = reset_namegen_count ();
                                            invert_term fvars [] (l, sub) arg in
                       (arg', fvars', (r, sub))
                   | _ ->
                     Errormsg.error Errormsg.none 
                                    ("Error: trans_disprs: Type of head does not match number of arguments in term "^
                                     (Absyn.string_of_term t2));
                     (None, fvars, (bty, sub)))
              in
              let rec trans_args (ty, subst) fvars args =
                (match args with
                     (a :: args') ->
                       let (a', fvars', (ty', subst')) = trans_arg (ty, subst) fvars a in
                       let (transargs, subst'', fvars'') = trans_args (ty',subst') fvars' args' in
                       if Option.isSome a'
                       then
                         (List.append [Option.get a'] transargs, subst'', fvars'')
                       else
                         (transargs, subst'', fvars'')
                   | [] -> ([], subst, fvars))
              in
              let (args', subst, fvars'') = trans_args (h_ty,[]) fvars' args in
              let term_ty = apply_subst h_ty subst in
              let t2' = 
                if Option.isNone head' 
                then
                  None
                else
                  (match Option.get head' with
                       Lfabsyn.IdTerm(ident) ->
                         Some(Lfabsyn.AppTerm(ident, args'))
                     (* these two cases shouldn't happen, so maybe change to just an error? *)
                     | Lfabsyn.AppTerm(ident,newargs) ->
                         Some(Lfabsyn.AppTerm(ident, List.append newargs args'))
                     | Lfabsyn.AbsTerm(_,_,_) ->
                         Errormsg.error Errormsg.none 
                                        ("Error: trans_disprs: This should not happen."^
                                         " Translation of constant or free var cannot be an abstraction.");
                         None)
              in
              let (t1', fvars''') = reset_namegen_count ();
                                    invert_term fvars'' [] (term_ty, []) t1 in
              ((t1',t2'),fvars''')
          | _ ->
            Errormsg.error Errormsg.none 
                           ("Error: trans_disprs: Neither disagreement pair term is application with constant or free var head.\n"^
                            "term 1: "^(Absyn.string_of_term t1)^"\nterm 2: "^(Absyn.string_of_term t2));
            ((None, None),fvars)
            
    in
    match disprs with
        ((t1,t2) :: disprs') ->
          let ((t1',t2'), fvars') = trans_dispr fvars (t1,t2) in
          if (Option.isSome t1') && (Option.isSome t2')
          then
            let (rest, fvars'') = trans_disprs fvars' disprs' in
            ((Option.get t1', Option.get t2') :: rest, fvars'')
          else
            (* there was an error in translationg, but try to continue *)
            trans_disprs fvars' disprs'
      | [] -> ([], fvars)
  in
  let (lfsubst, fvars') = trans_subst fvars subst in
  let (lfdisprs, fvars'') = trans_disprs fvars' disprs in
  (lfsubst, lfdisprs)
