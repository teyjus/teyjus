(** Optimizations available for use by the translation. *)

module type Optimization =
sig
  val get : unit -> bool
  val set : bool -> unit
  val run_optimization : Lpsig.signature -> Lpsig.signature
end

module Specialize : Optimization =
struct
  let run = ref false

  let get () = !run

  let set v = run := v
 
  let run_optimization (Lpsig.Signature(name, kinds, types)) = 
    (* for each constant that is an lf-type, modify the type.
       ( A -> lftype  ===> lf-obj -> A -> o )
       then go through each clause with 'hastype', modify it, and 
       move it to the correct predicate *)

    (* Modify the types *)
    let alterTypes symb (Lpabsyn.TypeDec(id,ty,clauses)) typedecls =
      let rec is_type ty =
        match ty with
            Lpabsyn.ImpType(t1,t2) -> is_type t2
          | Lpabsyn.ConstType(Lpabsyn.Const("lf_type"), []) -> true
          | _ -> false
      in
      if (is_type ty)
        then
          let rec change ty =
            match ty with
                Lpabsyn.ImpType(t1, t2) -> Lpabsyn.ImpType(t1, change t2)
              | Lpabsyn.ConstType(Lpabsyn.Const("lf_type"), []) -> 
                  Lpabsyn.ConstType(Lpabsyn.Const("o"), [])
              | _ -> 
                  (Errormsg.error Errormsg.none
                                  ("This should not have happened. The type " ^
                                     (Lpabsyn.print_type ty) ^
                                     "should have target type lf_type");
                    ty)
          in
          let newty = Lpabsyn.ImpType(Lpabsyn.ConstType(Lpabsyn.Const("lf_obj"),[]),(change ty)) in
          Symboltable.insert (Symboltable.remove typedecls symb) 
                             symb
                             (Lpabsyn.TypeDec(id,newty,clauses))
        else typedecls
    in
    (* modify the clauses *)
    let perClause typedecls ((Lpabsyn.Clause(head, goals)) as c) =
      let targettype = 
        match head with
            Lpabsyn.Head(Lpabsyn.Const("hastype"), [_;Lpabsyn.IdTerm(Lpabsyn.Const(name))])
          | Lpabsyn.Head(Lpabsyn.Const("hastype"), [_;Lpabsyn.AppTerm(Lpabsyn.Const(name),_)]) ->
              name
          | _ -> 
              (Errormsg.error Errormsg.none
                              ("Found non-hastype clause: " ^ (Lpabsyn.print_clause c));
              "hastype")
      in
      let rec walk_clause (Lpabsyn.Clause(head, goals)) =
        let head' = walk_head head in
        let goals' = List.map walk_goal goals in
        Lpabsyn.Clause(head', goals')
      and walk_head head =
        match head with
            Lpabsyn.Head(Lpabsyn.Const("hastype"), [trm; Lpabsyn.AppTerm(tpconst, args)]) ->
              Lpabsyn.Head(tpconst, (trm :: args))
          | Lpabsyn.Head(Lpabsyn.Const("hastype"), [trm; Lpabsyn.IdTerm(tpconst)]) ->
              Lpabsyn.Head(tpconst, [trm])
          | _ -> head
      and walk_goal goal =
        match goal with
            Lpabsyn.Atom(tm) -> Lpabsyn.Atom(walk_term tm)
          | Lpabsyn.ImpGoal(c,g) ->
              Lpabsyn.ImpGoal(walk_clause c, walk_goal g)
          | Lpabsyn.Conjunction(g1,g2) -> 
              Lpabsyn.Conjunction(walk_goal g1, walk_goal g2)
          | Lpabsyn.Disjunction(g1,g2) ->
              Lpabsyn.Disjunction(walk_goal g1, walk_goal g2)
          | Lpabsyn.Universal(id,ty,body) ->
              Lpabsyn.Universal(id,ty, walk_goal body)
          | Lpabsyn.Existential(id,ty,body) ->
              Lpabsyn.Existential(id,ty, walk_goal body)
      and walk_term tm =
        match tm with
            Lpabsyn.AppTerm(Lpabsyn.Const("hastype"),[trm; Lpabsyn.AppTerm(tpconst, args)]) ->
              Lpabsyn.AppTerm(tpconst, (trm :: args))
          | _ -> tm
      in
      let newclause = walk_clause c in
      match (Symboltable.lookup typedecls (Symb.symbol targettype)) with
          Some(Lpabsyn.TypeDec(_,_,clauses)) -> clauses := List.append (!clauses) [ref newclause]
        | None -> ()
    in
    (* Alter the type of each constant corresponding to an LF type so
       that it is a predicate. *) 
    let typedecls' = Symboltable.fold types alterTypes types in
    (* For each clause we must modify to use the specialized predicate 
       based on the LF type. *)
    let _ = 
      List.iter (fun c -> perClause typedecls' !c) 
                (match (Symboltable.lookup typedecls' (Symb.symbol "hastype")) with
                     None -> 
                       ((Errormsg.error Errormsg.none 
                                        "predicate 'hastype' was not found in symboltable.");
                        [])
                  | Some(Lpabsyn.TypeDec(_,_,clauses)) ->
                      !clauses)
     in
     let typedecls'' = Symboltable.remove typedecls' (Symb.symbol "hastype") in
     Lpsig.Signature(name,kinds,typedecls'')
end

module Swap : Optimization =
struct
  let run = ref false

  let get () = !run

  let set v = run := v

  let run_optimization (Lpsig.Signature(name, kinds, types)) =
    (* for each entry in the type table (that is a predicate) 
       move the first argument to the last. Then for any use
       of the predicate move the first argument to the last. *)
    let preds = ref [] in
    let rec predicate ty =
      match ty with
          Lpabsyn.ImpType(t1,t2) -> predicate t2
        | Lpabsyn.ConstType(Lpabsyn.Const("o"), []) -> true
        | _ -> false
    in
    (* modify the types *)
    let alterTypes symb (Lpabsyn.TypeDec(id, ty, clauses)) typedecls =
      if (predicate ty)
        then
          match ty with
              Lpabsyn.ConstType(Lpabsyn.Const("o"), []) -> typedecls
            | Lpabsyn.ImpType(_, Lpabsyn.ConstType(Lpabsyn.Const("o"), [])) -> typedecls
            | Lpabsyn.ImpType(firstarg, (Lpabsyn.ImpType(nextarg, rest) as body)) ->
                let rec move firstarg t =
                  match t with
                      Lpabsyn.ImpType(finalarg, Lpabsyn.ConstType(Lpabsyn.Const("o"), [])) ->
                        Lpabsyn.ImpType(finalarg, 
                                        Lpabsyn.ImpType(firstarg, 
                                                        Lpabsyn.ConstType(Lpabsyn.Const("o"), [])))
                    | Lpabsyn.ImpType(l, r) ->
                        Lpabsyn.ImpType(l, move firstarg r)
                    | _ -> (Errormsg.error Errormsg.none
                                  ("This shouldn't happen. Already checked if " ^ 
                                   (Lpabsyn.print_type ty) ^ 
                                   " was a prediate type but it does not have the correct form.");
                           t)
                in 
                let _ = preds := (id :: (!preds)) in
                Symboltable.insert (Symboltable.remove types symb)
                                     symb 
                                     (Lpabsyn.TypeDec(id, (move firstarg body), clauses))
                                   
            | _ -> (Errormsg.error Errormsg.none
                                   ("This shouldn't happen. Already checked if " ^ 
                                    (Lpabsyn.print_type ty) ^ 
                                    " was a prediate type but it does not have the correct form.");
                   typedecls)
        else types
    in
    (* modify the terms *)
    let alterTerms symb (Lpabsyn.TypeDec(id, ty, clauses)) =
      let rec walk_term tm =
        match tm with
            Lpabsyn.AppTerm(id, tms) ->
              let tms' = List.map walk_term tms in 
              if (List.mem id !preds)
                then
                  Lpabsyn.AppTerm(id, List.append (List.tl tms') [List.hd tms'])
                else
                  Lpabsyn.AppTerm(id, tms')
          | Lpabsyn.AbsTerm(id, ty, tm) ->
              Lpabsyn.AbsTerm(id, ty, walk_term tm)
          | Lpabsyn.IdTerm(id) -> tm
      and walk_type ty =
        match ty with
            Lpabsyn.ImpType(t1,t2) -> 
              Lpabsyn.ImpType(walk_type t1, walk_type t2)
          | Lpabsyn.ConstType(id,tms) ->
              Lpabsyn.ConstType(id, List.map walk_term tms)
          | Lpabsyn.VarType(_) -> ty
      in
      let rec walk_clause (Lpabsyn.Clause(Lpabsyn.Head(id, tms), goals)) =
        let tms' = List.map walk_term tms in
        let head' = if (List.mem id !preds)
                     then Lpabsyn.Head(id, List.append (List.tl tms') [List.hd tms'])
                     else Lpabsyn.Head(id, tms') in
        let goals' = List.map walk_goal goals in
        Lpabsyn.Clause(head', goals')
      and walk_goal g =
        match g with
            Lpabsyn.Atom(tm) -> Lpabsyn.Atom(walk_term tm)
          | Lpabsyn.ImpGoal(c, g) -> 
              Lpabsyn.ImpGoal(walk_clause c, walk_goal g)
          | Lpabsyn.Conjunction(g1,g2) -> 
              Lpabsyn.Conjunction(walk_goal g1, walk_goal g2)
          | Lpabsyn.Disjunction(g1,g2) -> 
              Lpabsyn.Disjunction(walk_goal g1, walk_goal g2)
          | Lpabsyn.Universal(id, ty, g) -> 
              Lpabsyn.Universal(id, walk_type ty, walk_goal g)
          | Lpabsyn.Existential(id, ty, g) -> 
              Lpabsyn.Existential(id, walk_type ty, walk_goal g)
      
      in
      clauses := List.map (fun c -> ref (walk_clause !c)) !clauses
    in
    (* First we change the type of each predicate. *)
    let alteredtypes = Symboltable.fold types alterTypes types in
    (* Next we walk through to swap the location of terms which are
      arguments to the modified predicates. *)
    let _ = Symboltable.iter alteredtypes alterTerms in
    Lpsig.Signature(name, kinds, alteredtypes)
end
