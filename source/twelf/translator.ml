(** Translators for translating LF specifications into LP programs. *)

module type Translator =
sig
  val translate : Lfsig.signature -> Lpsig.signature
end

let getLP id = 
  match id with
      Lfabsyn.Const(s,_) -> Lpabsyn.Const("lf_" ^ s)
    | Lfabsyn.Var(s,_) -> Lpabsyn.Var(s)
let getLF id = 
  match id with
      Lpabsyn.Const(s) -> 
        Lfabsyn.Const(String.sub s 3 ((String.length s) -3), Errormsg.none)
    | Lpabsyn.Var(s) -> Lfabsyn.Var(s, Errormsg.none)


(* useful values to reference during translation. *)
let lftypeStr = "lf_type"
let lfobjStr = "lf_obj"
let lftype = Lpabsyn.Const(lftypeStr)
let lfobj = Lpabsyn.Const(lfobjStr)
let oType = Lpabsyn.ConstType(Lpabsyn.Const("o"), [])
let lftypeType = Lpabsyn.ConstType(lftype, [])
let lfobjType = Lpabsyn.ConstType(lfobj, [])
let initkinddecls = 
      Symboltable.insert (Symboltable.insert (Symboltable.empty)
                                             (Symbol.symbol lfobjStr)
                                             (Lpabsyn.Kind(lfobj, 0)))
                         (Symbol.symbol lftypeStr)
                         (Lpabsyn.Kind(lftype, 0))

let istypeStr = "istype"
let hastypeStr = "hastype"
let istype = Lpabsyn.Const(istypeStr)
let hastype = Lpabsyn.Const(hastypeStr)
let istypeClauses = ref []
let hastypeClauses = ref []
let inittypedecls =
      Symboltable.insert (Symboltable.insert (Symboltable.empty)
                                             (Symbol.symbol istypeStr)
                                             (Lpabsyn.TypeDec(istype, Lpabsyn.ImpType(lftypeType, oType), istypeClauses)))
                         (Symbol.symbol hastypeStr)
                         (Lpabsyn.TypeDec(hastype, Lpabsyn.ImpType(lfobjType, Lpabsyn.ImpType(lftypeType,oType)), hastypeClauses))

(* Generate unique names for variables generated during 
   translation. *)
let newVarCount = ref 0
let newVar () = 
  let vname = "X_" ^ (string_of_int !newVarCount) in
  let _ = newVarCount := !newVarCount + 1 in
  vname

(** Flatten an LF kind into a simple type. *)
let rec flatten_kind k =
  match k with
      Lfabsyn.PiKind(_, ty, body, _) -> 
        Lpabsyn.ImpType((flatten_type ty), (flatten_kind body))
    | Lfabsyn.ImpKind(l, r, _) -> 
        Lpabsyn.ImpType((flatten_type l), (flatten_kind r))
    | Lfabsyn.Type(_) -> lftypeType      
(** Flatten an LF type into a simple type. *)                                       
and flatten_type t =
  match t with
      Lfabsyn.PiType(id, ty, body, _) ->
        Lpabsyn.ImpType((flatten_type ty), (flatten_type body))
    | Lfabsyn.ImpType(l,r,_) -> 
        Lpabsyn.ImpType((flatten_type l), (flatten_type r))
    | Lfabsyn.AppType(_,_,_)
    | Lfabsyn.IdType(_,_) -> lfobjType
(** Encode an LF term into a simply typed term. *)
let rec encode_term t =
    match t with
        Lfabsyn.AbsTerm(id,ty,tm,_) ->
          Lpabsyn.AbsTerm(getLP id, flatten_type ty, encode_term tm)
      | Lfabsyn.AppTerm(head,tms,_) -> 
          let transtms = List.map encode_term tms in
          Lpabsyn.AppTerm(getLP head, transtms)
      | Lfabsyn.IdTerm(id,_) -> 
          Lpabsyn.IdTerm(getLP id)

(** The basic, naive translation. *)
module NaiveTranslation : Translator =
struct
  (** Encode an LF kind as a clause.
      @returns a function that when applied to the encoding of an LF 
               constant `a' produces a clause encoding the judgement
               `a : k'. *)
  let rec encode_kind k =
    match k with
        Lfabsyn.PiKind(id, ty, body, _) ->
          fun s ->
            let lpvar = Lpabsyn.IdTerm(getLP id) in
            let goal = (encode_type_goal ty) lpvar in
            let Lpabsyn.Clause(head, goals) =
              (encode_kind body) (match s with
                                      Lpabsyn.IdTerm(n) -> 
                                        Lpabsyn.AppTerm(n, [lpvar])
                                    | Lpabsyn.AppTerm(h,tms) -> 
                                        Lpabsyn.AppTerm(h, List.append tms [lpvar])
                                    | _ -> Errormsg.error Errormsg.none 
                                                          ("Error applying encoded kind " ^ 
                                                            (Lfabsyn.print_kind k) ^ 
                                                            " to term " ^ 
                                                            (Lpabsyn.print_term s)); s)
            in
            Lpabsyn.Clause(head, List.append goals [goal])
      | Lfabsyn.ImpKind(l, r, _) ->
          fun s ->
            let name = newVar () in
            let lpvar = Lpabsyn.IdTerm(Lpabsyn.Var(name)) in
            let goal = (encode_type_goal l) lpvar in
            let Lpabsyn.Clause(head, goals) =
              (encode_kind r) (match s with
                                   Lpabsyn.IdTerm(n) -> 
                                     Lpabsyn.AppTerm(n, [lpvar])
                                 | Lpabsyn.AppTerm(h,tms) -> 
                                     Lpabsyn.AppTerm(h, List.append tms [lpvar])
                                 | _ -> Errormsg.error Errormsg.none 
                                                       ("Error applying encoded kind " ^ 
                                                         (Lfabsyn.print_kind k) ^ 
                                                         " to term " ^ 
                                                         (Lpabsyn.print_term s)); s)
            in
            Lpabsyn.Clause(head, List.append goals [goal])
      | Lfabsyn.Type(_) -> 
          fun s -> 
            Lpabsyn.Clause(Lpabsyn.Head(istype, [s]),[])
  (** Encode an LF type as a clause.
      @returns a function that when applied to the encoding of an LF 
               constant `c' produces a clause encoding the judgement
               `c : t'. *)
  and encode_type t =
    match t with
        Lfabsyn.PiType(id,ty,body,_) ->
          fun s ->
            let lpvar = Lpabsyn.IdTerm(getLP id) in
            let goal = (encode_type_goal ty) lpvar in
            let Lpabsyn.Clause(head, goals) = 
              (encode_type body) (match s with
                                       Lpabsyn.IdTerm(n) -> 
                                         Lpabsyn.AppTerm(n, [lpvar])
                                     | Lpabsyn.AppTerm(h,tms) -> 
                                         Lpabsyn.AppTerm(h, List.append tms [lpvar])
                                     | _ -> Errormsg.error Errormsg.none 
                                                           ("Error applying encoded type " ^ 
                                                             (Lfabsyn.print_typ t) ^ 
                                                             " to term " ^ 
                                                             (Lpabsyn.print_term s)); s) 
            in
            Lpabsyn.Clause(head, List.append goals [goal])
      | Lfabsyn.AppType(id,tms,_) ->
          fun s ->
            let typtm = Lpabsyn.AppTerm(getLP id, List.map encode_term tms) in
            let head = Lpabsyn.Head(hastype, [s; typtm]) in
            Lpabsyn.Clause(head,[])
      | Lfabsyn.ImpType(t1,t2,_) ->
          fun s ->
            let lpvar = Lpabsyn.IdTerm(Lpabsyn.Var(newVar ())) in
            let goal = (encode_type_goal t1) lpvar in
            let Lpabsyn.Clause(head, goals) = 
              (encode_type t2) (match s with
                                    Lpabsyn.IdTerm(n) -> 
                                      Lpabsyn.AppTerm(n, [lpvar])
                                  | Lpabsyn.AppTerm(h,tms) -> 
                                      Lpabsyn.AppTerm(h, List.append tms [lpvar])
                                  | _ -> Errormsg.error Errormsg.none 
                                                        ("Error applying encoded type " ^ 
                                                          (Lfabsyn.print_typ t) ^ 
                                                          " to term " ^ 
                                                          (Lpabsyn.print_term s)); s) 
            in
            Lpabsyn.Clause(head, List.append goals [goal])
      | Lfabsyn.IdType(id,_) ->
          fun s ->
            Lpabsyn.Clause(Lpabsyn.Head(hastype, [s;
                                                  Lpabsyn.IdTerm(getLP
                                                                   id)]), [])
  (** Similar to {!encode_type} but generates a goal rather than a 
      clause. *) 
  and encode_type_goal t =
    match t with
        Lfabsyn.PiType(id,ty,body,_) ->
          fun s ->
            let lpvar = Lpabsyn.IdTerm(getLP id) in
            let clause = (encode_type ty) lpvar in
            let goal = 
              (encode_type_goal body) (match s with
                                           Lpabsyn.IdTerm(n) -> 
                                             Lpabsyn.AppTerm(n, [lpvar])
                                         | Lpabsyn.AppTerm(h,tms) -> 
                                             Lpabsyn.AppTerm(h, List.append tms [lpvar])
                                         | _ -> Errormsg.error Errormsg.none 
                                                               ("Error applying encoded type " ^ 
                                                                 (Lfabsyn.print_typ t) ^ 
                                                                 " to term " ^ 
                                                                 (Lpabsyn.print_term s)); s) 
            in
            Lpabsyn.Universal(getLP id, flatten_type ty, Lpabsyn.ImpGoal(clause, goal))
      | Lfabsyn.AppType(id,tms,_) ->
          fun s ->
            let typtm = Lpabsyn.AppTerm(getLP id, List.map encode_term tms) in
            Lpabsyn.Atom(Lpabsyn.AppTerm(hastype, [s; typtm]))
      | Lfabsyn.ImpType(t1,t2,_) ->
          fun s ->
            let name = newVar () in
            let lpvar = Lpabsyn.IdTerm(Lpabsyn.Var(name)) in
            let clause = (encode_type t1) lpvar in
            let goal = 
              (encode_type_goal t2) (match s with
                                         Lpabsyn.IdTerm(n) -> 
                                           Lpabsyn.AppTerm(n, [lpvar])
                                       | Lpabsyn.AppTerm(h,tms) -> 
                                           Lpabsyn.AppTerm(h, List.append tms [lpvar])
                                       | _ -> Errormsg.error Errormsg.none 
                                                             ("Error applying encoded type " ^ 
                                                               (Lfabsyn.print_typ t) ^ 
                                                               " to term " ^ 
                                                               (Lpabsyn.print_term s)); s) 
            in
            Lpabsyn.Universal(Lpabsyn.Var(name), flatten_type t1, Lpabsyn.ImpGoal(clause, goal))
      | Lfabsyn.IdType(id,_) ->
          fun s ->
          Lpabsyn.Atom(Lpabsyn.AppTerm(hastype, [s; Lpabsyn.IdTerm(getLP id)]))

  (* Process each type declaration of an LF signature. 
     Create a simply-typed constant corresponding to the current
     constant. Generate the istype clause for this type. For each
     object declaration corresponding to this type generate a
     corresponding simply-typed constant and generate the hastype
     clause for that constant. *)
  let process types =
    let perType symb (Lfabsyn.TypeFam(id,kind,_,_,_,objs,_)) typedecls =
      (* make type decl for this constant *)
      let lpid = getLP id in
      let lpname = 
         match lpid with
             Lpabsyn.Const(name) -> name
           | Lpabsyn.Var(name) -> 
             (Errormsg.error Errormsg.none 
                             ("error inserting into type symboltable: " ^ 
                               (Lpabsyn.print_id lpid) ^ 
                               " is not a constant.")); name in
      let typedecls' =
            Symboltable.insert typedecls 
                               (Symbol.symbol lpname)
                               (Lpabsyn.TypeDec(lpid, (flatten_kind kind), ref []))
      in
      (* make istype clause for type, put in istypeClauses *)
      let newClause = ref ((encode_kind kind) (Lpabsyn.IdTerm(lpid))) in
      let _ = istypeClauses := (List.append (!istypeClauses) [newClause]) in
        let perObj typedecls (Lfabsyn.Object(id, ty,_,_,_,_)) =
          (* for object in typedecl's list,
             make type decl for constant
             make hastype clause, put in hastypeClauses *)
          let objlpid = getLP id in
          let objlpname = 
             match objlpid with
                 Lpabsyn.Const(name) -> name
               | Lpabsyn.Var(name) -> 
                 (Errormsg.error Errormsg.none 
                                 ("error inserting into type symboltable: " ^ 
                                   (Lpabsyn.print_id objlpid) ^ 
                                   " is not a constant.")); name in
          let newClause = ref ((encode_type ty) (Lpabsyn.IdTerm(objlpid))) in
          let typedecls' =
            (Symboltable.insert typedecls
                               (Symbol.symbol objlpname)
                               (Lpabsyn.TypeDec(objlpid, (flatten_type ty), ref [])))
          in
          (hastypeClauses := List.append (!hastypeClauses) [newClause];
           typedecls')
        in
        List.fold_left (fun tps o -> perObj tps (!o)) typedecls' (!objs)
    in
    Symboltable.fold types
                     perType
                     inittypedecls

  let translate (Lfsig.Signature(_, types)) =
    let _ = hastypeClauses := [] ; istypeClauses := [] in
    let typedecls = process types
    in Lpsig.Signature("top", initkinddecls, typedecls)
    
end


(** Optimized translation using strictness to reduce size of clauses. *)
module OptimizedTranslation : Translator =
struct
  (** Similar to {!Translator.NaiveTranslation.encode_kind} but 
      includes strictness checks. *)
  let rec encode_kind k =
    match k with
        Lfabsyn.PiKind(id, ty, body, _) ->
          fun s ->
            let lpvar = Lpabsyn.IdTerm(getLP id) in
            let goal = if (Strictness.appears_strict id ty)
                         then []
                         else [(encode_type_goal ty) lpvar] in
            let Lpabsyn.Clause(head, goals) =
              (encode_kind body) (match s with
                                      Lpabsyn.IdTerm(n) -> 
                                        Lpabsyn.AppTerm(n, [lpvar])
                                    | Lpabsyn.AppTerm(h,tms) -> 
                                        Lpabsyn.AppTerm(h, List.append tms [lpvar])
                                    | _ -> Errormsg.error Errormsg.none 
                                                          ("Error applying encoded kind " ^ 
                                                            (Lfabsyn.print_kind k) ^ 
                                                            " to term " ^ 
                                                            (Lpabsyn.print_term s)); s)
            in
            Lpabsyn.Clause(head, List.append goals goal)
      | Lfabsyn.ImpKind(l, r, _) ->
          fun s ->
            let name = newVar () in
            let lpvar = Lpabsyn.IdTerm(Lpabsyn.Var(name)) in
            let goal = (encode_type_goal l) lpvar in
            let Lpabsyn.Clause(head, goals) =
              (encode_kind r) (match s with
                                   Lpabsyn.IdTerm(n) -> 
                                     Lpabsyn.AppTerm(n, [lpvar])
                                 | Lpabsyn.AppTerm(h,tms) -> 
                                     Lpabsyn.AppTerm(h, List.append tms [lpvar])
                                 | _ -> Errormsg.error Errormsg.none 
                                                       ("Error applying encoded kind " ^ 
                                                         (Lfabsyn.print_kind k) ^ 
                                                         " to term " ^ 
                                                         (Lpabsyn.print_term s)); s)
            in
            Lpabsyn.Clause(head, List.append goals [goal])
      | Lfabsyn.Type(_) -> 
          fun s -> 
            Lpabsyn.Clause(Lpabsyn.Head(istype, [s]),[])
  (** Similar to {!Translator.NaiveTranslation.encode_type} but 
      includes strictness checks. *)
  and encode_type t =
    match t with
        Lfabsyn.PiType(id,ty,body,_) ->
          fun s ->
            let lpvar = Lpabsyn.IdTerm(getLP id) in
            let goal = if (Strictness.appears_strict id ty)
                         then []
                         else [(encode_type_goal ty) lpvar] in
            let Lpabsyn.Clause(head, goals) = 
              (encode_type body) (match s with
                                       Lpabsyn.IdTerm(n) -> 
                                         Lpabsyn.AppTerm(n, [lpvar])
                                     | Lpabsyn.AppTerm(h,tms) -> 
                                         Lpabsyn.AppTerm(h, List.append tms [lpvar])
                                     | _ -> Errormsg.error Errormsg.none 
                                                           ("Error applying encoded type " ^ 
                                                             (Lfabsyn.print_typ t) ^ 
                                                             " to term " ^ 
                                                             (Lpabsyn.print_term s)); s) 
            in
            Lpabsyn.Clause(head, List.append goals goal)
      | Lfabsyn.AppType(id,tms,_) ->
          fun s ->
            let typtm = Lpabsyn.AppTerm(getLP id, List.map encode_term tms) in
            let head = Lpabsyn.Head(hastype, [s; typtm]) in
            Lpabsyn.Clause(head,[])
      | Lfabsyn.ImpType(t1,t2,_) ->
          fun s ->
            let lpvar = Lpabsyn.IdTerm(Lpabsyn.Var(newVar ())) in
            let goal = (encode_type_goal t1) lpvar in
            let Lpabsyn.Clause(head, goals) = 
              (encode_type t2) (match s with
                                    Lpabsyn.IdTerm(n) -> 
                                      Lpabsyn.AppTerm(n, [lpvar])
                                  | Lpabsyn.AppTerm(h,tms) -> 
                                      Lpabsyn.AppTerm(h, List.append tms [lpvar])
                                  | _ -> Errormsg.error Errormsg.none 
                                                        ("Error applying encoded type " ^ 
                                                          (Lfabsyn.print_typ t) ^ 
                                                          " to term " ^ 
                                                          (Lpabsyn.print_term s)); s) 
            in
            Lpabsyn.Clause(head, List.append goals [goal])
      | Lfabsyn.IdType(id,_) ->
          fun s ->
            Lpabsyn.Clause(Lpabsyn.Head(hastype, [s; Lpabsyn.IdTerm(getLP id)]), [])
  and encode_type_goal t =
    match t with
        Lfabsyn.PiType(id,ty,body,_) ->
          fun s ->
            let lpvar = Lpabsyn.IdTerm(getLP id) in
            let clause = (encode_type ty) lpvar in
            let goal = 
              (encode_type_goal body) (match s with
                                           Lpabsyn.IdTerm(n) -> 
                                             Lpabsyn.AppTerm(n, [lpvar])
                                         | Lpabsyn.AppTerm(h,tms) -> 
                                             Lpabsyn.AppTerm(h, List.append tms [lpvar])
                                         | _ -> Errormsg.error Errormsg.none 
                                                               ("Error applying encoded type " ^ 
                                                                 (Lfabsyn.print_typ t) ^ 
                                                                 " to term " ^ 
                                                                 (Lpabsyn.print_term s)); s) 
            in
            Lpabsyn.Universal(getLP id, flatten_type ty, Lpabsyn.ImpGoal(clause, goal))
      | Lfabsyn.AppType(id,tms,_) ->
          fun s ->
            let typtm = Lpabsyn.AppTerm(getLP id, List.map encode_term tms) in
            Lpabsyn.Atom(Lpabsyn.AppTerm(hastype, [s; typtm]))
      | Lfabsyn.ImpType(t1,t2,_) ->
          fun s ->
            let name = newVar () in
            let lpvar = Lpabsyn.IdTerm(Lpabsyn.Var(name)) in
            let clause = (encode_type t1) lpvar in
            let goal = 
              (encode_type_goal t2) (match s with
                                         Lpabsyn.IdTerm(n) -> 
                                           Lpabsyn.AppTerm(n, [lpvar])
                                       | Lpabsyn.AppTerm(h,tms) -> 
                                           Lpabsyn.AppTerm(h, List.append tms [lpvar])
                                       | _ -> Errormsg.error Errormsg.none 
                                                             ("Error applying encoded type " ^ 
                                                               (Lfabsyn.print_typ t) ^ 
                                                               " to term " ^ 
                                                               (Lpabsyn.print_term s)); s) 
            in
            Lpabsyn.Universal(Lpabsyn.Var(name), flatten_type t1, Lpabsyn.ImpGoal(clause, goal))
      | Lfabsyn.IdType(id,_) ->
          fun s ->
          Lpabsyn.Atom(Lpabsyn.AppTerm(hastype, [s; Lpabsyn.IdTerm(getLP id)]))

  (** Process each type declaration in the LF signature. *)
  let process types =
    let perType symb (Lfabsyn.TypeFam(id,kind,_,_,_,objs,_)) typedecls =
      (* make type decl for this constant *)
      let lpid = getLP id in
      let lpname = 
         match lpid with
             Lpabsyn.Const(name) -> name
           | Lpabsyn.Var(name) -> 
             (Errormsg.error Errormsg.none 
                             ("error inserting into type symboltable: " ^ 
                               (Lpabsyn.print_id lpid) ^ 
                               " is not a constant.")); name in
      let typedecls' =
            Symboltable.insert typedecls 
                               (Symbol.symbol lpname)
                               (Lpabsyn.TypeDec(lpid, (flatten_kind kind), ref []))
      in
      (* make istype clause for type, put in istypeClauses *)
      let newClause = ref ((encode_kind kind) (Lpabsyn.IdTerm(lpid))) in
      let _ = istypeClauses := (List.append (!istypeClauses) [newClause]) in
        let perObj typedecls (Lfabsyn.Object(id, ty,_,_,_,_)) =
          (* for object in typedecl's list,
             make type decl for constant
             make hastype clause, put in hastypeClauses *)
          let objlpid = getLP id in
          let objlpname = 
             match objlpid with
                 Lpabsyn.Const(name) -> name
               | Lpabsyn.Var(name) -> 
                 (Errormsg.error Errormsg.none 
                                 ("error inserting into type symboltable: " ^ 
                                   (Lpabsyn.print_id objlpid) ^ 
                                   " is not a constant.")); name in
          let newClause = ref ((encode_type ty) (Lpabsyn.IdTerm(objlpid))) in
          let typedecls' =
            (Symboltable.insert typedecls
                               (Symbol.symbol objlpname)
                               (Lpabsyn.TypeDec(objlpid, (flatten_type ty), ref [])))
          in
          (hastypeClauses := List.append (!hastypeClauses) [newClause];
           typedecls')
        in
        List.fold_left (fun tps o -> perObj tps (!o)) typedecls' (!objs)
    in
    (initkinddecls,
      Symboltable.fold types
                       perType
                       inittypedecls)

  (** Run the optimizations which are on. *)
  let run_optimizations sign =
    let specialized = 
      if (Optimization.Specialize.get ())
        then Optimization.Specialize.run_optimization sign
        else sign
    in
    let swapped =
      if (Optimization.Swap.get ())
        then Optimization.Swap.run_optimization specialized
        else specialized
    in
    swapped

  let translate (Lfsig.Signature(_, types)) =
    let _ = hastypeClauses := []; istypeClauses := [] in
    let (kinddecls, typedecls) = process types in 
    let initial = Lpsig.Signature("top", kinddecls, typedecls) in
    run_optimizations(initial)
end
