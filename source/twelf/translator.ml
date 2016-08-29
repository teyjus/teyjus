(** Translators for translating LF specifications into LP programs. *)

module type Translator =
sig
  (** Translate the given LF signature into an LP signature. *)
  val translate : Lfsig.signature -> 
                    (Metadata.metadata * 
                      Absyn.akind Table.SymbolTable.t * 
                      Absyn.aconstant Table.SymbolTable.t * 
                      Absyn.aterm list)

  val translate_query : Lfabsyn.query -> Metadata.metadata -> 
                          Absyn.akind Table.SymbolTable.t -> 
                          Absyn.aconstant Table.SymbolTable.t -> (Absyn.aterm * Absyn.atypesymbol list)
end

let currentTranslation = ref "naive"
let set_translation s =
  match s with
      "naive" 
    | "optimized" ->
        currentTranslation = ref s; true
    | _ -> Errormsg.warning Errormsg.none ("Invalid translation: " ^ s);
           false
let get_translation () = !currentTranslation

(* Generate unique names for variables generated during 
   translation. *)
let newVarCount = ref 0
let newVar () = 
  let vname = "X_" ^ (string_of_int !newVarCount) in
  let _ = newVarCount := !newVarCount + 1 in
  vname

(* Construct the two kinds lftype and lfobj *)
let lftypeStr = "lf_type"
let lftypeSymb = Symbol.symbol lftypeStr
let lftype = Absyn.Kind(lftypeSymb, Some(0), ref 0, Absyn.GlobalKind, Errormsg.none)

let lfobjStr = "lf_object"
let lfobjSymb = Symbol.symbol lfobjStr
let lfobj = Absyn.Kind(lfobjSymb, Some(0), ref 0, Absyn.GlobalKind, Errormsg.none)
		   
(* Construct the two default predicates istype and hastype *)
let istypeStr = "istype"
let istypeSymb = Symbol.symbol istypeStr
let istype = Absyn.Constant(istypeSymb, ref Absyn.Prefix, ref 0, ref true, ref false, ref false,
			    ref false, ref false, ref false,
			    ref (Some(Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(lftype,[]),
						  	             Absyn.ApplicationType(Pervasive.kbool,[])),
					             ref None, ref false))),
			    ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant, ref 0, Errormsg.none)

let hastypeStr = "hastype"
let hastypeSymb = Symbol.symbol hastypeStr
let hastype = Absyn.Constant(hastypeSymb, ref Absyn.Prefix, ref 0, ref true, ref false, ref false,
			     ref false, ref false, ref false,
			     ref (Some(Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(lfobj,[]),
							              Absyn.ArrowType(Absyn.ApplicationType(lftype, []),
									              Absyn.ApplicationType(Pervasive.kbool,[]))),
					              ref None, ref false))),
			     ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant, ref 0, Errormsg.none)
		      
(** Flatten an LF kind into a simple type. *)
let rec flatten_kind k =
  match k with
      Lfabsyn.PiKind(_, ty, body, _) -> 
        Absyn.ArrowType((flatten_type ty), (flatten_kind body))
    | Lfabsyn.ImpKind(l, r, _) -> 
        Absyn.ArrowType((flatten_type l), (flatten_kind r))
    | Lfabsyn.Type(_) ->
        Absyn.ApplicationType(lftype,[])      
(** Flatten an LF type into a simple type. *)                                       
and flatten_type t =
  match t with
      Lfabsyn.PiType(id, ty, body, _) ->
        Absyn.ArrowType((flatten_type ty), (flatten_type body))
    | Lfabsyn.ImpType(l,r,_) -> 
        Absyn.ArrowType((flatten_type l), (flatten_type r))
    | Lfabsyn.AppType(_,_,_)
    | Lfabsyn.IdType(_,_) -> Absyn.ApplicationType(lfobj,[])
						  
(** Encode an LF term into a simply typed term. *)
let rec encode_term constants metadata tm =
    match tm with
        Lfabsyn.AbsTerm(id,ty,t,_) ->
          let s = Symbol.symbol (Lfabsyn.string_of_id id) in
          Absyn.AbstractionTerm(Absyn.NestedAbstraction(Absyn.BoundVar(s,
                                                                       ref None,
								       ref false,
								       ref (Some(flatten_type ty))),
							encode_term constants metadata t),
				Errormsg.none)
      | Lfabsyn.AppTerm(head,tms,_) -> 
          let transtms = List.map (encode_term constants metadata) tms in
          Absyn.ApplicationTerm(Absyn.FirstOrderApplication(encode_term constants metadata (Lfabsyn.IdTerm(head, Errormsg.none)), 
                                                            List.map (encode_term constants metadata) tms, 
                                                            List.length tms),
				Errormsg.none)
      | Lfabsyn.IdTerm(id,_) ->
          match id with
              Lfabsyn.Const(_,_) ->
                (match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.string_of_id id))) with
                     Some(s) ->
                       (match (Table.find s constants) with
                            Some(c) ->
                              Absyn.ConstantTerm(c, [], Errormsg.none)
                          | None ->
                              Errormsg.error Errormsg.none 
                                             ("No constant found for LP symbol: '" ^ (Symbol.printName s) ^ 
                                                  "' in LF term: '" ^ (Lfabsyn.string_of_term tm) ^ "'");
                              Absyn.ErrorTerm)
                   | None ->
                       Errormsg.error Errormsg.none 
                                      ("No mapping found for LF constant: '" ^ (Lfabsyn.string_of_id id) ^ 
                                           "' in LF term: '" ^ (Lfabsyn.string_of_term tm) ^ "'");
                       Absyn.ErrorTerm)
            | Lfabsyn.Var(n,_) ->
                Absyn.makeBoundVarTerm (Absyn.BoundVar(Symbol.symbol n,
                                                       ref None,
                                                       ref false,
                                                       ref None))
                                       Errormsg.none
            | Lfabsyn.LogicVar(n,_) ->
                Absyn.makeFreeVarTerm (Absyn.ImplicitVar(Symbol.symbol n,
                                                         ref None,
                                                         ref false,
                                                         ref None))
                                      Errormsg.none

(** Encode an LF kind as a term.
      @returns a function that when applied to the encoding of an LF 
               constant `a' produces a term encoding the judgement
               `a : k'. *)			    
let rec encode_kind opt metadata consttbl k =
  match k with
      Lfabsyn.PiKind(id,ty,k,_) ->
        fun m ->
          let vartm = 
            Absyn.makeBoundVarTerm (Absyn.BoundVar(Symbol.symbol (Lfabsyn.string_of_id id), 
                                                   ref None, ref false, ref (Some(flatten_type ty)))) 
                                   Errormsg.none in
          let l = encode_type_positive opt metadata consttbl ty vartm in
          let r = encode_kind opt metadata consttbl k (Absyn.ApplicationTerm(Absyn.CurriedApplication(m, vartm), Errormsg.none)) in
          let bodytm = 
            Absyn.ApplicationTerm(
              Absyn.FirstOrderApplication(
                Absyn.ConstantTerm(Pervasive.implConstant,[],Errormsg.none), 
                [l; r], 2), 
              Errormsg.none) in
          Absyn.ApplicationTerm(
            Absyn.FirstOrderApplication(
              Absyn.ConstantTerm(Pervasive.allConstant,[],Errormsg.none), 
              [vartm; bodytm], 2), 
            Errormsg.none)
    | Lfabsyn.ImpKind(l,r,_) ->
        fun m ->
          let vartm = 
            Absyn.makeBoundVarTerm (Absyn.BoundVar(Symbol.symbol (newVar ()), 
                                                   ref None, ref false, ref (Some(flatten_type l)))) 
                                   Errormsg.none in
          let l' = encode_type_positive opt metadata consttbl l vartm in
          let r' = encode_kind opt metadata consttbl k (Absyn.ApplicationTerm(Absyn.CurriedApplication(m, vartm), Errormsg.none)) in
          let body = 
            Absyn.ApplicationTerm(
              Absyn.FirstOrderApplication(
                Absyn.ConstantTerm(Pervasive.implConstant,[],Errormsg.none), 
                [l'; r'], 2), 
              Errormsg.none) in
          Absyn.ApplicationTerm(
            Absyn.FirstOrderApplication(
              Absyn.ConstantTerm(Pervasive.allConstant,[],Errormsg.none), 
              [vartm; body], 2), 
            Errormsg.none)
    | Lfabsyn.Type(_) ->
        fun m ->
          Absyn.ApplicationTerm(Absyn.FirstOrderApplication(Absyn.ConstantTerm(istype, [], Errormsg.none), [m], 1), Errormsg.none)
		  
(** Encode an LF type as a term repsenting a clause.
      @returns a function that when applied to the encoding of an LF 
               constant `c' produces a term encoding the judgement
               `c : t'. *)
and encode_type_negative opt metadata consttbl ty =
  match ty with
      Lfabsyn.PiType(id,typ,body,_) ->
        fun m ->
          let vartm =
            Absyn.makeBoundVarTerm (Absyn.BoundVar(Symbol.symbol (Lfabsyn.string_of_id id), 
                                                   ref None, ref false, ref (Some(flatten_type ty)))) 
                                   Errormsg.none in
          let r = 
            encode_type_negative opt metadata consttbl body 
                                 (Absyn.ApplicationTerm(Absyn.CurriedApplication(m, vartm), Errormsg.none)) in
          let bodytm =
            if (opt && Strictness.appears_strict id body)
            then r
            else
              let l = 
                encode_type_positive opt metadata consttbl typ vartm in
                Absyn.ApplicationTerm(
                  Absyn.FirstOrderApplication(
                    Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none), 
                    [l;r], 2), 
                  Errormsg.none) 
            in
         Absyn.ApplicationTerm(
           Absyn.FirstOrderApplication(
             Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none), 
             [vartm; bodytm], 2), 
           Errormsg.none)
    | Lfabsyn.ImpType(l,r,_) ->
        fun m ->
          let vartm =
            Absyn.makeBoundVarTerm (Absyn.BoundVar(Symbol.symbol (newVar ()), 
                                    ref None, ref false, ref (Some(flatten_type l)))) 
                                   Errormsg.none in
          let l' = encode_type_positive opt metadata consttbl l vartm in
          let r' = 
            encode_type_negative opt metadata consttbl r 
                                 (Absyn.ApplicationTerm(Absyn.CurriedApplication(m, vartm), Errormsg.none)) in
          let bodytm =
            Absyn.ApplicationTerm(
              Absyn.FirstOrderApplication(
                Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none), 
                [l';r'], 2), 
              Errormsg.none) 
          in
          Absyn.ApplicationTerm(
            Absyn.FirstOrderApplication(
              Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none), 
              [vartm; bodytm], 2), 
            Errormsg.none)
    | Lfabsyn.AppType(id,tms,_) ->
        fun m ->
          (match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.string_of_id id))) with
               Some(s) ->
                 (match Table.find s consttbl with
                      Some(c) ->
                        let lptms = List.map (encode_term consttbl metadata) tms in
                        let tytm = 
                              Absyn.ApplicationTerm(
                                Absyn.FirstOrderApplication(Absyn.ConstantTerm(c,[],Errormsg.none), lptms, List.length lptms), 
                                Errormsg.none) in
                        Absyn.ApplicationTerm(
                          Absyn.FirstOrderApplication(
                            Absyn.ConstantTerm(hastype, [], Errormsg.none), 
                            [m;tytm], 2), 
                          Errormsg.none)
                    | None ->
                        Errormsg.error Errormsg.none 
                                       ("No constant found for LP symbol: '" ^ (Symbol.printName s) ^ 
                                            "' in LF type: '" ^ (Lfabsyn.string_of_typ ty) ^ "'");
                        Absyn.ErrorTerm)
             | None ->
                 Errormsg.error Errormsg.none 
                                ("No mapping found for LF constant: '" ^ (Lfabsyn.string_of_id id) ^ 
                                     "' in LF type: '" ^ (Lfabsyn.string_of_typ ty) ^ "'");
                 Absyn.ErrorTerm)
    | Lfabsyn.IdType(id,_) ->
        fun m ->
          (match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.string_of_id id))) with
               Some(s) ->
                 (match Table.find s consttbl with
                      Some(c) ->
                        Absyn.ApplicationTerm(
                          Absyn.FirstOrderApplication(
                            Absyn.ConstantTerm(hastype, [], Errormsg.none), 
                            [m;Absyn.ConstantTerm(c,[],Errormsg.none)], 2), 
                          Errormsg.none)
                    | None ->
                        Errormsg.error Errormsg.none 
                                       ("No constant found for LP symbol: '" ^ (Symbol.printName s) ^ "'");
                        Absyn.ErrorTerm)
             | None ->
                 Errormsg.error Errormsg.none 
                                ("No mapping found for LF constant: '" ^ (Lfabsyn.string_of_id id) ^ "'");
                 Absyn.ErrorTerm)
		 
(** Similar to {!encode_type_negative} but generates a term representing
    a goal rather than a clause. *)
and encode_type_positive opt metadata consttbl ty =
  match ty with
      Lfabsyn.PiType(id,typ,body,_) ->
        fun m ->
          let vartm =
            Absyn.makeBoundVarTerm (Absyn.BoundVar(Symbol.symbol (Lfabsyn.string_of_id id), 
                                                   ref None, ref false, ref (Some(flatten_type ty)))) 
                                   Errormsg.none in
          let l = 
            encode_type_negative opt metadata consttbl typ vartm in
          let r = 
            encode_type_positive opt metadata consttbl body 
                                 (Absyn.ApplicationTerm(Absyn.CurriedApplication(m, vartm), Errormsg.none)) in
          let bodytm = 
            Absyn.ApplicationTerm(
              Absyn.FirstOrderApplication(
                Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none), 
                [l;r], 2), 
              Errormsg.none) in
         Absyn.ApplicationTerm(
           Absyn.FirstOrderApplication(
             Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none), 
             [vartm; bodytm], 2), 
           Errormsg.none)
    | Lfabsyn.ImpType(l,r,_) ->
        fun m ->
          let vartm =
            Absyn.makeBoundVarTerm (Absyn.BoundVar(Symbol.symbol (newVar ()), ref None, ref false, ref (Some(flatten_type l)))) 
                                   Errormsg.none in
          let l' = 
            encode_type_negative opt metadata consttbl l vartm in
          let r' = 
            encode_type_positive opt metadata consttbl r 
                                 (Absyn.ApplicationTerm(Absyn.CurriedApplication(m, vartm), Errormsg.none)) in
          let bodytm = 
            Absyn.ApplicationTerm(
              Absyn.FirstOrderApplication(
                Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none), 
                [l';r'], 2), 
              Errormsg.none) in
         Absyn.ApplicationTerm(
           Absyn.FirstOrderApplication(
             Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none), 
             [vartm; bodytm], 2), 
           Errormsg.none)
    | Lfabsyn.AppType(id,tms,_) ->
        fun m ->
          (match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.string_of_id id))) with
               Some(s) ->
                 (match Table.find s consttbl with
                      Some(c) ->
                        let lptms = List.map (encode_term consttbl metadata) tms in
                        let tytm = 
                              Absyn.ApplicationTerm(
                                Absyn.FirstOrderApplication(Absyn.ConstantTerm(c,[],Errormsg.none), lptms, List.length lptms), 
                                Errormsg.none) in
                        Absyn.ApplicationTerm(
                          Absyn.FirstOrderApplication(
                            Absyn.ConstantTerm(hastype, [], Errormsg.none), 
                            [m;tytm], 2), 
                          Errormsg.none)
                    | None ->
                        Errormsg.error Errormsg.none 
                                       ("No constant found for LP symbol: '" ^ (Symbol.printName s) ^ "'");
                        Absyn.ErrorTerm)
             | None ->
                 Errormsg.error Errormsg.none 
                                ("No mapping found for LF constant: '" ^ (Lfabsyn.string_of_id id) ^ "'");
                 Absyn.ErrorTerm)
    | Lfabsyn.IdType(id,_) ->
        fun m ->
          (match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.string_of_id id))) with
               Some(s) ->
                 (match Table.find s consttbl with
                      Some(c) ->
                        Absyn.ApplicationTerm(
                          Absyn.FirstOrderApplication(
                            Absyn.ConstantTerm(hastype, [], Errormsg.none), 
                            [m;Absyn.ConstantTerm(c,[],Errormsg.none)], 2), 
                          Errormsg.none)
                    | None ->
                        Errormsg.error Errormsg.none 
                                       ("No constant found for LP symbol: '" ^ (Symbol.printName s) ^ "'");
                        Absyn.ErrorTerm)
             | None ->
                 Errormsg.error Errormsg.none 
                                ("No mapping found for LF constant: '" ^ (Lfabsyn.string_of_id id) ^ "'");
                 Absyn.ErrorTerm)

let trans_fixity fix assoc =
  match (fix, assoc) with
      (Lfabsyn.Infix,Lfabsyn.Left) -> Absyn.Infixl
    | (Lfabsyn.Infix,Lfabsyn.Right) -> Absyn.Infixr
    | (Lfabsyn.Infix,_) -> Absyn.Infix
    | (Lfabsyn.Prefix,Lfabsyn.Right) -> Absyn.Prefixr
    | (Lfabsyn.Prefix,_) -> Absyn.Prefix
    | (Lfabsyn.Postfix,Lfabsyn.Left) ->	Absyn.Postfixl
    | (Lfabsyn.Postfix,_) -> Absyn.Postfix
		       

(* set up the name mapping for constants.
     Per typefam:
       create mapping for the type-level constant,
       per object associated with type fam:
         create mapping for the object-level constant *)
let initialize_metadata types =
  let perType symb (Lfabsyn.TypeFam(_,_,_,_,_,objs,_)) metadata =
    let perObj (Lfabsyn.Object(id,_,_,_,_,_)) metadata =
      Metadata.new_mapping metadata (Symb.symbol (Lfabsyn.string_of_id id))
    in
    List.fold_left (fun m o -> perObj (!o) m) (Metadata.new_mapping metadata symb) (!objs)
  in
  Symboltable.fold types perType Metadata.empty

(* add constants for each type and each object-level constant to the constant table being built. *)
let initialize_constants metadata types=
  let perType symb (Lfabsyn.TypeFam(id,kind,fix,assoc,prec,objs,_)) constants =
    let perObj (Lfabsyn.Object(id, ty,fix,assoc,prec,_)) constants =
      let s = Symb.symbol (Lfabsyn.string_of_id id) in
      let lpsymb = Option.get (Metadata.getLP metadata s) in
      let objconst =
        Absyn.Constant(lpsymb, ref (trans_fixity fix assoc), ref prec,
                       ref true, ref false, ref false, ref false, ref false, ref false,
		       ref (Some(Absyn.Skeleton(flatten_type ty, ref None, ref false))),
		       ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant,
		       ref 0, Errormsg.none)
      in
      Table.add lpsymb objconst constants
    in
    let lpsymb = Option.get (Metadata.getLP metadata symb) in
    let tyconst =
      Absyn.Constant(lpsymb, ref (trans_fixity fix assoc), ref prec,
	  	     ref true, ref false, ref false, ref false, ref false, ref false,
		     ref (Some(Absyn.Skeleton(flatten_kind kind, ref None, ref false))),
		     ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant,
		     ref 0, Errormsg.none)
    in
    List.fold_left (fun c o -> perObj (!o) c) (Table.add lpsymb tyconst constants) (!objs)
  in
  Symboltable.fold types perType (Table.add (Symbol.symbol hastypeStr)
					    hastype
					    (Table.add (Symbol.symbol istypeStr)
						       istype
						       Table.empty))


(* Process each type level declaration and each corresponding object level declaration. *)
let process strictness metadata constants types =
  let perObj (Lfabsyn.Object(id,typ,_,_,_,_) as o) clauselst =
    match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.string_of_id id))) with
        Some(s) ->
          (match (Table.find s constants) with
               Some(c) ->
                 let aterm = Absyn.ConstantTerm(c, [], Errormsg.none) in
                 let clause = (encode_type_negative strictness metadata constants typ) aterm in
                 List.append clauselst [clause]
             | None ->
                 Errormsg.error Errormsg.none 
                                ("No constant found for LP symbol: '" ^ (Symbol.printName s) ^ 
                                     "' from LF object decl: '" ^ (Lfabsyn.string_of_obj o) ^ "'");
                 clauselst)
      | None ->
          Errormsg.error Errormsg.none 
                         ("No mapping found for LF constant: '" ^ (Lfabsyn.string_of_id id) ^
                              "' from LF object decl: '" ^ (Lfabsyn.string_of_obj o) ^ "'");
          clauselst
  in
  let perType symb ((Lfabsyn.TypeFam(id,kind,_,_,_,objs,_)) as t) clauselst =
    match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.string_of_id id))) with
        Some(s) ->
          (match (Table.find s constants) with
               Some(c) ->
                 let clause = encode_kind strictness metadata constants kind (Absyn.ConstantTerm(c, [], Errormsg.none)) in
                 List.fold_left (fun c o -> perObj (!o) c) (List.append clauselst [clause]) (!objs)
             | None ->
                 Errormsg.error Errormsg.none 
                                ("No constant found for LP symbol: '" ^ (Symbol.printName s) ^ 
                                     "' from LF type decl: '" ^ (Lfabsyn.string_of_typefam t) ^ "'");
                 clauselst)
      | None ->
          Errormsg.error Errormsg.none 
                         ("No mapping found for LF constant: '" ^ (Lfabsyn.string_of_id id) ^
                              "' from LF type decl: '" ^ (Lfabsyn.string_of_typefam t) ^ "'");
          clauselst
  in
  Symboltable.fold types perType []

module NaiveTranslation : Translator =
struct
  let translate (Lfsig.Signature(name,types)) =
    let metadata = initialize_metadata types in
    let kinds = Table.add (Symbol.symbol lfobjStr) lfobj (Table.add (Symbol.symbol lftypeStr) lftype Table.empty) in
    let constants = initialize_constants metadata types in
    let clauses = process false metadata constants types in
    (metadata, kinds, constants, clauses)

  let translate_query (Lfabsyn.Query(vars, pt, ty)) metadata kindTab constTab =
    let enc_type = encode_type_positive false metadata constTab in
    let make_var (Lfabsyn.Var(n,_), t) =
      (Absyn.FreeVarTerm(Absyn.NamedFreeVar(Absyn.ImplicitVar(Symbol.symbol n, ref None, ref false, ref (Some(flatten_type ty)))), 
                         Errormsg.none),
       enc_type ty)
    in
    let lp_vars = List.map make_var vars in
    let add_term tm (vartm, encty) =
      Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(
          Absyn.ConstantTerm(Pervasive.andConstant,[],Errormsg.none), 
          [tm; (encty vartm)], 
          2), 
        Errormsg.none) in 
    let (pfvar, c) = make_var (pt, ty) in
    let qterm = List.fold_left add_term (c pfvar) lp_vars in
    (qterm, List.map (fun (x,y) -> Absyn.getTermFreeVariableTypeSymbol x) lp_vars)
end

module OptimizedTranslation : Translator =
struct
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

  let optimize tm =
    let specialized =
      if (Optimization.Specialize.get ())
        then Optimization.Specialize.optimize tm
        else tm
    in
    let swapped =
      if (Optimization.Swap.get ())
        then Optimization.Swap.optimize specialized
        else specialized
    in
    swapped
      
  let translate (Lfsig.Signature(name, types)) =
    let metadata = initialize_metadata types in
    let kinds = Table.add (Symbol.symbol lfobjStr) lfobj (Table.add (Symbol.symbol lftypeStr) lftype Table.empty) in
    let constants = initialize_constants metadata types in
    let clauses = process true metadata constants types in
    let solun = (metadata, kinds, constants, clauses) in
    run_optimizations solun
    

  let translate_query (Lfabsyn.Query(vars, pt, ty)) metadata kindTab constTab =
    let enc_type = encode_type_positive true metadata constTab in
    let make_var (Lfabsyn.Var(n,_), t) =
      (Absyn.FreeVarTerm(Absyn.NamedFreeVar(Absyn.ImplicitVar(Symbol.symbol n, ref None, ref false, ref (Some(flatten_type ty)))), 
                         Errormsg.none),
       enc_type ty)
    in
    let lp_vars = List.map make_var vars in
    let add_term tm (vartm, encty) =
      Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(
          Absyn.ConstantTerm(Pervasive.andConstant,[],Errormsg.none), 
          [tm; (encty vartm)], 
          2), 
        Errormsg.none) in
    let (pfvar, c) = make_var (pt, ty) in
    let qterm = List.fold_left add_term (c pfvar) lp_vars in
    (optimize qterm, List.map (fun (x,y) -> Absyn.getTermFreeVariableTypeSymbol x) lp_vars) 
end
