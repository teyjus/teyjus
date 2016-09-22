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
        currentTranslation := s; true
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
let istype = Absyn.Constant(istypeSymb, ref Absyn.NoFixity, ref 0, ref true, ref false, ref false,
			    ref false, ref false, ref false,
			    ref (Some(Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(lftype,[]),
						  	             Absyn.ApplicationType(Pervasive.kbool,[])),
					             ref None, ref false))),
			    ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant, ref 0, Errormsg.none)

let hastypeStr = "hastype"
let hastypeSymb = Symbol.symbol hastypeStr
let hastype = Absyn.Constant(hastypeSymb, ref Absyn.NoFixity, ref 0, ref true, ref false, ref false,
			     ref false, ref false, ref false,
			     ref (Some(Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(lfobj,[]),
							              Absyn.ArrowType(Absyn.ApplicationType(lftype, []),
									              Absyn.ApplicationType(Pervasive.kbool,[]))),
					              ref None, ref false))),
			     ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant, ref 0, Errormsg.none)

(* makeApp: takes a head term `h' and a list of argument terms `a1', `a2', ..., `an' 
            and returns an application term `(((h a1) a2) ... an)'
*)
let makeApp h args =
  List.fold_left (fun t a -> Absyn.ApplicationTerm(Absyn.CurriedApplication(t,a),Errormsg.none))
                 h
                 args
		      
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
let rec encode_term constants metadata vars tm =
    match tm with
        Lfabsyn.AbsTerm(id,ty,t,_) ->
          let s = Symbol.symbol (Lfabsyn.get_id_name id) in
          let bvar = Absyn.BoundVar(s,ref None,ref false,ref (Some(flatten_type ty))) in
          let vars' = Table.add s bvar vars in
          Absyn.AbstractionTerm(Absyn.NestedAbstraction(bvar,
							encode_term constants metadata vars' t),
				Errormsg.none)
      | Lfabsyn.AppTerm(head,tms,_) -> 
          let transhead = encode_term constants metadata vars (Lfabsyn.IdTerm(head, Errormsg.none)) in
          let transtms = List.map (encode_term constants metadata vars) tms in
          makeApp transhead transtms
      | Lfabsyn.IdTerm(id,_) ->
          match id with
              Lfabsyn.Const(_,_) ->
                (match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.get_id_name id))) with
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
            | Lfabsyn.Var(n,t,_) ->
                (match (Table.find (Symbol.symbol n) vars) with
                     Some(tysymb) -> Absyn.makeBoundVarTerm tysymb Errormsg.none
                   | None ->
                       Errormsg.error Errormsg.none
                                      ("No variable named `"^n^"' found in scope.");
                       Absyn.ErrorTerm)
            | Lfabsyn.LogicVar(n,t,_) ->
                (match (Table.find (Symbol.symbol n) vars) with
                     Some(tysymb) -> Absyn.makeFreeVarTerm tysymb Errormsg.none
                   | None ->
                       Errormsg.error Errormsg.none
                                      ("No variable named `"^n^"' found in scope.");
                       Absyn.ErrorTerm)
                

(** Encode an LF kind as a term.
      @returns a function that when applied to the encoding of an LF 
               constant `a' produces a term encoding the judgement
               `a : k'. *)			    
let rec encode_kind opt metadata consttbl vars k =
  match k with
      Lfabsyn.PiKind(id,ty,k,_) ->
        fun m ->
          let bvar = Absyn.BoundVar(Symbol.symbol (Lfabsyn.get_id_name id), 
                                    ref None, ref false, ref (Some(flatten_type ty)))
          in
          let vartm = Absyn.makeBoundVarTerm (bvar) Errormsg.none in
          let l = (encode_type_positive opt metadata consttbl vars ty) vartm in
          let r = (encode_kind opt metadata consttbl vars k) (makeApp m [vartm]) in
          let bodytm = makeApp (Absyn.ConstantTerm(Pervasive.implConstant,[],Errormsg.none)) [l;r] in
          let abstm =
            Absyn.AbstractionTerm(
              Absyn.NestedAbstraction(bvar, bodytm),
              Errormsg.none)
          in
          makeApp (Absyn.ConstantTerm(Pervasive.allConstant,[],Errormsg.none)) [abstm]
    | Lfabsyn.ImpKind(l,r,_) ->
        fun m ->
          let bvar = Absyn.BoundVar(Symbol.symbol (newVar ()), 
                                    ref None, ref false, ref (Some(flatten_type l)))
          in
          let vartm = Absyn.makeBoundVarTerm bvar Errormsg.none in
          let l' = (encode_type_positive opt metadata consttbl vars  l) vartm in
          let r' = (encode_kind opt metadata consttbl vars k) (makeApp m [vartm]) in
          let body = makeApp (Absyn.ConstantTerm(Pervasive.implConstant,[],Errormsg.none)) [l';r'] in
          let abstm =
            Absyn.AbstractionTerm(
              Absyn.NestedAbstraction(bvar, body),
              Errormsg.none)
          in
          makeApp (Absyn.ConstantTerm(Pervasive.allConstant,[],Errormsg.none)) [abstm]
    | Lfabsyn.Type(_) ->
        let istype =
          (match Table.find (Symbol.symbol istypeStr) consttbl with
               Some(c) -> c
             | None -> istype)
        in
        fun m ->
          Absyn.ApplicationTerm(Absyn.CurriedApplication(Absyn.ConstantTerm(istype, [], Errormsg.none), m), Errormsg.none)
		  
(** Encode an LF type as a term repsenting a clause.
      @returns a function that when applied to the encoding of an LF 
               constant `c' produces a term encoding the judgement
               `c : t'. *)
and encode_type_negative opt metadata consttbl vars ty =
  match ty with
      Lfabsyn.PiType(id,typ,body,_) ->
        fun m ->
          let bvar = Absyn.BoundVar(Symbol.symbol (Lfabsyn.get_id_name id), 
                                    ref None, ref false, ref (Some(flatten_type ty)))
          in
          let vars' = Table.add (Symbol.symbol (Lfabsyn.get_id_name id)) bvar vars in
          let vartm = Absyn.makeBoundVarTerm bvar Errormsg.none in
          let r = (encode_type_negative opt metadata consttbl vars' body) (makeApp m [vartm]) in
          let bodytm =
            if (opt && Strictness.appears_strict id body)
            then r
            else
              let l = (encode_type_positive opt metadata consttbl vars' typ) vartm in 
              makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l;r]
          in
          let abstm =
            Absyn.AbstractionTerm(
              Absyn.NestedAbstraction(bvar, bodytm),
              Errormsg.none)
          in
          makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none)) [abstm]
    | Lfabsyn.ImpType(l,r,_) ->
        fun m ->
          let varname = newVar () in
          let bvar = Absyn.BoundVar(Symbol.symbol  varname, 
                                    ref None, ref false, ref (Some(flatten_type l)))
          in
          let vars' = Table.add (Symbol.symbol varname) bvar vars in
          let vartm = Absyn.makeBoundVarTerm bvar Errormsg.none in
          let l' = (encode_type_positive opt metadata consttbl vars' l) vartm in
          let r' = (encode_type_negative opt metadata consttbl vars' r) (makeApp m [vartm]) in
          let bodytm = makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l';r'] in
          let abstm =
            Absyn.AbstractionTerm(
              Absyn.NestedAbstraction(bvar, bodytm),
              Errormsg.none)
          in
          makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none)) [abstm]
    | Lfabsyn.AppType(id,tms,_) ->
        let hastype =
          (match Table.find (Symbol.symbol hastypeStr) consttbl with
               Some(c) -> c
             | None -> hastype)
        in
        fun m ->
          (match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.get_id_name id))) with
               Some(s) ->
                 (match Table.find s consttbl with
                      Some(c) ->
                        let lptms = List.map (encode_term consttbl metadata vars) tms in
                        let tytm = makeApp (Absyn.ConstantTerm(c,[],Errormsg.none)) lptms in
                        makeApp (Absyn.ConstantTerm(hastype, [], Errormsg.none)) [m;tytm]
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
        let hastype =
          (match Table.find (Symbol.symbol hastypeStr) consttbl with
               Some(c) -> c
             | None -> hastype)
        in
        fun m ->
          (match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.get_id_name id))) with
               Some(s) ->
                 (match Table.find s consttbl with
                      Some(c) -> makeApp (Absyn.ConstantTerm(hastype, [], Errormsg.none)) [m;Absyn.ConstantTerm(c,[],Errormsg.none)]
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
and encode_type_positive opt metadata consttbl vars ty =
  match ty with
      Lfabsyn.PiType(id,typ,body,_) ->
        fun m ->
          let bvar = Absyn.BoundVar(Symbol.symbol (Lfabsyn.get_id_name id), 
                                    ref None, ref false, ref (Some(flatten_type ty)))
          in
          let vars' = Table.add (Symbol.symbol (Lfabsyn.get_id_name id)) bvar vars in
          let vartm = Absyn.makeBoundVarTerm bvar Errormsg.none in
          let l = (encode_type_negative opt metadata consttbl vars' typ) vartm in
          let r = (encode_type_positive opt metadata consttbl vars' body) (makeApp m [vartm]) in
          let bodytm = makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l;r] in
          let abstm =
            Absyn.AbstractionTerm(
              Absyn.NestedAbstraction(bvar, bodytm),
              Errormsg.none)
          in
          makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none)) [abstm]
    | Lfabsyn.ImpType(l,r,_) ->
        fun m ->
          let varname = newVar () in
          let bvar = Absyn.BoundVar(Symbol.symbol varname, ref None, ref false, ref (Some(flatten_type l))) in
          let vars' = Table.add (Symbol.symbol varname) bvar vars in
          let vartm = Absyn.makeBoundVarTerm bvar Errormsg.none in
          let l' = (encode_type_negative opt metadata consttbl vars' l) vartm in
          let r' = (encode_type_positive opt metadata consttbl vars' r) (makeApp m [vartm]) in
          let bodytm = makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l';r'] in
          let abstm =
            Absyn.AbstractionTerm(
              Absyn.NestedAbstraction(bvar, bodytm),
              Errormsg.none)
          in
          makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none)) [abstm]
    | Lfabsyn.AppType(id,tms,_) ->
        let hastype =
          (match Table.find (Symbol.symbol hastypeStr) consttbl with
               Some(c) -> c
             | None -> hastype)
        in
        fun m ->
          (match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.get_id_name id))) with
               Some(s) ->
                 (match Table.find s consttbl with
                      Some(c) ->
                        let lptms = List.map (encode_term consttbl metadata vars) tms in
                        let tytm = makeApp (Absyn.ConstantTerm(c,[],Errormsg.none)) lptms in
                        makeApp (Absyn.ConstantTerm(hastype, [], Errormsg.none)) [m;tytm]
                    | None ->
                        Errormsg.error Errormsg.none 
                                       ("No constant found for LP symbol: '" ^ (Symbol.printName s) ^ "'");
                        Absyn.ErrorTerm)
             | None ->
                 Errormsg.error Errormsg.none 
                                ("No mapping found for LF constant: '" ^ (Lfabsyn.get_id_name id) ^ "'");
                 Absyn.ErrorTerm)
    | Lfabsyn.IdType(id,_) ->
        let hastype =
          (match Table.find (Symbol.symbol hastypeStr) consttbl with
               Some(c) -> c
             | None -> hastype)
        in
        fun m ->
          (match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.get_id_name id))) with
               Some(s) ->
                 (match Table.find s consttbl with
                      Some(c) ->
                        makeApp (Absyn.ConstantTerm(hastype, [], Errormsg.none)) [m;Absyn.ConstantTerm(c,[],Errormsg.none)]
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
      (Lfabsyn.NoFixity,_) -> Absyn.NoFixity
    | (Lfabsyn.Infix,Lfabsyn.Left) -> Absyn.Infixl
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
      Metadata.new_mapping metadata (Symb.symbol (Lfabsyn.get_id_name id))
    in
    List.fold_left (fun m o -> perObj (!o) m) (Metadata.new_mapping metadata symb) (!objs)
  in
  Symboltable.fold types perType Metadata.empty

(* add constants for each type and each object-level constant to the constant table being built. *)
let initialize_constants metadata types=
  let perType symb (Lfabsyn.TypeFam(id,kind,fix,assoc,prec,objs,_)) constants =
    let perObj (Lfabsyn.Object(id, ty,fix,assoc,prec,_)) constants =
      let s = Symb.symbol (Lfabsyn.get_id_name id) in
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
    match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.get_id_name id))) with
        Some(s) ->
          (match (Table.find s constants) with
               Some(c) ->
                 let aterm = Absyn.ConstantTerm(c, [], Errormsg.none) in
                 let clause = (encode_type_negative strictness metadata constants Table.empty typ) aterm in
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
    match (Metadata.getLP metadata (Symb.symbol (Lfabsyn.get_id_name id))) with
        Some(s) ->
          (match (Table.find s constants) with
               Some(c) ->
                 (* Taking out istype clauses for now. It is unclear how to deal with them 
                    in the optimizations, or when they might be useful in this application. *)
                 (*let clause = encode_kind strictness metadata constants kind (Absyn.ConstantTerm(c, [], Errormsg.none)) in
                 List.fold_left (fun c o -> perObj (!o) c) (List.append clauselst [clause]) (!objs)*)
                 List.fold_left (fun c o -> perObj (!o) c) clauselst (!objs)
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
    let makeAnd newtm tm =
      makeApp (Absyn.ConstantTerm(Pervasive.andConstant,[],Errormsg.none)) [newtm; tm]
    in
    let buildTerm pairs =
      let rec helper pairs typesymbTable (terms, typesymbs) =
        match pairs with
            ((Lfabsyn.LogicVar(n,_,_) as id,t)::pairs') ->
              let typsymb = Absyn.ImplicitVar(Symbol.symbol n, 
                                              ref None, 
                                              ref true, 
                                              ref (Some(flatten_type t))) 
              in
              let typesymbTable' = Table.add (Symbol.symbol n) typsymb typesymbTable in
              let varterm = Absyn.makeFreeVarTerm typsymb Errormsg.none in
              let enctype = (encode_type_positive false metadata constTab typesymbTable' t) varterm in
              helper pairs' typesymbTable' (enctype :: terms, typsymb :: typesymbs)
          | [] -> (terms, typesymbs)
      in
      let (terms, fvars) = helper pairs Table.empty ([],[]) in
      (List.fold_left makeAnd (List.hd terms) (List.tl terms), fvars) 
    in
    buildTerm (List.append vars [(pt, ty)])
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
    let makeAnd newtm tm =
      makeApp (Absyn.ConstantTerm(Pervasive.andConstant,[],Errormsg.none)) [tm; newtm]
    in
    let buildTerm pairs (Lfabsyn.LogicVar(pt,_,_)) lftype  =
      let strict_in_list id typs = List.exists (Strictness.appears_strict id) typs in
      let makeImplicitVar name ty = Absyn.ImplicitVar(Symbol.symbol name, 
                                                      ref None, 
                                                      ref true, 
                                                      ref (Some(flatten_type ty))) 
      in
      let rec helper pairs typesymbTable (terms, typesymbs) =
        match pairs with
            ((Lfabsyn.LogicVar(n,_,_) as id,t)::pairs') ->
              let typsymb = makeImplicitVar n t in
              let typesymbTable' = Table.add (Symbol.symbol n) typsymb typesymbTable in
              let varterm = Absyn.makeFreeVarTerm typsymb Errormsg.none in
              if (strict_in_list id (lftype :: snd (List.split pairs')))
              then
                helper pairs' typesymbTable' (terms, (typsymb :: typesymbs))
              else
                let enctype = (encode_type_positive true metadata constTab typesymbTable' t) varterm in
                helper pairs' typesymbTable' (enctype :: terms, typsymb :: typesymbs)
          | [] -> 
              let typsymb = makeImplicitVar pt lftype in
              let typesymbTable' = Table.add (Symbol.symbol pt) typsymb typesymbTable in
              let proofterm = Absyn.makeFreeVarTerm typsymb Errormsg.none in
              let enctype = (encode_type_positive true metadata constTab typesymbTable' lftype) proofterm in
              (enctype :: terms, typsymb :: typesymbs)
      in
      let (terms, fvars) = helper pairs Table.empty ([],[]) in
      (List.fold_left makeAnd (List.hd terms) (List.tl terms), fvars) 
    in
    let (unop_query, fvars) = buildTerm vars pt ty in
    (optimize unop_query, fvars)
end
