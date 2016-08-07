(** Translators for translating LF specifications into LP programs. *)

module type Translator =
sig
  val translate : Lfsig.signature -> Absyn.amodule
end

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

(* Need the o type for predicates *)
(* NOTE: Not sure if this is correct... *)
let o = Option.get (Table.lookup (Symbol.symbol "o") Pervasive.pervasiveKinds)
		   
(* Construct the two default predicates istype and hastype *)
let istypeStr = "istype"
let istypeSymb = Symbol.symbol istypeStr
let istype = Absyn.Constant(istypeSymb, Absyn.Prefix, ref 0, ref true, ref false, ref false,
			    ref false, ref false, ref false,
			    Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(lftype,[]),
							   Absyn.ApplicationType(o,[])),
					   ref None, ref false),
			    ref 0, ref None, ref None, ref None, Absyn.GlobalConstant, ref 0, Errormsg.none)

let hastypeStr = "hastype"
let hastypeSymb = Symbol.symbol hastypeStr
let hastype = Absyn.Constant(hastypeSymb, Absyn.Prefix, ref 0, ref true, ref false, ref false,
			     ref false, ref false, ref false,
			     Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(lfobj,[]),
							    Absyn.ArrowType(Absyn.ApplicationType(lftype, []),
									    Absyn.ApplicationType(o,[]))),
					    ref None, ref false),
			     ref 0, ref None, ref None, ref None, Absyn.GlobalConstant, ref 0, Errormsg.none)
		      
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
let rec encode_term constants metadata t =
    match t with
        Lfabsyn.AbsTerm(id,ty,tm,_) ->
          let s = Metadata.getLP metadata (Symb.symbol (Lfabsyn.string_of_id id)) in
          Absyn.AbstractionTerm(Absyn.NestedAbstraction(Absyn.BoundVar(s,
                                                                       ref None,
								       ref false,
								       flatten_type ty),
							encode_term constants metadata tm),
				Errormsg.none)
      | Lfabsyn.AppTerm(head,tms,_) -> 
          let transtms = List.map (encode_term constants metadata) tms in
          Absyn.ApplicationTerm(Absyn.FirstOrderApplication(encode_term constants metadata head, tms, List.length tms),
				Errormsg.none)
      | Lfabsyn.IdTerm(id,_) ->
	  let s = Metadata.getLP metadata (Symb.symbol (Lfabsyn.string_of_id id)) in
	  let c = Table.find s constants in
	  match c with
              Some(const) ->
                Absyn.ConstantTerm(const, [], Errormsg.none)
	    | None ->
                Absyn.BoundVarTerm(Absyn.NamedBoundVar(Absyn.BoundVar(Symbol.symbol (Symb.name s),
								      ref None,
								      ref false,
								      ref None)),
				   Errormsg.none)

(** Encode an LF kind as a clause.
      @returns a function that when applied to the encoding of an LF 
               constant `a' produces a clause encoding the judgement
               `a : k'. *)			    
let rec encode_kind strictness constants metadata k =
  match k with
      Lfabsyn.PiKind(id,ty,body,_) ->
        fun tm ->
          let s = Symbol.symbol (string_of_id id) in
	  let flatty = flatten_type ty in
          let v = Absyn.BoundVarTerm(Absyn.NamedBoundVar(Absyn.BoundVar(s, ref None, ref false, ref flatty)),
				     Errormsg.none) in
          let head = (encode_kind strictness constants metadata body)
		     (Absyn.ApplicationTerm(Absyn.FirstOrderApplication(tm,[v],1), Errormsg.none)) in
	  let goal = (encode_type_as_goal strictness constants metadata ty) v in
	  (match head with
	       Absyn.Fact(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset, mods) ->
                 Absyn.Rule(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset,
	                    goal, ref Absyn.GoalEnvAssoc([]), ref None, ref false, [])
	     | Absyn.Rule(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset, g, lst, cut, env, mods) ->
	         Absyn.Rule(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset,
	 		    Absyn.AndGoal(g, goal), lst, cut, env, mods))
    | Lfabsyn.ImpKind(ty,body,_) ->
        fun tm ->
          let newvar = newVar () in
	  let s = Symbol.symbol newvar in
	  let flatty = flatten_type ty in
          let v = Absyn.BoundVarTerm(Absyn.NamedBoundVar(Absyn.BoundVar(s, ref None, ref false, ref flatty)),
				     Errormsg.none) in
          let head = (encode_kind strictness constants metadata body)
		     (Absyn.ApplicationTerm(Absyn.FirstOrderApplication(tm,[v],1), Errormsg.none)) in
	  let goal = (encode_type_as_goal strictness constants metadata ty) v in
	  (match head with
	       Absyn.Fact(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset, mods) ->
                 Absyn.Rule(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset,
	                    goal, ref Absyn.GoalEnvAssoc([]), ref None, ref false, [])
	     | Absyn.Rule(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset, g, lst, cut, env, mods) ->
	         Absyn.Rule(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset,
			    Absyn.AndGoal(g, goal), lst, cut, env, mods))
    | Lfabsyn.Type(_) ->
        fun tm ->
          Absyn.Fact(istype, [tm], [], 1, 0, Absyn.TermVarMap([]), Absyn.TypeVarMap([]), [], ref None, [])
		  
(** Encode an LF type as a clause.
      @returns a function that when applied to the encoding of an LF 
               constant `c' produces a clause encoding the judgement
               `c : t'. *)
and encode_type strictness constants metadata t =
  match t with
      Lfabsyn.PiType(id,ty,body,_) ->
        fun tm ->
          let s = Symbol.symbol (string_of_id id) in
	  let flatty = flatten_type ty in
          let v = Absyn.BoundVarTerm(Absyn.NamedBoundVar(Absyn.BoundVar(s, ref None, ref false, ref flatty)),
				     Errormsg.none) in
	  let head = (encode_type strictness constants metadata body)
	             Absyn.ApplicationTerm(Absyn.FirstOrderApplication(tm, [v], 0)) in
	  if (strictness && (Strictness.appears_strict id body))
	    then
	      head
	    else
              let goal = (encode_type_as_goal strictness constants metadata ty) v in
              (match head with
	           Absyn.Fact(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset, mods) ->
                     Absyn.Rule(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset,
	                        goal, ref Absyn.GoalEnvAssoc([]), ref None, ref false, [])
	         | Absyn.Rule(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset, g, lst, cut, env, mods) ->
	             Absyn.Rule(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset,
		    	        Absyn.AndGoal(g, goal), lst, cut, env, mods))
    | Lfabsyn.ImpType(l,r,_) ->
        fun tm ->
          let newvar = newVar () in
	  let s = Symbol.symbol newvar in
	  let flatty = flatten_type ty in
          let v = Absyn.BoundVarTerm(Absyn.NamedBoundVar(Absyn.BoundVar(s, ref None, ref false, ref flatty)),
				     Errormsg.none) in
	  let head = (encode_type strictness constants metadata body)
	             Absyn.ApplicationTerm(Absyn.FirstOrderApplication(tm, [v], 0)) in
	  if (strictness && (Strictness.appears_strict id body))
	    then
	      head
	    else
              let goal = (encode_type_as_goal strictness constants metadata ty) v in
              (match head with
	           Absyn.Fact(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset, mods) ->
                     Absyn.Rule(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset,
	                        goal, ref Absyn.GoalEnvAssoc([]), ref None, ref false, [])
	         | Absyn.Rule(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset, g, lst, cut, env, mods) ->
	             Absyn.Rule(pred, tms, typs, numtms, numtyps, tmmap, typmap, logic, offset,
		    	        Absyn.AndGoal(g, goal), lst, cut, env, mods))
    | Lfabsyn.AppType(id,tms,_) ->
       fun tm ->
         let c = Option.get (Table.find (Symbol.symbol (Metadata.getLP metadata (string_of_id id))) constants) in
         let transtms = List.map (encode_term constants metadata) tms in
	 let t2 = Absyn.ApplicationTerm(Absyn.FirstOrderApplication(Absyn.ConstantTerm(c, [], Errormsg.none),
								    transtms, List.length transtms),
					Errormsg.none) in
	 Absyn.Face(hastype, [tm, t2], [], 2, 0, Absyn.TermVarMap([]), Absyn.TypeVarMap([]), [], ref None, [])
    | Lfabsyn.IdType(id,_) ->
       fun tm ->
         let c = Option.get (Table.find (Symbol.symbol (Metadata.getLP metadata (string_of_id id))) constants) in
         Absyn.Fact(hastype, [tm, Absyn.ConstantTerm(c, [], Errormsg.none)], [], 2, 0,
		    Absyn.TermVarMap([]), Absyn.TypeVarMap([]), [], ref None, [])
		 
(** Similar to {!encode_type} but generates a goal rather than a 
      clause. *)
and encode_type_goal strictness constants metadata t =
  match t with
      Lfabsyn.PiType(id,ty,body,_) ->
        fun tm ->
          let s = Symbol.symbol (string_of_id id) in
	  let flatty = flatten_type ty in
          let v = Absyn.BoundVarTerm(Absyn.NamedBoundVar(Absyn.BoundVar(s, ref None, ref false, ref flatty)),
				     Errormsg.none) in
	  let clause = (encode_type ty) v in
	  Absyn.ImpGoal(Absyn.Definitions([Option.get (Table.find (Symb.symbol hastypeStr)),
					   (ref [clause], ref false, ref 0, ref None)]),
			Absyn.VarInits([]),
			Absyn.TypeVarInits([]),
			(encode_type_as_goal strictness constants metadata body)
			  (Absyn.ApplicationTerm(Absyn.FirstOrderApplication(tm, [v], 0),Errormsg.none)))
    | Lfabsyn.ImpType(l, r, _) ->
        fun tm ->
          let newvar = newVar () in
	  let s = Symbol.symbol newvar in
	  let flatty = flatten_type ty in
          let v = Absyn.BoundVarTerm(Absyn.NamedBoundVar(Absyn.BoundVar(s, ref None, ref false, ref flatty)),
				     Errormsg.none) in
	  let clause = (encode_type ty) v in
	  Absyn.ImpGoal(Absyn.Definitions([Option.get (Table.find (Symb.symbol hastypeStr)),
					   (ref [clause], ref false, ref 0, ref None)]),
			Absyn.VarInits([]),
			Absyn.TypeVarInits([]),
			(encode_type_as_goal strictness constants metadata body)
			  (Absyn.ApplicationTerm(Absyn.FirstOrderApplication(tm, [v], 0),Errormsg.none)))
    | Lfabsyn.AppType(id,tms, _) ->
       fun tm ->
         let c = Option.get (Table.find (Metadata.getLP metadata (string_of+id id)) constants) in
	 let tms = List.map (encode_term constants metadata) tms in
         Absyn.AtomicGoal(hastype,
			  0,
			  0,
			  [tm,
			   Absyn.ApplicationTerm(Absyn.FirstOrderApplication(Absyn.ConstantTerm(c,[],Errormsg.none),
									     tms,
									     0),
						 Errormsg.none)], [])
    | Lfabsyn.IdType(id,_) ->
       fun tm ->
         let c = Option.get (Table.find (Metadata.getLP metadata (string_of+id id)) constants) in
         Absyn.AtomicGoal(hastype, 0,0,[tm,Absyn.ConstantTerm(c,[],Errormsg.none)],[])

let trans_fixity fix assoc =
  match (fix, assoc) with
      (Lfabsyn.Infix,Lfabsyn.Left) -> Absyn.Infixl
    | (Lfabsyn.Infix,Lfabsyn.Right) -> Absyn.Infixr
    | (Lfabsyn.Infix,_) -> Absyn.Infix
    | (Lfabsyn.Prefix,Lfabsyn.Left) -> Absyn.Prefixl
    | (Lfabsyn.Prefix,Lfabsyn.Right) -> Absyn.Prefixr
    | (Lfabsyn.Prefix,_) -> Absyn.Prefix
    | (Lfabsyn.Postfix,Lfabsyn.Left) ->	Absyn.Postfixl
    | (Lfabsyn.Postfix,_) -> Absyn.Postfix
		       

(* set up the name mapping for constants.
     Per typefam:
       create mapping for the type-level constant,
       per object associated with type fam:
         create mapping for the object-level constant *)
let initialize_metadata (Lfsig.Signature(_,types)) =
  let perType symb Lfabsyn.TypeFam(_,_,_,_,_,objs,_) metadata =
    let perObj Lfabsyn.Object(id,_,_,_,_,_) metadata =
      Metadata.new_mapping metadata (Symb.symbol (string_of_id id))
    in
    List.fold_left (fun o m -> perObj (!o) m) (Metadata.new_mapping metadata symb)
  in
  Table.fold perType types Metadata.empty

(* add constants for each type and each object-level constant to the constant table being built. *)
let initialize_constants metadata types=
  let perType symb (Lfabsyn.TypeFam(id,kind,fix,assoc,prec,objs,_)) constants =
    let perObj (Lfabsyn.Object(id, ty,fix,assoc,prec,_)) constants =
      let s = Symb.symbol (string_of_id id) in
      let lpsymb = Option.get (Metadata.getLP metadata s) in
      let objconst =
        Absyn.Constant(lpsymb, trans_fixity fix assoc, prec,
                       ref true, ref false, ref false, ref false, ref false, ref false,
		       ref Some(Absyn.Skeleton(flatten_type ty, ref None, ref false)),
		       ref 0, ref None, ref None, ref Absyn.GlobalConstant,
		       ref 0, Errormsg.none)
      in
      Table.add lpsymb objconst constants
    in
    let lpsymb = Option.get (Metadata.getLP metadata symb) in
    let tyconst =
      Absyn.Constant(lpsymb, trans_fixity fix assoc, prec,
	  	     ref true, ref false, ref false, ref false, ref false, ref false,
		     ref Some(Absyn.Skeleton(flatten_kind kind, ref None, ref false)),
		     ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant,
		     ref 0, Errormsg.none)
    in
    List.fold_left (fun o c -> perObj (!o) c) (!objs) (Table.add lpsymb tyconst constants)
  in
  Symboltable.fold types perType (Table.add (Symbol.symbol hastypeStr)
					    hastype
					    (Table.add (Symbol.symbol istypeStr)
						       istype
						       Table.empty))


(* Process each type level declaration and each corresponding object level declaration. *)
let process strictness metadata constants clauses types =
  let perObj (Lfabsyn.Object(id,typ,_,_,_,_)) clauselst =
    let s = Option.get (Metadata.getLP metadata (Symb.symbol (string_of_id id))) in
    let aterm = Absyn.ConstantTerm(Option.get (Table.find s constants),
				   [], Errormsg.none)in
    let clause = (encode_type typ) aterm in
    List.append clauselst [clause]
  in
  let perType symb (Lfabsyn.TypeDef(id,kind,_,_,_,objs,_)) clauselst =
    let aterm = Absyn.ConstantTerm(Option.get (Table.find (Option.get (Metadata.getLP metadata symb)) constants),
				   [], Errormsg.none)in
    let clause = (encode_kind kind) aterm in
    List.fold_left (fun o c -> perObj (!o) c) (!objs) (List.append clauselst [clause])
  in
  Table.fold perType types []
    
		   
module NaiveTranslation : Translator =
struct
  let translate (Lfsig.Signature(name,types)) =
    let metadata = initialize_metadata types in
    let kinds = Table.add (Symbol.symbol lfobj) lfobj (Table.add (Symbol.symbol lftypeStr) lftype) in
    let constants = initialize_constants metadata types in
    let clauses = Absyn.ClauseBlocks(process false metadata constants types) in
    Absyn.Module("top", [], ref constants, ref kinds, Table.empty,
	         [], [], [], [], [], ref [], [], ref [], ref clauses)
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
      
  let translate (Lfsig.Signature(name, types)) =
    let metadata = initialize_metadata types in
    let kinds = Table.add (Symbol.symbol lfobj) lfobj (Table.add (Symbol.symbol lftypeStr) lftype) in
    let constants = initialize_constants metadata types in
    let clauses = Absyn.ClauseBlocks(process true metadata constants types) in
    let mod =
      Absyn.Module("top", [], ref constants, ref kinds, Table.empty,
	           [], [], [], [], [], ref [], [], ref [], clauses) in
    run_optimizations mod
    
end
