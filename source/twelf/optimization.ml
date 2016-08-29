(** Optimizations available for use by the translation. *)

module type Optimization =
sig
  val get : unit -> bool
  val set : bool -> unit
  val run_optimization : (Metadata.metadata * 
                            Absyn.akind Table.SymbolTable.t * 
                            Absyn.aconstant Table.SymbolTable.t * 
                            Absyn.aterm list) -> 
                                                 (Metadata.metadata * 
                                                    Absyn.akind Table.SymbolTable.t * 
                                                    Absyn.aconstant Table.SymbolTable.t * 
                                                    Absyn.aterm list)

  val optimize : Absyn.aterm -> Absyn.aterm

end

module Specialize : Optimization =
struct
  let run = ref false

  let get () = !run

  let set v = run := v
 


  let rec optimize tm =
    (** NOTE: This optimization assumes that the translation and
              any other optimizations ensure that all predicates appear
              as first order applications and are supplied with all
              of their term arguments. *)
    match tm with
        Absyn.AbstractionTerm(abstm, p) ->
          let abstm' =
            (match abstm with
                 Absyn.NestedAbstraction(ty,tm) ->
                   Absyn.NestedAbstraction(ty,optimize tm)
               | Absyn.UNestedAbstraction(tys,i,tm) ->
                   Absyn.UNestedAbstraction(tys,i,optimize tm))
          in Absyn.AbstractionTerm(abstm', p)
      | Absyn.ApplicationTerm(Absyn.FirstOrderApplication(Absyn.ConstantTerm(c,_,_),[lfterm;lftype],_),_) 
          when Absyn.getConstantName c = "hastype" ->
          (match lftype with
               Absyn.ApplicationTerm(Absyn.FirstOrderApplication(targetty, tyargs,i),p) ->
                 Absyn.ApplicationTerm(Absyn.FirstOrderApplication(targetty, (lfterm :: tyargs), i + 1),p)
             | Absyn.ConstantTerm(_,_,p) ->
                 Absyn.ApplicationTerm(Absyn.FirstOrderApplication(lftype, [lfterm], 1),p))
      | Absyn.ApplicationTerm(apptm,p) ->
          let apptm' =
            (match apptm with
                 Absyn.FirstOrderApplication(head,tms,i) ->
                   Absyn.FirstOrderApplication(optimize head, List.map optimize tms, i)
               | Absyn.CurriedApplication(t1,t2) -> 
                   Absyn.CurriedApplication(optimize t1, optimize t2))
          in Absyn.ApplicationTerm(apptm',p)
      | _ -> tm

  let run_optimization (metadata, kinds, constants, terms) = 
    (* for each constant that is an lf-type, modify the type.
       ( A -> lftype  ===> lf-obj -> A -> o )
       then go through each clause with 'hastype', modify it, and 
       move it to the correct predicate *)
    match ((Table.find (Symbol.symbol "lf_type") kinds), (Table.find (Symbol.symbol "lf_object") kinds)) with
        (Some(lftype), Some(lfobj)) ->
          let rec isLFtype ty =
            match ty with
                Absyn.ArrowType(t1,t2) -> isLFtype t2
              | Absyn.ApplicationType(lftype, _) -> true
              | _ -> false
          in
          let changeType symb (Absyn.Constant(s,x1,x2,x3,x4,x5,x6,x7,x8,skel,x9,x10,x11,x12,x13,x14,x15)) constants =
            (match !skel with
                 Some(Absyn.Skeleton(ty,y1,y2)) ->
                   if (isLFtype ty)
                   then
                     let rec alterTarget t =
                       match t with
                            Absyn.ArrowType(lastarg,Absyn.ApplicationType(_,_)) ->
                              Absyn.ArrowType(lastarg,Absyn.ApplicationType(Pervasive.kbool,[]))
                          | Absyn.ArrowType(t1, t2) -> 
                              Absyn.ArrowType(t1, alterTarget t2)
                          | _ ->
                              Errormsg.error Errormsg.none 
                                             ("Ill-formed type for LP constant: '" ^ (Symbol.printName s) ^ 
                                                 " : " ^ (Absyn.string_of_type ty) ^ "'");
                              Absyn.ErrorType
                     in
                     let ty' = Absyn.ArrowType(Absyn.ApplicationType(lfobj,[]), alterTarget ty) in
                     let constants' = 
                       Table.add symb 
                                 (Absyn.Constant(s,x1,x2,x3,x4,x5,x6,x7,x8,ref (Some(Absyn.Skeleton(ty',y1,y2))),x9,x10,x11,x12,x13,x14,x15)) 
                                 constants in
                     constants'
                   else
                     constants
               | None ->
                   Errormsg.error Errormsg.none ("No type provided for LP constant '" ^ (Symbol.printName s) ^ "'");
                   constants)
          in
          let constants' = Table.fold changeType constants constants in
          let terms' = List.map optimize terms in
          (metadata, kinds, constants', terms')
      | (Some(lftype),None) ->
          Errormsg.error Errormsg.none "Could not find kind 'lf_object' in kind table.";
          (metadata, kinds, constants, terms)
      | (None,Some(lfobj)) ->
          Errormsg.error Errormsg.none "Could not find kind 'lf_type' in kind table.";
          (metadata, kinds, constants, terms)
      | (None,None) ->
          Errormsg.error Errormsg.none "Could not find kinds 'lf_object' or 'lf_type' in kind table.";
          (metadata, kinds, constants, terms)

end

module Swap : Optimization =
struct
  let run = ref false

  let get () = !run

  let set v = run := v

  let rec predicate ty =
    match ty with
        Absyn.ArrowType(t1,t2) -> predicate t2
      | Absyn.ApplicationType(k, _) 
          when k = Pervasive.kbool -> true
      | _ -> false

  let swapType symb (Absyn.Constant(s,x1,x2,x3,x4,x5,x6,x7,x8,skel,x9,x10,x11,x12,x13,x14,x15)) constants =
    match !skel with
        Some(Absyn.Skeleton(ty,y1,y2)) ->
          if (predicate ty)
          then
            let rec swapType firstarg t =
              match t with
                   Absyn.ArrowType(lastarg,(Absyn.ApplicationType(k,l) as o))
                     when k = Pervasive.kbool ->
                     Absyn.ArrowType(lastarg,Absyn.ArrowType(firstarg, o))
                 | Absyn.ArrowType(t1, t2) -> 
                     Absyn.ArrowType(t1, swapType firstarg t2)
                 | _ ->
                     Errormsg.error Errormsg.none 
                                    ("Ill-formed type for LP constant: '" ^ (Symbol.printName s) ^ 
                                        " : " ^ (Absyn.string_of_type ty) ^ "'");
                     Absyn.ErrorType
            in
            let ty' = 
              (match ty with
                   Absyn.ApplicationType(k,_) 
                     when k = Pervasive.kbool -> ty
                 | Absyn.ArrowType(firstarg,t2) -> 
                     swapType firstarg t2) in
            let constants' = 
              Table.add symb 
                        (Absyn.Constant(s,x1,x2,x3,x4,x5,x6,x7,x8,ref (Some(Absyn.Skeleton(ty',y1,y2))),x9,x10,x11,x12,x13,x14,x15)) 
                        constants in
            constants'
          else
            constants
      | None ->
          Errormsg.error Errormsg.none ("No type provided for LP constant '" ^ (Symbol.printName s) ^ "'");
          constants
    
  let rec optimize tm =
    match tm with
        Absyn.AbstractionTerm(abs,p) ->
          let abs' =
            (match abs with
                 Absyn.NestedAbstraction(tysymb,body) ->
                   Absyn.NestedAbstraction(tysymb, optimize body)
               | Absyn.UNestedAbstraction(tysymbs,i,body) ->
                   Absyn.UNestedAbstraction(tysymbs,i, optimize body))
          in
          Absyn.AbstractionTerm(abs',p)
      | Absyn.ApplicationTerm(apptm, p) ->
        (*** NOTE: at this point we depend on the translation (and any other optimizations) 
                   to maintain that first order applications are always used for predicates
                   and that they always appear with a full list of arguments. *)
          let apptm' =
            (match apptm with
                 Absyn.FirstOrderApplication(Absyn.ConstantTerm(c,_,_)as head,tms,i) ->
                   let tms' = 
                     List.map optimize
                              (if (predicate (Absyn.getSkeletonType (Absyn.getConstantSkeletonValue c))) 
                               then
                                 List.append (List.tl tms) [List.hd tms]
                               else
                                 tms) 
                   in
                   Absyn.FirstOrderApplication(head,tms',i)
               | Absyn.FirstOrderApplication(head, tms, i) ->
                   Absyn.FirstOrderApplication(optimize head, List.map optimize tms, i)
               | Absyn.CurriedApplication(t1,t2) -> 
                   Absyn.CurriedApplication(optimize t1, optimize t2)) 
          in
          Absyn.ApplicationTerm(apptm',p)
      | _ -> tm

  let run_optimization (metadata, kinds, constants, terms) =
    (* for each predicate in the constant table alter its type
       so that the first argument becomes the final argument. *)
    (* then look for any uses of the predicate and move first 
       argument to last position. *)
    
    let constants' = Table.fold swapType constants constants in
    let terms' = List.map optimize terms in
    (metadata, kinds, constants', terms')
end
