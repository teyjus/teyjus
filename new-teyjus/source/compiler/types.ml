type typemolecule =
  Molecule of (Absyn.atype * Absyn.atype list)

type typeandenvironment =
  TypeAndEnvironment of (Absyn.atype * Absyn.atype list)

type variablebindings = (Absyn.atype * int) list

(*  The error molecule. *)
let errorMolecule = Molecule(Absyn.ErrorType, [])

(*  Molecule Accessors  *)
let getMoleculeType = function Molecule(t, _) -> t
let getMoleculeEnvironment = function Molecule(_, env) -> env

type unifyresult =
    OccursCheckFailure
  | ClashFailure
  | Success

exception UnifyException of unifyresult

(**********************************************************************
*constantType:
* Constructs the type of the given constant, building an appropriate
* environment.
**********************************************************************)
let makeConstantMolecule = fun constant ->
  let env = ref [] in
  let rec instance ty =
    let ty' = Absyn.dereferenceType ty in
    match ty' with
      Absyn.TypeSetType(def, l) ->
        let l' = ref (List.map instance (!l)) in
        let t = Absyn.TypeSetType(def, l') in
        (env := t :: (!env);
        t)
    | Absyn.ApplicationType(k, tl) ->
        Absyn.ApplicationType(k, (List.map instance tl))
    | Absyn.ArrowType(l,r) -> Absyn.ArrowType(instance l, instance r)
    | Absyn.SkeletonVarType(_) -> ty'
    | Absyn.TypeVarType(_) -> ty'
    | Absyn.ErrorType -> ty'
  in
  
  
  let envsize = Absyn.getConstantTypeEnvSize constant in
  let skel = Absyn.getConstantSkeleton constant in
  
  if Option.isSome skel then
    let ty = Absyn.getSkeletonType (Option.get skel) in
    let ty' = instance ty in
    if List.length (!env) > 0 then
      Molecule(ty', !env)
    else
      Molecule(instance ty, Absyn.makeTypeEnvironment envsize)
  else
    Errormsg.impossible (Absyn.getConstantPos constant)
      "Types.makeConstantMolecule: constant has no skeleton"

(**********************************************************************
*makeKindMolecule:
* Constructs a sort from an absyn kind.
**********************************************************************)
let makeKindMolecule kind =
  Molecule(Absyn.ApplicationType(kind, []), [])

(**********************************************************************
*getEnvironmentElement:
* Gets the ith element of an environment.  i refers to the index
* specified by a skeleton variable (See Absyn.SkeletonVarType).
**********************************************************************)
let rec getEnvironmentElement = fun env i ->
  match env with
    e::es ->
      if i = 0 then
        e
      else
        (getEnvironmentElement es (i - 1))
  | [] -> (Errormsg.impossible Errormsg.none "Types.getEnvironmentElement: invalid element index")

(**********************************************************************
*dereferenceMolecule:
* Dereference a type molecule.  Seems like this is split from the
* dereference in absyn in a strange way.
**********************************************************************)
let rec dereferenceMolecule = function Molecule(t, env) ->
  let rec get = fun i env ->
    match env with
      [] ->
        (Errormsg.impossible Errormsg.none "Types.dereferenceMolecule: invalid skeleton index.")
    | e::es ->
        if i = 0 then
          e
        else
          get (i - 1) es
  in
  
  (*  Follow references *)
  let t' = (Absyn.dereferenceType t) in
  
  (*  Get appropriate index if it is a skeleton variable. *)
  if Absyn.isSkeletonVariableType t' then
    dereferenceMolecule (Molecule(get (Absyn.getSkeletonVariableIndex t') env, env))
  else
    Molecule(t', env)

(**********************************************************************
*string_of_typemolecule:
* Generates a string representation of a type molecule.
**********************************************************************)
let rec string_of_typemolecule = fun mol ->
  let (s, _) = string_of_typemolecule' mol [] in
  s

and string_of_typemolecule' = fun mol bindings ->
  let Molecule(t,env) = (dereferenceMolecule mol) in

  let rec string_of_skelvar = fun i ->
    let rec get' = fun i env ->
      match env with
        e::es ->
          if i = 0 then
            string_of_type (Absyn.dereferenceType e)
          else
            get' (i - 1) es
      | [] -> Errormsg.impossible Errormsg.none "Types.string_of_typemolecule': invalid skeleton index."
    in
    get' i env
  
  and string_of_var = fun bindings ->
    try
      let i = List.assoc t bindings in
      ("_" ^ (string_of_int i), bindings)
    with
      Not_found ->
        let i = List.length bindings in
        ("_" ^ (string_of_int i), (t, i)::bindings)
  
  and string_of_typeset = fun ts ->
    let get' = fun d tlist ->
      match tlist with
        [t] -> (string_of_type t)
      | _ -> (string_of_type d)
    in
    match ts with
      Absyn.TypeSetType(d, t) -> (get' d (!t))
    | _ -> (Errormsg.impossible Errormsg.none "Types.string_of_typeset: invalid typeset")
  
  and string_of_type = fun t ->
    match t with
      Absyn.SkeletonVarType(i) -> (string_of_skelvar !i)
    | Absyn.TypeVarType(_) -> (string_of_var bindings)
    | Absyn.TypeSetType(_) -> (string_of_typeset t)
    | Absyn.ApplicationType(k,args) ->
        let rec string_of_args = fun args bindings ->
          match args with
            arg::args' ->
              let (arg', bindings') = string_of_typemolecule' (Molecule(arg, env)) bindings in
              let (args'', bindings'') = (string_of_args args' bindings') in
              (" " ^ arg' ^ args'', bindings'')
          | [] -> ("", bindings)
        in
        let s = (Absyn.getKindName k) in
        let (args', bindings') = (string_of_args args bindings) in
        if args' = "" then
          (s, bindings')
        else
          ("(" ^ s ^ args' ^ ")", bindings')
    | Absyn.ArrowType(l,r) ->
        let (l', bindings') = string_of_typemolecule' (Molecule(l, env)) bindings in
        let (r', bindings') = string_of_typemolecule' (Molecule(r, env)) bindings' in
        ("(" ^ l' ^ " -> " ^ r' ^ ")", bindings')
    | Absyn.ErrorType -> ("error", bindings)
  in
  string_of_type t

(**********************************************************************
*skeletonizeType:
* Converts a type with free variables into a type molecule with
* skeleton variables.
**********************************************************************)
let rec skeletonizeType ty =
  (********************************************************************
  *getMoleculeListTypes;
  ********************************************************************)
  let rec getMoleculeListTypes args =
    match args with
      [] -> []
    | a::aa ->
        (getMoleculeType a)::(getMoleculeListTypes aa)
  in

  (********************************************************************
  *replaceType:
  * Finds the given type and converts it to a skeleton type.
  ********************************************************************)
  let rec replaceType ty env index =
    match env with
      [] -> None
    | t::ts ->
        if t = ty then
          Some(Absyn.SkeletonVarType(ref index))
        else
          replaceType ty ts (index - 1)
  in
  
  (********************************************************************
  *skeletonizeArgs:
  * Skeletonizes a list of types in turn.
  ********************************************************************)
  let rec skeletonizeArgs args env index =
    match args with
      [] -> ([], false, index)
    | a::aa ->
        let (a', newvars, index') = skeletonize a env index in
        let env' = getMoleculeEnvironment a' in
        
        let (args', newvars', index'') = skeletonizeArgs aa env' index' in
        (a'::args', newvars || newvars', index'')
  
  and skeletonize ty env index =
    let ty' = Absyn.dereferenceType ty in
    match ty' with
      Absyn.TypeVarType(r) ->
        (match !r with
          Absyn.BindableTypeVar(_) ->
            let ty'' = replaceType ty' env (index - 1) in
            if Option.isSome ty'' then
              (Molecule(Option.get ty'', env), false, index)
            else
              let index' = index + 1 in 
              (Molecule(Absyn.SkeletonVarType(ref index'), ty'::env), true, index')
        | _ -> Errormsg.impossible Errormsg.none "Types.skeletonizeType: invalid type variable")
    | Absyn.ApplicationType(k, args) ->
        let (argmols, newvars, index') = skeletonizeArgs args env index in
        let args' = getMoleculeListTypes argmols in
        (Molecule(Absyn.ApplicationType(k, args'), env), newvars, index')
    | Absyn.ArrowType(_) ->
        let targty = Absyn.getArrowTypeTarget ty' in
        let argtys = Absyn.getArrowTypeArguments ty' in
               
        let (targmol, newvars, index') = skeletonize targty env index in
        let targty' = getMoleculeType targmol in
        let env' = getMoleculeEnvironment targmol in
        
        let (argmols, newvars', index'') = skeletonizeArgs argtys env' index' in
        let argtys' = getMoleculeListTypes argmols in
        (Molecule(Absyn.makeArrowType targty' argtys', env'), newvars || newvars', index'')
    | Absyn.TypeSetType(_) ->
        (Errormsg.impossible Errormsg.none "Types.skeletonizeType: invalid type set")
    | _ -> (Molecule(ty', env), false, index)
  in

  let (mol, newvars, index) = skeletonize ty [] 0 in
  
  if index > Pervasiveutils.maxSkeletonIndex then
    (Errormsg.error Errormsg.none "unable to skeletonize type: type contains too many free variables";
    errorMolecule)
  else
    Molecule((getMoleculeType mol), List.rev (getMoleculeEnvironment mol))


(**********************************************************************
*occursCheck:
* Performs an occurs check on a type: given a type variable and a type,
* determines if the variable occurs in the type.
**********************************************************************)
and occursCheck = fun var skel bindings ->
  match skel with
    Absyn.ErrorType -> skel
  | Absyn.TypeVarType(_) ->
      if var = skel then
        raise (UnifyException(OccursCheckFailure))
      else
        skel
  | Absyn.SkeletonVarType(i) ->
      let skel' = (Absyn.dereferenceType (getEnvironmentElement bindings !i)) in
      occursCheck var skel' []
  | Absyn.ApplicationType(k, args) ->
      (****************************************************************
      *occurs':
      * Checks each argument and builds a list of the result.
      ****************************************************************)
      let rec occurs' = function
        [] -> []
      | arg::args ->
          let arg' = (occursCheck var (Absyn.dereferenceType arg) bindings) in
          arg' :: (occurs' args)
      in
      Absyn.ApplicationType(k, (occurs' args))
      
  | Absyn.ArrowType(l, r) ->
      let l' = (occursCheck var (Absyn.dereferenceType l) bindings) in
      let r' = (occursCheck var (Absyn.dereferenceType r) bindings) in
      Absyn.ArrowType(l', r')
  | Absyn.TypeSetType(_) -> skel
      
(**********************************************************************
*unbindVariables:
* Goes through a list of variables and changes their references to
* indicate that they are not bound.
**********************************************************************)
let rec unbindVariables = function
  [] -> ()
| Absyn.TypeVarType(r)::vs ->
    (match !r with
      Absyn.BindableTypeVar(tr) ->
        (tr := None;
        unbindVariables vs)
    | _ -> Errormsg.impossible Errormsg.none "Types.unbindVariables: invalid type variable")
| v::vs ->
    (Errormsg.impossible Errormsg.none
      "Types.unbindVariables: non-variable type encountered.")
    
(**********************************************************************
*bindVariable:
* Bind a type variable to a particular type.
**********************************************************************)
let bindVariable = fun var mol bindings ->
  let ty = (getMoleculeType mol) in
  try 
    let t = (occursCheck var ty (getMoleculeEnvironment mol)) in
    (Absyn.getTypeVariableReference var := Some(t);
    var :: bindings)
  with
    UnifyException(OccursCheckFailure) ->
      (unbindVariables bindings; raise (UnifyException(OccursCheckFailure)))

(**********************************************************************
*unify:
* Unifies two type molecules.
**********************************************************************)
let rec unify = fun (tm1 : typemolecule) (tm2 : typemolecule) ->
  let rec in_set = fun s set ->
    match set with
      s'::set' -> if s = s' then true else in_set s set'
    | [] -> false
  in
  
  let intersection = fun s1 s2 ->
    let rec inter_element = fun s slist result ->
      match slist with
        s'::ss ->
          if s = s' then
            s :: result
          else
            inter_element s ss result
      | [] -> result
    in
    
    let rec inter_list = fun s1 s2 result ->
      match s1 with
        s::ss ->  inter_list ss s2 (inter_element s s2 result)
      | [] -> result
    in
    
    (inter_list s1 s2 [])
  in
    
  let rec unify' = fun t1 t2 bindings ->
    (*  Get the actual type and evironment for each molecule. *)
    let t1 = (dereferenceMolecule t1) in
    let skel1 = (getMoleculeType t1) in
    let t2 = (dereferenceMolecule t2) in
    let skel2 = (getMoleculeType t2) in
    
    (*  If either is an error, bail.  *)
    if (t1 = errorMolecule) || (t2 = errorMolecule) then
      bindings
    else
    
    (*  Check for type variables and bind.  *)
    if (Absyn.isVariableType skel1) then
      (*  If both are variables, bind one to the other. *)
      if (Absyn.isVariableType skel2) then
        (*  Only bind if the variables are not equal. *)
        if (skel1 <> skel2) then
          ((Absyn.getTypeVariableReference skel1) := (Some(skel2));
          skel1::bindings)
        else
          bindings
      
      (*  Otherwise just bind.  *)
      else
        (bindVariable skel1 t2 bindings)
    else
    
    if Absyn.isTypeSetType skel1 then
      let set1 = Absyn.getTypeSetSet skel1 in
      (*  If both are type sets, then set each set to the intersection of
          the sets.  If the intersection is empty, clash error. *)
      if Absyn.isTypeSetType skel2 then
        let set2 = Absyn.getTypeSetSet skel2 in
        let inter = intersection (!set1) (!set2) in
        
        if (List.length inter) = 0 then
          (Errormsg.log Errormsg.none "Types.unify: empty intersection.";
          unbindVariables bindings;
          raise (UnifyException ClashFailure))
        else
          (set1 := inter;
          set2 := inter;
          bindings)
      else

      (*  Check if the other type is in the list.  If it is, then
          just remove all but that item from the list.  If it isn't,
          then clash error. *)
        if (in_set skel2 (!set1)) then
          (set1 := [skel2];
          bindings)
        else
          (Errormsg.log Errormsg.none "Types.unify: type not in set.";
          unbindVariables bindings;
          raise (UnifyException ClashFailure))
    else
    
    match skel2 with
      (*  Type variable: just bind. *)
      Absyn.TypeVarType(_) -> (bindVariable skel2 t1 bindings)
      
      (*  Type set: check if skel1 is in the type set; if so,
          set the type set to only have skel1 in it.  Otherwise,
          clash error.  *)
    | Absyn.TypeSetType(default, set2) ->
        if (in_set skel1 (!set2)) then
          (set2 := [skel1];
          bindings)
        else
          (Errormsg.log Errormsg.none "Types.unify: type not in set.";
          unbindVariables bindings;
          raise (UnifyException ClashFailure))

      (*  Application type: first match the kind (application), then the head,
          and finally each argument in turn.  *)
    | Absyn.ApplicationType(f, a) ->
        (match skel1 with
          Absyn.ApplicationType(f', a') ->
            (*  Type check two lists of arguments.  Collect the bindings. *)
            let rec checkArgs = fun args1 args2 bindings ->
              match (args1, args2) with
                ([], []) -> bindings
              | (a1::a1s, a2::a2s) ->
                  let bindings' = unify' (Molecule(a1, (getMoleculeEnvironment t1))) (Molecule(a2, (getMoleculeEnvironment t2))) bindings in
                  (checkArgs a1s a2s bindings')
              | _ -> (Errormsg.impossible Errormsg.none "Types.unify: invalid number of application arguments")
            in
            
            (*  Match the head. *)
            if f <> f' then
              (Errormsg.log Errormsg.none ("Types.unify: unable to unify " ^ (Absyn.getKindName f) ^ " and " ^ (Absyn.getKindName f'));
              unbindVariables bindings;
              raise (UnifyException ClashFailure))
            else
            
            (*  Get application argument types. *)
            let args1 = a' in
            let args2 = a in
            
            (checkArgs args1 args2 bindings)
            
        | _ ->
            (Errormsg.log Errormsg.none ("Types.unify: error checking application: invalid type: " ^ (Absyn.string_of_type skel1));
            unbindVariables bindings;
            raise (UnifyException ClashFailure)))
    
      (*  Arrow type: first make sure both types are arrow types.
          Then match the left and right sides of the arrow. *)
    | Absyn.ArrowType(l, r) ->
        (match skel1 with
          Absyn.ArrowType(l', r') ->
            let bindings' = (unify' (Molecule(l, (getMoleculeEnvironment t1))) (Molecule(l', (getMoleculeEnvironment t2))) bindings) in
            (unify' (Molecule(r, (getMoleculeEnvironment t1))) (Molecule(r', (getMoleculeEnvironment t2))) bindings')
        | _ ->
            (Errormsg.log Errormsg.none "Types.unify: error checking arrow: invalid type.";
            unbindVariables bindings;
            raise (UnifyException(ClashFailure))))
    | Absyn.ErrorType -> (Errormsg.impossible (Errormsg.none) "Types.unify: Absyn.ErrorType encountered.")
    | _ ->
        (Errormsg.impossible (Errormsg.none) "Types.unify: Invalid type encountered.")
  in
  try
    (*  Only success or failure matters.  Discard the result. *)
    let _ = unify' tm1 tm2 [] in
    Success
  with
    UnifyException(t) -> t
(**********************************************************************
*clashError:
* Records a clash error when type checking.
**********************************************************************)
let clashError = fun fargty argty term ->
  let (expected, bindings) = string_of_typemolecule' fargty [] in
  let (actual, _) = string_of_typemolecule' argty bindings in
  
  Errormsg.error (Absyn.getTermPos term) ("clash in operator and operand types" ^
    (Errormsg.info ("Expected operand type: " ^ expected)) ^
    (Errormsg.info ("Actual operand type: " ^ actual)) ^
    (Errormsg.info ("in expression: " ^ (Absyn.string_of_term term))))

(**********************************************************************
*occursCheckError:
* Records an occurs-check error when type checking.
**********************************************************************)
let occursCheckError = fun fargty argty term ->
  let (operator, bindings) = string_of_typemolecule' fargty [] in
  let (operand, _) = string_of_typemolecule' argty bindings in
  
  Errormsg.error (Absyn.getTermPos term) ("occurs-check failure" ^
    (Errormsg.info ("Operator type: " ^ operator)) ^
    (Errormsg.info ("Operand type: " ^ operand)) ^
    (Errormsg.info ("in expression: " ^ (Absyn.string_of_term term))))

(**********************************************************************
*checkApply:
* Check an application between a function and an argument.
**********************************************************************)
let checkApply = fun fty argty term ->
  (********************************************************************
  *unify':
  * Attempt to unify the first argument of an application with the first
  * expected argument.  Calls necessary error functions.
  ********************************************************************)
  let unify' = fun farg arg result ->
    match (unify farg arg) with
      Success -> result
    | OccursCheckFailure -> (occursCheckError farg arg term; errorMolecule)
    | ClashFailure -> (clashError farg arg term; errorMolecule)
  in
  
  let fty = dereferenceMolecule fty in
  let fskel = getMoleculeType fty in
  
  (*  Just fail on an error term. *)
  if fskel = Absyn.errorType then
    errorMolecule
  else
    (*  Check the function isn't an arrow type and isn't a type variable (and
        so cannot be instantiated to an arrow type).  *)
    if not ((Absyn.isVariableType fskel) || (Absyn.isArrowType fskel)) then
      let (operator, _) = string_of_typemolecule' fty [] in
      (Errormsg.error (Absyn.getTermPos term) ("operator is not a function" ^
        (Errormsg.info ("Operator type: " ^ operator)) ^
        (Errormsg.info ("in expression: " ^ (Absyn.string_of_term term) ^ ".")));
      errorMolecule)
    
    (*  Check if the function is a variable... *)
    else if (Absyn.isVariableType fskel) then
      let fargskel = Absyn.makeTypeVariable () in
      let fargty = Molecule(fargskel, []) in
      let targty = Molecule(Absyn.makeTypeVariable (), []) in
      
      (unify' fargty argty targty)

    (*  Otherwise just check like normal. Function is an arrow type.*)
    else
      let fenv = getMoleculeEnvironment fty in
      let argskels = (Absyn.getArrowTypeArguments fskel) in
      let targskel = (Absyn.getArrowTypeTarget fskel) in
      let fargty = Molecule(List.hd argskels, fenv) in
      let numargs = (List.length argskels) - 1 in
      
      if (numargs) > 0 then
        (unify' fargty argty (Molecule((Absyn.makeArrowType targskel (List.tl argskels)), fenv)))
      else
        (unify' fargty argty (Molecule(targskel, fenv)))


(* Produce a list of type variables that appear free in the given type *)
(* expression and are new to the given list of type variables.         *)
let rec freeTypeVars tyexp tyfvs =
  match tyexp with
	Absyn.TypeVarType(varInfo) ->
	  (match (!varInfo) with
		Absyn.FreeTypeVar(_, _) ->
		  (* whether the variable appears in the given var list *)
		  let rec isNewTyFv tyfvs =
			match tyfvs with
			  [] -> true
			| (tyfv :: rest) -> 
				if (tyfv == tyexp) then false
				else isNewTyFv rest
		  in
		  if (isNewTyFv tyfvs) then (tyexp::tyfvs)
		  else tyfvs
	  | Absyn.BindableTypeVar(target) -> 
		  (match (!target) with
			Some(targetType) -> freeTypeVars targetType tyfvs
		  | None -> 
			  Errormsg.impossible Errormsg.none
				"freeTypeVars: invalid type expression"))
  | Absyn.ArrowType(arg, target) ->
	  freeTypeVars target (freeTypeVars arg tyfvs)
  | Absyn.ApplicationType(kind, args) ->
	  let rec freeTypeVarsInTyList tyexps tyfvs =
		match tyexps with
		  [] -> tyfvs
		| (tyexp :: rest) ->
			freeTypeVarsInTyList rest (freeTypeVars tyexp tyfvs)
	  in
	  freeTypeVarsInTyList args tyfvs
  | _ -> 
	  Errormsg.impossible Errormsg.none 
		"freeTypeVars: invalid type expression"
			
