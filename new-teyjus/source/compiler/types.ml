type typemolecule =
  Molecule of (Absyn.atype * Absyn.atype list * bool)

type variablebindings = (Absyn.atype * int) list

(*  The error molecule. *)
let errorMolecule = Molecule(Absyn.ErrorType, [], false)

(*  Molecule Accessors  *)
let getMoleculeType = function Molecule(t, _, _) -> t
let getMoleculeEnvironment = function Molecule(_, env, _) -> env

type unifyresult =
    OccursCheckFailure
  | ClashFailure
  | Success

exception UnifyException of unifyresult

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
* Dereference a type molecule.
**********************************************************************)
let dereferenceMolecule = function Molecule(t, env, b) ->
  Molecule((Absyn.dereferenceType t), env, b)

(**********************************************************************
*string_of_typemolecule:
* Generates a string representation of a type molecule.
**********************************************************************)
let rec string_of_typemolecule = fun mol bindings ->
  let Molecule(t,env,b) = (dereferenceMolecule mol) in

  let string_of_var = fun bindings ->
    try
      let i = List.assoc t bindings in
      ("_" ^ (string_of_int i), bindings)
    with
      Not_found ->
        let i = List.length bindings in
        ("_" ^ (string_of_int i), (t, i)::bindings)
  in
  
  match t with
    Absyn.SkeletonVarType(_) -> (string_of_var bindings)
  | Absyn.TypeVarType(_) -> (string_of_var bindings)
  | Absyn.TypeSetType(_) -> (string_of_var bindings)
  | Absyn.AppType(k,args) ->
      let rec string_of_args = fun args bindings ->
        match args with
          arg::args' ->
            let (arg', bindings') = string_of_typemolecule (Molecule(arg, env, false)) bindings in
            let (args'', bindings'') = (string_of_args args' bindings') in
            (" " ^ arg' ^ args'', bindings'')
        | [] -> ("", bindings)
      in
      let s = (Absyn.getKindName k) in
      let (args', bindings') = (string_of_args args bindings) in
      (s ^ args', bindings')
  | Absyn.ArrowType(l,r) ->
      let (l', bindings') = string_of_typemolecule (Molecule(l, env, false)) bindings in
      let (r', bindings') = string_of_typemolecule (Molecule(r, env, false)) bindings' in
      ("(" ^ l' ^ " -> " ^ r' ^ ")", bindings')
  | Absyn.ErrorType -> ("error", bindings)

(**********************************************************************
*occursCheck:
* Performs an occurs check on a type: given a type variable and a type,
* determines if the variable occurs in the type.
**********************************************************************)
let rec occursCheck = fun var skel bindings ->
  match skel with
    Absyn.ErrorType -> skel
  | Absyn.TypeVarType(_) ->
      if var = skel then
        raise (UnifyException(OccursCheckFailure))
      else
        skel
  | Absyn.SkeletonVarType(i) ->
      let skel' = (Absyn.dereferenceType (getEnvironmentElement bindings i)) in
      occursCheck var skel' []
  | Absyn.AppType(k, args) ->
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
      Absyn.AppType(k, (occurs' args))
      
  | Absyn.ArrowType(l, r) ->
      let l' = (occursCheck var (Absyn.dereferenceType l) bindings) in
      let r' = (occursCheck var (Absyn.dereferenceType r) bindings) in
      Absyn.ArrowType(l', r')

(**********************************************************************
*unbindVariables:
* Goes through a list of variables and changes their references to
* indicate that they are not bound.
**********************************************************************)
let rec unbindVariables = function
  [] -> ()
| Absyn.TypeVarType(t,b)::vs ->
    (t := None;
    unbindVariables vs)
    
(**********************************************************************
*bindVariable:
* Bind a type variable to a particular type.
**********************************************************************)
let bindVariable = fun var mol bindings ->
  let ty = (getMoleculeType mol) in
  try 
    let t = (occursCheck var ty (getMoleculeEnvironment mol)) in
    ((Absyn.getTypeVariableReference ty) := Some ty; var :: bindings)
  with
    UnifyException(OccursCheckFailure) ->
      (unbindVariables bindings; raise (UnifyException(OccursCheckFailure)))

(**********************************************************************
*unify:
* Unifies two type molecules.
**********************************************************************)
let rec unify = fun (tm1 : typemolecule) (tm2 : typemolecule) ->
  
  let rec unify' = fun t1 t2 bindings ->
    (*  Get the actual type and evironment for each molecule. *)
    let t1 = (dereferenceMolecule t1) in
    let skel1 = (getMoleculeType t1) in
    let env1 = (getMoleculeEnvironment t1) in
    let t2 = (dereferenceMolecule t2) in
    let skel2 = (getMoleculeType t2) in
    let env2 = (getMoleculeEnvironment t2) in
    
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
          ((Absyn.getTypeVariableReference skel1) := (Some skel2);
          skel1::bindings)
        else
          bindings
      
      (*  Otherwise just bind.  *)
      else
        (bindVariable skel1 t2 bindings)
    else
    
    match skel2 with
      (*  Type variable: just bind. *)
      Absyn.TypeVarType(_) -> (bindVariable skel2 t1 bindings)
      
      (*  Application type: first match the kind (application), then the head,
          and finally each argument in turn.  *)
    | Absyn.AppType(f, a) ->
        (match skel1 with
          Absyn.AppType(f', a') ->
            (*  Type check two lists of arguments.  Collect the bindings. *)
            let rec checkArgs = fun args1 args2 bindings ->
              match (args1, args2) with
                ([], []) -> bindings
              | (a1::a1s, a2::a2s) ->
                  let bindings' = unify' (Molecule(a1, (getMoleculeEnvironment t1), false)) (Molecule(a2, (getMoleculeEnvironment t2), false)) bindings in
                  (checkArgs a1s a2s bindings)
              | _ -> (Errormsg.impossible Errormsg.none "Types.unify: invalid number of application arguments")
            in
            
            (*  Match the head. *)
            if f <> f' then
              (unbindVariables bindings; raise (UnifyException ClashFailure))
            else
            
            (*  Get application argument types. *)
            let args1 = (Absyn.getTypeArguments skel1) in
            let args2 = (Absyn.getTypeArguments skel2) in
            
            (checkArgs args1 args2 bindings)
            
        | _ -> (unbindVariables bindings; raise (UnifyException ClashFailure)))
    
      (*  Arrow type: first make sure both types are arrow types.
          Then match the left and right sides of the arrow. *)
    | Absyn.ArrowType(l, r) ->
        match skel1 with
          Absyn.ArrowType(l', r') ->
            let bindings' = (unify' (Molecule(l, (getMoleculeEnvironment t1), false)) (Molecule(l', (getMoleculeEnvironment t2), false)) bindings) in
            (unify' (Molecule(r, (getMoleculeEnvironment t1), false)) (Molecule(r', (getMoleculeEnvironment t2), false)) bindings')
        | _ ->
            (unbindVariables bindings;
            raise (UnifyException(ClashFailure)))
    | Absyn.ErrorType -> (Errormsg.impossible (Errormsg.none) "Types.unify: Absyn.ErrorType encountered.")
    | _ ->
        (Errormsg.impossible (Errormsg.none) "Types.unify: Invalid type encountered.")
  in
  try
    (unify' tm1 tm2 [];
    Success)
  with
    UnifyException(t) -> t
(**********************************************************************
*clashError:
* Records a clash error when type checking.
**********************************************************************)
let clashError = fun fargty argty term ->
  let (expected, bindings) = string_of_typemolecule fargty [] in
  let (actual, _) = string_of_typemolecule argty bindings in
  
  Errormsg.error (Absyn.getTermPos term) ("clash in operator and operand types" ^
    (Errormsg.info ("Expected operand type: " ^ expected)) ^
    (Errormsg.info ("Actual operand type: " ^ actual)) ^
    (Errormsg.info ("in expression: " ^ (Absyn.string_of_term term))))

(**********************************************************************
*occursCheckError:
* Records an occurs-check error when type checking.
**********************************************************************)
let occursCheckError = fun fargty argty term ->
  let (operator, bindings) = string_of_typemolecule fargty [] in
  let (operand, _) = string_of_typemolecule argty bindings in
  
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
    try
      (unify farg arg;
      result)
    with
      UnifyException(OccursCheckFailure) -> (occursCheckError farg arg term; errorMolecule)
    | UnifyException(ClashFailure) -> (clashError farg arg term; errorMolecule)
  in
  
  let fty = (dereferenceMolecule fty) in
  let fskel = (getMoleculeType) fty in
  
  (*  Just fail on an error term. *)
  if fskel = Absyn.errorType then
    errorMolecule
  else
    let (operator, _) = string_of_typemolecule fty [] in
    
    (*  Check the function isn't an arrow type and isn't a type variable (and
        so cannot be instantiated to an arrow type).  *)
    if not ((Absyn.isVariableType fskel) || (Absyn.isArrowType fskel)) then
      (Errormsg.error (Absyn.getTermPos term) ("operator is not a function." ^
        (Errormsg.info ("Operator type: " ^ operator)) ^
        (Errormsg.info ("in expression: " ^ (Absyn.string_of_term term) ^ ".")));
      errorMolecule)
    
    (*  Check if the function is a variable... *)
    else if (Absyn.isVariableType fskel) then
      let fargskel = Absyn.TypeVarType(ref None, false) in
      let fargty = Molecule(fargskel, [], false) in
      let targskel = Absyn.TypeVarType(ref None, false) in
      let targty = Molecule(Absyn.TypeVarType(ref None, false), [], false) in
      
      (unify' fargty argty targty)

    (*  Otherwise just check like normal. *)
    else
      let fenv = getMoleculeEnvironment fty in
      let argskels = (Absyn.getTypeArguments fskel) in
      let targskel = (Absyn.getTypeTarget fskel) in
      let fargty = Molecule(List.hd argskels,fenv, false) in
      let numargs = (List.length (Absyn.getTypeArguments fskel)) in
      
      if (numargs) > 0 then
        (unify' fargty argty (Molecule((Absyn.makeArrowType (targskel::argskels)), fenv, false)))
      else
        (unify' fargty argty (Molecule(targskel, fenv, false)))
