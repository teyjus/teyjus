type typemolecule =
  Molecule of (Absyn.atype * Absyn.atype list * bool)

let getMoleculeType = function Molecule(t, _, _) -> t
let getMoleculeEnvironment = function Molecule(_, env, _) -> env

let errorMolecule = Molecule(Absyn.ErrorType, [], false)

type unifyresult =
    OccursCheckFailure
  | ClashFailure
  | Success

exception UnifyException of unifyresult


(**********************************************************************
*dereferenceMolecule:
* Dereference a type molecule.
**********************************************************************)
let dereferenceMolecule = function Molecule(t, env, b) ->
  Molecule((Absyn.dereferenceType t), env, b)

(**********************************************************************
*unify:
* Unifies two type molecules.
**********************************************************************)
let unify = fun tm1 tm2 ->
  
  let unify' = fun t1 t2 bindings ->
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
    if (Absyn.isTypeVariable skel1) then
      (*  If both are variables, bind one to the other. *)
      if (Absyn.isTypeVariable skel2) then
        (*  Only bind if the variables are not equal. *)
        if (skel1 <> skel2) then
          ((Absyn.getTypeVariableReference skel1) := (Some skel2);
          skel1::bindings)
        else
          bindings
      
      (*  Otherwise just bind.  *)
      else
        (bindVariable skel1 ty2 bindings)
    else
    
    match skel2 with
      (*  Type variable: just bind. *)
      Absyn.TypeVarType(_) -> (bindVariable skel2 ty1 bindings)
      
      (*  Application type: first match the kind (application), then the head,
          and finally each argument in turn.  *)
    | Absyn.AppType(f, a) ->
        (match skel1 with
          Absyn.AppType(f', a') ->
            (*  Type check two lists of arguments.  Collect the bindings. *)
            let checkArgs = fun args1 args2 bindings ->
              match (args1, args2) with
                ([], []) -> bindings
              | (a1::a1s, a2::a2s) ->
                  let bindings' = unify' a1 a2 bindings in
                  (checkArgs a1s a2s)
              | _ -> (Errormsg.impossible Errormsg.none "Types.unify: invalid number of application arguments")
            in
            
            (*  Match the head. *)
            if f <> f' then
              (unbindVariables bindings; raise UnifyException(ClashError))
            else
            
            (*  Get application argument types. *)
            let args1 = (Absyn.getAppTypeArguments skel1) in
            let args2 = (Absyn.getAppTypeArguments skel2) in
            
            (checkArgs args1 args2 bindings)
            
        | _ -> (unbindVariables bindings; raise UnifyException(ClashError)))
    
      (*  Arrow type: first make sure both types are arrow types.
          Then match the left and right sides of the arrow. *)
    | Absyn.ArrowType(l, r) ->
        match skel1 with
          Absyn.ArrowType(l', r') ->
            let bindings' = (unify l l' bindings) in
            (unify r r' bindings')
        | _ ->
            (unbindVariables bindings;
            raise UnifyException(ClashError))
    | Absyn.ErrorType -> (Errormsg.impossible (Errormsg.none) "Types.unify: Absyn.ErrorType encountered.")
    | _ ->
        (Errormsg.impossible (Errormsg.none) "Types.unify: Invalid type encountered.")
  in
  (unify' tm1 tm2 [])

(**********************************************************************
*bindVariable:
* Bind a type variable to a particular type.
**********************************************************************)
let bindVariable = fun var mol bindings ->
  let ty = (getTypeMoleculeType mol) in
  match (occursCheck var ty (getTypeMoleculeEnvironment mol)) with
    Some t -> ((Absyn.getTypeVariableReference ty) := ty; var :: bindings)
  | None -> (unbindVariables bindings; raise UnifyException(OccursCheckFailure))

(**********************************************************************
*clashError:
* Records a clash error when type checking.
**********************************************************************)
let clashError = fun fargty argty term ->
  Errormsg.error (Absyn.getTermPos term) ("clash in operator and operand types" ^
    (Errormsg.info ("Expected operand type: " ^ (typemolecule_of_string fargty))) ^
    (Errormsg.info ("Actual operand type: " ^ (typemolecule_of_string argty))) ^
    (Errormsg.info ("in expression: " ^ (Absyn.term_of_string term))))

(**********************************************************************
*occursCheckError:
* Records an occurs-check error when type checking.
**********************************************************************)
let occursCheckError = fun fargty argty term ->
  Errormsg.error (Absyn.getTermPos term) ("occurs-check failure" ^
    (Errormsg.info ("Operator type: " ^ (typemolecule_of_string fargty))) ^
    (Errormsg.info ("Operand type: " ^ (typemolecule_of_string argty))) ^
    (Errormsg.info ("in expression: " ^ (Absyn.term_of_string term))))

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
      OccursCheckFailure -> (occursCheckError farg arg term; errorTypeMolecule)
    | ClashFailure -> (clashError farg arg term; errorTypeMolecule)
    | Success -> (result)
  in
  
  let fty = dereferenceTypeMolecule fty in
  let fskel = getTypeMoleculeType fty in
  
  (*  Just fail on an error term. *)
  if fskel = Absyn.errorType then
    errorTypeMolecule
  else
    (*  Check the function isn't an arrow type and isn't a type variable (and
        so cannot be instantiated to an arrow type).  *)
    if not ((Absyn.isTypeVariable fskel) || (Absyn.isArrorType fskel)) then
      (Errormsg.error (Absyn.getTermPos term) ("operator is not a function." ^
        (Errormsg.info ("Operator type: " ^ (typemolecule_of_string fty))) ^
        (Errormsg.info ("in expression: " ^ (Absyn.aterm_of_string term) ^ ".")));
      errorTypeMolecule)
    
    (*  Check if the function is a variable... *)
    else if (Absyn.isTypeVariable fskel) then
      let fargskel = Absyn.TypeVarType(None, false) in
      let fargty = TypeMolecule(fargskel, []) in
      let targskel = Absyn.TypeVarType(None, false) in
      let targty = TypeMolecule(Absyn.TypeVarType(None, false), []) in
      
      (unify' fargty argy targty)

    (*  Otherwise just check like normal. *)
    else
      let fenv = getTypeMoleculeEnvironment fty in
      let argskels = (Absyn.getArrowTypeArguments fskel) in
      let targskel = (Absyn.getArrowTypeTarget fskel) in
      let fargty = TypeMolecule(List.hd argskels,fenv) in
      let numargs = (Absyn.getArrowTermNumArguments fskel) in
      
      if (numargs) > 0 then
        (unify' fargty argty TypeMolecule(Absyn.ArrowTerm(targskel, (List.tl argskels), numargs), fenv))
      else
        (unify' fargty argty TypeMolecule(targskel, fenv))
