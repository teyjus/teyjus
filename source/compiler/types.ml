(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Nathan Guermond, Steven Holte, 
*  Gopalan Nadathur, Xiaochu Qi, Zach Snow
****************************************************************************)
(****************************************************************************
* This file is part of Teyjus.
*
* Teyjus is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Teyjus is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.
****************************************************************************)
type typemolecule =
  Molecule of (Absyn.atype * Absyn.atype list)

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
exception EqualMappedTypeSkelsFailure

(**********************************************************************
*constantType:
* Constructs the type of the given constant, building an appropriate
* environment.
*
* Arguments:
*   parsingtoplevel
**********************************************************************)
let makeConstantMolecule parsingtoplevel constant =
  let env = ref [] in
  let bindings = ref [] in
  let rec instance ty =
    let ty' = Absyn.dereferenceType ty in
    match ty' with
      Absyn.TypeSetType(def, l, _) ->
        if List.mem_assq ty' !bindings then
          List.assq ty' !bindings
        else
          let l' = ref (List.map instance (!l)) in
          let t = Absyn.TypeSetType(def, l', ref None) in
          (env := t :: (!env);
          bindings := (ty', t) :: !bindings;
          t)
    | Absyn.ApplicationType(k, tl) ->
        Absyn.ApplicationType(k, (List.map instance tl))
    | Absyn.ArrowType(l,r) -> Absyn.ArrowType(instance l, instance r)
    | Absyn.SkeletonVarType(_) -> ty'
    | Absyn.TypeVarType(_) -> ty'
    | Absyn.ErrorType -> ty'
  in
  
  let envsize = 
    if (Absyn.isPervasiveConstant constant) then 
      Absyn.getConstantTypeEnvSize true constant 
    else
      Absyn.getConstantTypeEnvSize parsingtoplevel constant 
  in
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
  | [] -> (Errormsg.impossible Errormsg.none "Types.getEnvironmentElement: \
                                              invalid element index")

(**********************************************************************
*dereferenceMolecule:
* Dereference a type molecule.  The result of dereferenceMolecule should
* be a molecule whose type has no skeleton variables or bound type
* variables.
**********************************************************************)
let dereferenceMolecule (Molecule(t, env)) =
  (********************************************************************
  *get:
  * Gets the ith type from the type environment.
  ********************************************************************)
  let get i env =
    try
      List.nth env i
    with
      Failure(s) -> (Errormsg.impossible Errormsg.none 
                                         ("Types.dereferenceMolecule: \
                                          invalid skeleton index: " ^ 
                                          (string_of_int i) ^ 
                                          (Errormsg.info ("type: " ^ 
                                          (Absyn.string_of_type t)))))
  in
  
  (********************************************************************
  *dereference:
  * Dereferences a skeleton variable.  Also dereferences regular type
  * variables using Absyn.dereferenceType. 
  ********************************************************************)
  let rec dereference t =
    (*  Follow references *)
    let t' = (Absyn.dereferenceType t) in
    if Absyn.isSkeletonVariableType t' then
      let i = (Absyn.getSkeletonVariableIndex t') in
      dereference (get i env)
    else
      t'
  in
  Molecule(dereference t, env)

(**********************************************************************
*string_of_typemolecule:
* Generates a string representation of a type molecule.
**********************************************************************)
let rec string_of_typemolecule = fun mol ->
  let (s, _) = string_of_typemolecule' mol [] false in
  s

and string_of_typemolecule' = fun mol bindings printpar ->
  let Molecule(t, env) = (dereferenceMolecule mol) in

  let string_of_skelvar i =
    let get i env =
      try
        let e = (List.nth env i) in
        string_of_typemolecule' (Molecule(Absyn.dereferenceType e, env)) 
                                bindings printpar
      with
        Failure(s) -> (Errormsg.impossible Errormsg.none 
                                           ("Types.string_of_typemolecule': \
                                            invalid skeleton index: " ^ 
                                            (string_of_int i)))
    in
    get i env
  in
  
  let string_of_var = fun bindings ->
    let character i =
      if i>= 26 then
        (string_of_int i)
      else
        (String.make 1 (Char.chr ((Char.code 'A') + i)))
    in
    try
      let i = List.assq t bindings in
      (character i, bindings)
    with
      Not_found ->
        let i = List.length bindings in
        (character i, (t, i)::bindings)
  in
  
  let string_of_typeset ts bindings =
    match ts with
      Absyn.TypeSetType(d, t, _) ->
        (match !t with
          [t] -> string_of_typemolecule' (Molecule(t, env)) bindings printpar
        | _ -> string_of_typemolecule' (Molecule(d, env)) bindings printpar)
    | _ -> (Errormsg.impossible Errormsg.none "Types.string_of_typeset: \
                                               invalid typeset")
  in
  
  let rec string_of_args = fun args bindings ->
    match args with
      arg::args' ->
        let (arg', bindings') = 
                 string_of_typemolecule' (Molecule(arg, env)) bindings printpar
        in
        let (args'', bindings'') = (string_of_args args' bindings') in
        (" " ^ arg' ^ args'', bindings'')
    | [] -> ("", bindings)
  in
  
  let string_of_type_and_bindings t =
    match (Absyn.dereferenceType t) with
      Absyn.SkeletonVarType(i) -> (string_of_skelvar !i)
    | Absyn.TypeVarType(_) -> (string_of_var bindings)
    | Absyn.TypeSetType(_) -> (string_of_typeset t bindings)
    | Absyn.ApplicationType(k,args) ->
        let s = (Absyn.getKindName k) in
        let (args', bindings') = (string_of_args args bindings) in
        if args' = "" then
          (s, bindings')
        else
          ("(" ^ s ^ args' ^ ")", bindings')
    | Absyn.ArrowType(l,r) ->
        let (l', bindings') = 
               string_of_typemolecule' (Molecule(l, env)) bindings true
        in
        let (r', bindings') = 
               string_of_typemolecule' (Molecule(r, env)) bindings' false 
        in
           if printpar 
           then ("(" ^ l' ^ " -> " ^ r' ^ ")", bindings')
           else (l' ^ " -> " ^ r', bindings')
    | Absyn.ErrorType -> ("error", bindings)
  in
    string_of_type_and_bindings t

(********************************************************************
*getMoleculeListTypes;
********************************************************************)
let rec getMoleculeListTypes args =
  match args with
    [] -> []
  | a::aa ->
      (getMoleculeType a)::(getMoleculeListTypes aa)

(********************************************************************
*replaceType:
* Finds the given type and converts it to a skeleton type.
********************************************************************)
let rec replaceType ty env i =
  match env with
    [] -> None
  | t::ts ->
      if t == ty then
        Some(Absyn.SkeletonVarType(ref i))
      else
        replaceType ty ts (i + 1)

(********************************************************************
*skeletonizeArgs:
* Skeletonizes a list of types in turn.
********************************************************************)
let rec skeletonizeArgs args env index =
  match args with
    [] -> ([], env, false, index)
  | a::aa ->
      let _ = Errormsg.log Errormsg.none ("Types.skeletonizeArgs: " ^ 
                                          (Absyn.string_of_type_ast a)) in
      let (a', newvars, index') = skeletonize a env index in
      let env' = getMoleculeEnvironment a' in
      
      let (args', env'', newvars', index'') = skeletonizeArgs aa env' index' in
      (a'::args', env'', newvars || newvars', index'')

(********************************************************************
*skeletonize:
* Primary skeletonization routine; recurses over structure of type.
********************************************************************)
and skeletonize ty env index =
  let ty' = Absyn.dereferenceType ty in
  match ty' with
    Absyn.TypeVarType(r) ->
      (match r with
           (* The reference is always set to None at this point *)
         | Absyn.BindableTypeVar(r) when r = ref None ->
             let ty'' = replaceType ty' env 0 in
               if Option.isSome ty'' then
                 (Molecule(Option.get ty'', env), false, index)
               else
                 let index' = index + 1 in
                   (Molecule(Absyn.SkeletonVarType(ref index), env @ [ty']), 
                    true, 
                    index')
         | _ -> Errormsg.impossible Errormsg.none 
                  "Types.skeletonize: invalid type variable")
  | Absyn.ApplicationType(k, args) ->
      let (argmols, env', newvars, index') = skeletonizeArgs args env index in
      let args' = getMoleculeListTypes argmols in
        (Molecule(Absyn.ApplicationType(k, args'), env'), newvars, index')
  | Absyn.ArrowType(_) ->
      let targty = Absyn.getArrowTypeTarget ty' in
      let argtys = Absyn.getArrowTypeArguments ty' in
             
      let (targmol, newvars, index') = skeletonize targty env index in
      let targty' = getMoleculeType targmol in
      let env' = getMoleculeEnvironment targmol in
      
      let (argmols, env'', newvars', index'') = 
                      skeletonizeArgs argtys env' index' in
      let argtys' = getMoleculeListTypes argmols in
      (Molecule(Absyn.makeArrowType targty' argtys', env''), 
       newvars || newvars', index'')
  | Absyn.TypeSetType(_) ->
      (Errormsg.impossible Errormsg.none 
                           "Types.skeletonize: invalid type set")
  | _ -> (Molecule(ty', env), false, index)


let skeletonizeType ty =
  let _ = Errormsg.log Errormsg.none ("Types.skeletonizeType: " ^ 
                                      (Absyn.string_of_type_ast ty)) in
  let (mol, _, index) = skeletonize ty [] 0 in

  if index > Pervasiveutils.maxSkeletonIndex then
    (Errormsg.error Errormsg.none
                    "Unable to skeletonize type: \
                     type contains too many free variables";
    errorMolecule)
  else
    mol

(**********************************************************************
*skeletonizeMolecule:
* Converts a molecule with free variables and skeleton variables
* into a type molecule with skeleton variables only.
**********************************************************************)
let skeletonizeMolecule tmol =
  let Molecule(ty, env) = tmol in
  let (mol', _, index) = skeletonize ty env (List.length env) in
  if index > Pervasiveutils.maxSkeletonIndex then
    (Errormsg.error Errormsg.none
                    "Unable to skeletonize type: \
                     type contains too many free variables";
    errorMolecule)
  else
    mol'

(**********************************************************************
*occursCheck:
* Performs an occurs check on a type: given a type variable and a type,
* determines if the variable occurs in the type.
**********************************************************************)
let rec occursCheck var skel bindings =
  match skel with
    Absyn.ErrorType -> skel
  | Absyn.TypeVarType(_) ->
      if var == skel then
        raise (UnifyException(OccursCheckFailure))
      else
        skel
  | Absyn.SkeletonVarType(i) ->
      let skel' = (Absyn.dereferenceType (getEnvironmentElement bindings !i)) 
      in occursCheck var skel' []
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
* This is only useful at the top level
**********************************************************************)
let rec unbindVariables = function
  | [] -> ()
  | Absyn.TypeVarType(r)::vs ->
      (match r with
           Absyn.BindableTypeVar(tr) ->
             (tr := None;
              unbindVariables vs)
         | _ -> Errormsg.impossible Errormsg.none 
                  "Types.unbindVariables: invalid type variable")
  | v::vs ->
      (Errormsg.impossible Errormsg.none
         "Types.unbindVariables: \
         non-variable type encountered.")
    
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
let rec unify (tm1 : typemolecule) (tm2 : typemolecule) =
  let bindSkeletons skel1 t1 skel2 t2 bindings =
    (*  If both are variables, bind one to the other. *)
    if (Absyn.isVariableType skel2) then
      (*  Only bind if the variables are not equal. *)
      if (skel1 == skel2) then
        bindings
      else
        ((Absyn.getTypeVariableReference skel1) := (Some(skel2));
        skel1::bindings) 
    else
      (*  Otherwise just bind.  *)
      (bindVariable skel1 t2 bindings)
  in
  
  let isGround ty = match ty with
    Absyn.ApplicationType(_) -> true
  | _ -> false
  in
  
  let rec inSet s set =
    match set with
      s'::set' -> if s = s' then true else inSet s set'
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
    
  (********************************************************************
  *unify':
  * Auxiliary unification function.
  ********************************************************************)
  let rec unify' t1 t2 bindings =
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
      bindSkeletons skel1 t1 skel2 t2 bindings
    else if Absyn.isVariableType skel2 then
      bindSkeletons skel2 t2 skel1 t1 bindings
    else
    
    (*  Check for type-set hackery to handle overloaded operators.  *)
    if Absyn.isTypeSetType skel1 then
      let set1 = Absyn.getTypeSetSet skel1 in
      (*  If both are type sets, then set one set to the intersection of
          the sets, and the reference of the other set to point at this
          set.  If the intersection is empty, clash error. *)
      if Absyn.isTypeSetType skel2 then
        let set2 = Absyn.getTypeSetSet skel2 in
        let inter = intersection (!set1) (!set2) in
        
        if (List.length inter) = 0 then
          (Errormsg.log Errormsg.none "Types.unify: empty intersection.";
          unbindVariables bindings;
          raise (UnifyException ClashFailure))
        else
          let ref1 = Absyn.getTypeSetRef skel1 in
          let ref2 = Absyn.getTypeSetRef skel2 in
          let _ = set1 := inter in
          if ref1 == ref2 then
            bindings
          else
            (ref2 := Some(skel1);
            bindings)
      else

      (*  Check if the other type is in the list.  If it is, then
          just remove all but that item from the list.  If it isn't,
          then clash error. *)
        if (isGround skel2) && (inSet skel2 (!set1)) then
          (set1 := [skel2];
          bindings)
        else
          (Errormsg.log Errormsg.none "Types.unify: type not in set.";
          unbindVariables bindings;
          raise (UnifyException ClashFailure))
    else
    
    match skel2 with
      (*  Type set: check if skel1 is in the type set; if so,
          set the type set to only have skel1 in it.  Otherwise,
          clash error.  *)
      Absyn.TypeSetType(def, set2, r2) ->
        if (isGround skel1) && (inSet skel1 (!set2)) then
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
                  let bindings' = unify' (Molecule(a1, env1)) 
                                         (Molecule(a2, env2)) 
                                         bindings 
                  in (checkArgs a1s a2s bindings')
              | _ -> (Errormsg.impossible Errormsg.none 
                                          "Types.unify: invalid number \
                                           of application arguments")
            in
            
            (*  Match the head. *)
            if f <> f' then
              (Errormsg.log Errormsg.none ("Types.unify: unable to unify " ^ 
                                           (Absyn.getKindName f) ^ " and " ^ 
                                           (Absyn.getKindName f'));
              unbindVariables bindings;
              raise (UnifyException ClashFailure))
            else
            
            (*  Get application argument types. *)
            let args1 = a' in
            let args2 = a in
            
            (checkArgs args1 args2 bindings)
            
        | _ ->
            (Errormsg.log Errormsg.none 
                          ("Types.unify: error checking application: \
                            invalid type: " ^ (Absyn.string_of_type skel1));
            unbindVariables bindings;
            raise (UnifyException ClashFailure)))
      
      (*  Arrow type: first make sure both types are arrow types.
          Then match the left and right sides of the arrow. *)
    | Absyn.ArrowType(l, r) ->
        (match skel1 with
          Absyn.ArrowType(l', r') ->
            let bindings' = (unify' (Molecule(l', env1)) 
                                    (Molecule(l, env2)) 
                                    bindings) 
            in (unify' (Molecule(r', env1)) (Molecule(r, env2)) bindings')
        | _ ->
            (Errormsg.log Errormsg.none "Types.unify: \
                                         error checking arrow: invalid type.";
            unbindVariables bindings;
            raise (UnifyException(ClashFailure))))
    | Absyn.ErrorType -> (Errormsg.impossible (Errormsg.none) 
                                              "Types.unify: \
                                               Absyn.ErrorType encountered.")
    | _ ->
        (Errormsg.impossible (Errormsg.none) 
                             "Types.unify: Invalid type encountered.")
  in
  try
    (*  Only success or failure matters.  Discard the result. *)
    (*  let _ = Errormsg.log Errormsg.none
                             ("Unifying types: " ^ 
                              (string_of_typemolecule tm1) ^ 
                              "; " ^ (string_of_typemolecule tm2)) in *)

    let _ = unify' tm1 tm2 [] in
    Success
  with
    UnifyException(t) -> t

(**********************************************************************
*clashError:
* Records a clash error when type checking.
**********************************************************************)
let clashError = fun fargty argty term ->
  let (expected, bindings) = string_of_typemolecule' fargty [] false in
  let (actual, _) = string_of_typemolecule' argty bindings false in
    Errormsg.error (Absyn.getTermPos term) 
      ("clash in operator and operand types" ^
       (Errormsg.info ("expected operand type: " ^ expected)) ^
       (Errormsg.info ("actual operand type: " ^ actual)) ^
       (Errormsg.info ("in expression: " ^ (Absyn.string_of_term term))))

(**********************************************************************
*occursCheckError:
* Records an occurs-check error when type checking.
**********************************************************************)
let occursCheckError = fun fargty argty term ->
  let (operator, bindings) = string_of_typemolecule' fargty [] false in
  let (operand, _) = string_of_typemolecule' argty bindings false in
  
  Errormsg.error (Absyn.getTermPos term) ("occurs-check failure" ^
    (Errormsg.info ("operator type: " ^ operator)) ^
    (Errormsg.info ("operand type: " ^ operand)) ^
    (Errormsg.info ("in expression: " ^ (Absyn.string_of_term term))))


(**********************************************************************
*checkApply:
* Check an application between a function and an argument i.e. if the
* application is well-typed 
* -  term is the term application, used to display error messages
**********************************************************************)
let checkApply fty argty term =
  (********************************************************************
  *runUnify:
  * Attempt to unify the first argument of an application with the first
  * expected argument. Calls necessary error functions.
  ********************************************************************)
  let runUnify farg arg result =
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
    
    (*  The function's type is a variable *)
    if (Absyn.isVariableType fskel) then
      let fargskel = Absyn.makeTypeVariable () in
      let fargty = Molecule(fargskel, []) in
      let targskel = Absyn.makeTypeVariable () in
      let targty = Molecule(targskel, []) in
      let fty = (Absyn.makeArrowType targskel [fargskel]) in
        (* Replace the current type variable with an arrow and two
        * type variables, X -> Y *)
        ((Absyn.getTypeVariableReference fskel) := (Some(fty));
         (* This can only success *)
         (runUnify fargty argty targty))

    (*  The function's type is an arrow *)
    else if (Absyn.isArrowType fskel) then
      let fenv = getMoleculeEnvironment fty in
      let argskels = (Absyn.getArrowTypeArguments fskel) in
      let targskel = (Absyn.getArrowTypeTarget fskel) in
      let fargty = Molecule(List.hd argskels, fenv) in
      let numargs = (List.length argskels) - 1 in
        if (numargs) > 0 then
          (runUnify fargty argty 
             (Molecule((Absyn.makeArrowType targskel (List.tl argskels)), 
                       fenv)))
        else
          (runUnify fargty argty (Molecule(targskel, fenv)))

    (* If the type is neither a variable or an arrow type, there is no hope *)
    else 
      let (operator, _) = string_of_typemolecule' fty [] false in
        (Errormsg.error 
           (Absyn.getTermPos term) 
           ("operator is not a function" ^ 
            (Errormsg.info ("operator type: " ^ operator)) ^
            (Errormsg.info 
               ("in expression: " ^ (Absyn.string_of_term term) ^ ".")));
         errorMolecule)

(* Produce a list of type variables that appear free in the given type *)
(* expression and are new to the given list of type variables.         *)
let rec freeTypeVars tyexp tyfvs =
  match tyexp with
    | Absyn.TypeVarType(varInfo) ->
        (match (varInfo) with 
           | Absyn.BindableTypeVar(binding) ->
               if Option.isNone (!binding) then (* type variable *)
                 (* whether the variable appears in the given var list *)
                 let rec isNewTyFv tyfvs =
                   match tyfvs with
                       [] -> true
                     | (tyfv :: rest) -> 
                         if (tyfv == tyexp) then false
                         else isNewTyFv rest
                 in
                   if (isNewTyFv tyfvs) then 
                     (tyexp::tyfvs)
                   else tyfvs
               else (* a type reference really *)
                 (* It seems that we are never in this situation,
                  * possibly due to a previous call to dereferenceType *)
                 freeTypeVars (Option.get (!binding)) tyfvs
           | _ -> Errormsg.impossible Errormsg.none 
                    "freeTypeVars: invalid type expression")
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
			
(**********************************************************************
*equalMappedTypeSkels:
* Compares two type skeletons for equality.
**********************************************************************)
let equalMappedTypeSkels skel1 skel2 =
  (********************************************************************
  *equalMappedTypeSkels':
  * Auxiliary function.  Compares
  ********************************************************************)
  let rec equalMappedTypeSkels' vl skel1 skel2 =
    (*  Bail early on error.  *)
    if Absyn.isErrorType skel1 || Absyn.isErrorType skel2 then
      vl
    else
    
    match skel1 with
      Absyn.SkeletonVarType(r1) ->
        let findI i v =
          let (b, i') = v in
          (i == i')
        in
        let i1 = !r1 in
        
        if Absyn.isSkeletonVariableType skel2 then
          let i2 = Absyn.getSkeletonVariableIndex skel2 in
          try
            let i = List.assoc (i1) vl in
            if i == (i2) then
              vl
            else
              raise (EqualMappedTypeSkelsFailure)
          with Not_found ->
            let i = i2 in
            if not  (List.exists (findI i) vl) then
              (i1, i) :: vl
            else
              raise (EqualMappedTypeSkelsFailure)
        else
          raise (EqualMappedTypeSkelsFailure)
    | Absyn.ApplicationType(k, args) ->
        if Absyn.isApplicationType skel2 then
          let k' = Absyn.getApplicationTypeHead skel2 in
          let args' = Absyn.getApplicationTypeArgs skel2 in
          if k == k' && List.length args = List.length args' then
            List.fold_left2 equalMappedTypeSkels' vl args args'
          else
            raise (EqualMappedTypeSkelsFailure)
        else
          raise (EqualMappedTypeSkelsFailure)
    | Absyn.ArrowType(_) ->
        let t1 = Absyn.getArrowTypeTarget skel1 in
        let args1 = Absyn.getArrowTypeArguments skel1 in
        
        if Absyn.isArrowType skel2 then
          let t2 = Absyn.getArrowTypeTarget skel2 in
          let args2 = Absyn.getArrowTypeArguments skel2 in
          let vl' = equalMappedTypeSkels' vl t1 t2 in
		  if (List.length args1 = List.length args2) then
			(List.fold_left2 equalMappedTypeSkels' vl' args1 args2)
		  else raise (EqualMappedTypeSkelsFailure)
        else
          raise (EqualMappedTypeSkelsFailure)
    | _ -> Errormsg.impossible Errormsg.none 
                               "Types.equalMappedTypeSkels: invalid type."
  in
    let t1 = Absyn.getSkeletonType skel1 in
    let t2 = Absyn.getSkeletonType skel2 in
    try
      let _ = (equalMappedTypeSkels' [] t1 t2) in
      true
    with
      EqualMappedTypeSkelsFailure -> false


(*************************************************************************
*getNewVarsInTypes:
* Accumulates the new type variables occurring in a given list of types 
* into a growing list. Entries in the list of type variables are
* unique as addresses, not as structures
**************************************************************************)
let rec getNewVarsInTypes tyl ftyvars =
  match tyl with 
      (ty::tyl') ->
         let ftyvars' = getNewVarsInType (Absyn.dereferenceType ty) ftyvars in
         getNewVarsInTypes tyl' ftyvars'
    | [] -> ftyvars

(* this function actually works also in the case that skeleton variables appear 
  in the type *)
and getNewVarsInType ty ftyvars =
  match ty with
      Absyn.SkeletonVarType(_) -> ftyvars
    | Absyn.TypeVarType(_) -> 
        if (List.memq ty ftyvars) then
          ftyvars
        else
          (ty::ftyvars)
    | Absyn.ArrowType(t1,t2) -> (getNewVarsInTypes [t1; t2] ftyvars) 
    | Absyn.ApplicationType(_,tyl) -> (getNewVarsInTypes tyl ftyvars)
    | Absyn.ErrorType -> ftyvars
    | _ ->
        Errormsg.impossible Errormsg.none
                            "Types.getNewVarsInTypes: \
                             unexpected form encountered in type"

(**********************************************************************
*getNewVarsInTypeMol:
* Like getNewVarsInType, except that there is a need to carry out a
* two step process in this case, one for the (possible) skeleton and one
* for any environment types
**********************************************************************)
let getNewVarsInTypeMol (Molecule(ty,tyenv)) ftyvars =
       let ftyvars' = getNewVarsInType ty ftyvars in
         getNewVarsInTypes tyenv ftyvars'

(**********************************************************************
*replaceTypeSetType:
* Replaces a type set type with a regular type.  If the type set contains
* more than one type, it returns the type set default.  If the type
* set contains only one type, that type is returned.  Any other case
* is an error.
**********************************************************************)
let rec replaceTypeSetType t =
  let t' = Absyn.dereferenceType t in
  match t' with
    Absyn.TypeSetType(_) ->
      let set = !(Absyn.getTypeSetSet t') in
      (match set with
        [] ->
          Errormsg.impossible Errormsg.none
                              ("Types.replaceTypeSetType: invalid type set in "
                               ^ "constant environment.")
      | [t''] -> replaceTypeSetType t''
      | _ -> replaceTypeSetType (Absyn.getTypeSetDefault t'))
  | Absyn.ApplicationType(k,l) ->
      Absyn.ApplicationType(k, List.map replaceTypeSetType l)
  | Absyn.ArrowType(l,r) ->
      Absyn.ArrowType(replaceTypeSetType l, replaceTypeSetType r)
  | Absyn.TypeVarType(_)
  | Absyn.SkeletonVarType(_)
  | Absyn.ErrorType -> t'

(**********************************************************************
*unitTests:
* Simple "tests" to check if various functions provide reasonable
* output; the output has to be checked "by hand".
**********************************************************************)
let unitTests () =
  let test tmol1 tmol2 =
    let _ = Errormsg.log Errormsg.none 
                         ("Unifying " ^ (string_of_typemolecule tmol1) ^ 
                          " ; " ^ (string_of_typemolecule tmol2)) in
    let _ =
      if (unify tmol1 tmol2) = Success then
        (Errormsg.log Errormsg.none ("Unification Succeeded.");
        Errormsg.log Errormsg.none 
                     ("Unified " ^ (string_of_typemolecule tmol1) ^ 
                      " ; " ^ (string_of_typemolecule tmol2)))  
      else
        Errormsg.log Errormsg.none ("Unification Failed.")
    in
    ()
  in
  
  let _ = Errormsg.log Errormsg.none ("Types Unit Tests:") in
  
  (*  Skeletonization Tests:  *)
  let tv = Absyn.makeTypeVariable () in
  let ty = Absyn.ApplicationType(Absyn.Kind(Symbol.symbol "test", Some 1, ref 0,
                                            Absyn.GlobalKind, Errormsg.none),
    [tv; Absyn.makeTypeVariable (); Absyn.makeTypeVariable (); tv]) in
  let tskel = skeletonizeType ty in
  let _ = Errormsg.log Errormsg.none ("Test type: " ^ 
                                      (Absyn.string_of_type ty)) in
  let _ = Errormsg.log Errormsg.none ("Test skeleton: " ^ 
                                      (string_of_typemolecule tskel)) in
  
  (*  Unification Tests:  *)
  let tmol1 = Molecule(Absyn.ApplicationType(Pervasive.kint, []), []) in
  let tmol2 = Molecule(
    Absyn.makeTypeSetVariable
      (Absyn.ApplicationType(Pervasive.kint,[]))
      [Absyn.ApplicationType(Pervasive.kint,[]); 
       Absyn.ApplicationType(Pervasive.kreal,[])],
    []) in
  let _ = test tmol1 tmol2 in
  
  let tmol1 = Molecule(Absyn.ApplicationType(Pervasive.kint, []), []) in
  let tmol2 = Molecule(
    Absyn.makeTypeSetVariable
      (Absyn.ApplicationType(Pervasive.kint,[]))
      [Absyn.ApplicationType(Pervasive.kint,[]); 
       Absyn.ApplicationType(Pervasive.kreal,[])],
    []) in
  let _ = test tmol2 tmol1 in
  
  let tmol3 = Molecule(
    Absyn.makeTypeSetVariable
      (Absyn.ApplicationType(Pervasive.kint,[]))
      [Absyn.ApplicationType(Pervasive.kint,[]); 
       Absyn.ApplicationType(Pervasive.kreal,[])],
    []) in
  let tmol4 = Molecule(
    Absyn.makeTypeSetVariable
      (Absyn.ApplicationType(Pervasive.kint,[]))
      [Absyn.ApplicationType(Pervasive.kint,[]); 
       Absyn.ApplicationType(Pervasive.kreal,[])],
    []) in
  let _ = test tmol3 tmol4 in
  
  let tmol5 = Molecule(
    Absyn.makeTypeSetVariable
      (Absyn.ApplicationType(Pervasive.kint,[]))
      [Absyn.ApplicationType(Pervasive.kint,[]); 
       Absyn.ApplicationType(Pervasive.kreal,[])],
    []) in
  let tmol6 = Molecule(
    Absyn.makeTypeSetVariable
      (Absyn.ApplicationType(Pervasive.kint,[]))
      [Absyn.ApplicationType(Pervasive.kint,[])],
    []) in
  let _ = test tmol5 tmol6 in
  
  (*  Check Apply Tests *)
  let plus = makeConstantMolecule false Pervasive.overloadPlusConstant in
  let arg = Molecule(Absyn.ApplicationType(Pervasive.kint, []), []) in
  let _ = Errormsg.log Errormsg.none 
                       ("Testing checkApply (" ^ 
                        (string_of_typemolecule plus) ^ 
                        ") (" ^ (string_of_typemolecule arg) ^ ")") in
  let result = checkApply plus arg Absyn.errorTerm in
  let _ = Errormsg.log Errormsg.none ("checkApply Result: " ^ 
                                      (string_of_typemolecule result)) in
  
  let _ = Errormsg.log Errormsg.none ("Types Unit Tests Compete.\n") in
  ()
  
