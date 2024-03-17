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
type mapping = Mapping of (int * Absyn.atypevar)
let getMappingIndex = function
  Mapping(i,_) -> i

let getMappingTypeVar = function
  Mapping(_,t) -> t

(**********************************************************************
*mapi, iteri:
* Iterates over a list, applying a given function to each item in the
* list along with its index.
**********************************************************************)
let mapi f l =
  let rec map f l i =
    match l with
      [] -> []
    | l'::ls ->
        (f i l') :: (map f ls (i + 1))
  in
  map f l 0

let iteri f l =
  let rec iter f l i =
    match l with
      [] -> ()
    | l'::ls ->
        (f i l';
        iter f ls (i + 1))
  in
  iter f l 0
  
(**********************************************************************
*reduceSkeletons:
* For each constant in a module's symbol table, reduce the skeleton
* associated with the constant.  Note that this reduction is done
* destructively.
**********************************************************************)
let reduceSkeletons amod =
  (********************************************************************
  *reduceSkeleton:
  * Reduces a single type skeleton.
  ********************************************************************)
  let reduceSkeleton skel envsize =
    (*  An array representing the neededness information for each
        type variable in a constant's environment.  *)
    let argsUse = (Array.make envsize false) in
    let targetUse = (Array.make envsize false) in
    let resultNeededness = (Array.make envsize true) in

    (******************************************************************
    *process:
    * For each skeleton variable encountered, sets the associated
    * element in the given neededness array to true.
    ******************************************************************)
    let rec process array ty =
      match ty with
        Absyn.SkeletonVarType(r) ->
          let i = !r in
          (Array.set array i true)
      | Absyn.ArrowType(l, r) ->
          (process array l;
          process array r)
      | Absyn.ApplicationType(_,tl) ->
          (List.iter (process array) tl)
      | _ ->
          Errormsg.impossible Errormsg.none
            "Typereduction.process: invalid type"
    in
    
    (******************************************************************
    *processArg:
    * Simply processes an argument type and stores the result in the
    * arguments neededness array.
    ******************************************************************)
    let processArg ty =
      (process argsUse ty)
    in
    
    (******************************************************************
    *processTarget:
    * Processes the target type, handling the special case of when the
    * target type is just a type variable.
    ******************************************************************)
    let processTarget ty =  
      match ty with
        Absyn.SkeletonVarType(r) ->
          let i = !r in
          (Array.set targetUse i false)
      | _ -> process targetUse ty  
    in
    
    (******************************************************************
    *computeNeededness:
    * Given the use arrays for the arguments and the target type,
    * compute a neededness vector for the skeleton.  A type variable
    * is needed only when it is not used in both the arguments and the
    * target, with the exception of the target being a variable itself,
    * as discussed in processTarget.
    ******************************************************************)
    let rec computeNeededness i =
      if i >= (Array.length resultNeededness) then
        ()
      else
        let needed = not (Array.get targetUse i) in
          (*((Array.get argsUse i) && (Array.get targetUse i)) in*)
        (Array.set resultNeededness i needed;
        computeNeededness (i + 1))
    in
    
    (******************************************************************
    *renameVariables:
    * For each skeleton variable in a type, changes the skeleton
    * variable's index to the index in the new, reduced environment.
    ******************************************************************)
    let rec renameVariables ty =
      (****************************************************************
      *count:
      * Counts the number of elements of value v there are in the
      * neededness array at indices less than i.
      ****************************************************************)
      let rec count v i =
        if i = 0 then
          0
        else
          if (Array.get resultNeededness (i - 1)) = v then
            1 + (count v (i - 1))
          else
            (count v (i - 1))
      in
      let numTrue = (count true (Array.length resultNeededness)) in
      
      (****************************************************************
      *renameVariable:
      * Given an index i, computes the new index for the reduced
      * environment.
      ****************************************************************)
      let renameVariable i =
        let needed = Array.get resultNeededness i in
        if needed then
          (count true i)
        else
          numTrue + (count false i)
      in
      
      match ty with
        Absyn.SkeletonVarType(r) ->
          let i = !r in
          let i' = renameVariable i in
          (r := i')
      | Absyn.ArrowType(l,r) ->
          (renameVariables l;
          renameVariables r)
      | Absyn.ApplicationType(_,tl) ->
          (List.iter renameVariables tl)
      | _ ->
          Errormsg.impossible Errormsg.none
            "Typereduction.renameVariables: invalid type"
    in
    
    let ty = Absyn.getSkeletonType skel in
	let (args, target) = 
	  if (Absyn.isArrowType ty) then 
		  (Absyn.getArrowTypeArguments ty, Absyn.getArrowTypeTarget ty)
	  else
		  ([], ty)
	in

    (*let args = Absyn.getArrowTypeArguments ty in
      let target = Absyn.getArrowTypeTarget ty in *)
    
    (*  Compute the correct neededness information. *)
    (List.iter processArg args;
    processTarget target;
    computeNeededness 0;
    
    (*  Update all skeleton variables to reflect the new information. *)
    List.iter renameVariables args;
    renameVariables target;
    
    resultNeededness)       
  in
  
  (********************************************************************
  *reduceConstant:
  * Reduces a constant's skeleton and then destructively updates the
  * constant.
  ********************************************************************)
  let reduceConstant sym constant =
	
	(* calculate new environment size after reduction *)
	let calculateEnvSize size needed =
	  if (needed) then (size + 1)
	  else size
	in

	if (Absyn.isPervasiveConstant constant) then ()
	else
      let skel = Absyn.getConstantSkeleton constant in
      let envsize = Absyn.getConstantTypeEnvSize false constant in

      if Option.isSome skel then
		(*  Don't reduce the skeleton if there is no environment to reduce. *)
		if envsize > 0 then
          let neededness = (reduceSkeleton (Option.get skel) envsize) in
		  let newEnvSize = Array.fold_left calculateEnvSize 0 neededness in
		  (Absyn.getConstantTypeEnvSizeRef constant) := newEnvSize;
		  (Absyn.getConstantSkeletonNeedednessRef constant) := (Some(neededness))
		else
          ()
      else
		(Errormsg.impossible Errormsg.none 
     "Typereduction.reduceConstant: invalid skeleton")
  in
  
  (*  Iterate over the constant table, destructively reducing each
      constant's skeleton.  *)
  let _ = Errormsg.log Errormsg.none 
            "Typereduction.reduceSkeletons: reducing skeletons..." in
  let ctable = Absyn.getModuleConstantTable amod in
  let _ = Table.iter reduceConstant ctable in
  let _ = Errormsg.log Errormsg.none 
            "Typereduction.reduceSkeletons: reduced skeletons" in
    amod

let reducePredicates amod =
  (****************************************************************
  *getIndex:
  * Given a list and a predicate, returns the index of the first
  * element for which the predicate is true.
  ****************************************************************)
  let getIndex l f =
    let rec get' l f i =
      match l with
        [] -> raise Not_found
      | l'::ls ->
          if (f l') then
            i
          else
            get' ls f (i + 1)
    in
    get' l f 0
  in
  
  (****************************************************************
  *getVars:
  * Given a list of types, returns a list of typevars.
  ****************************************************************)
  let rec getVars tl =
    match tl with
      [] -> []
    | t::ts ->
        let t' = Absyn.dereferenceType t in
        if Absyn.isVariableType t' then
          let tvar = Absyn.getTypeFreeVariableVariableData t' in
          tvar::(getVars ts)
        else
          (getVars ts)
  in
  (****************************************************************
  *getClauses:
  * Gets a list of lists of clauses from a list of defs.
  ****************************************************************)
  let rec getClauses cbs =
    match cbs with
        [] -> []
      | ((_,clauses)::ds) ->
          (Absyn.getClauseBlockClauses clauses) :: (getClauses ds)
  in
  (********************************************************************
  *computeMapping:
  * Given a clause and a type, maps the given type to the correct
  * type in clause, if it exists.
  ********************************************************************)
  let computeMapping tvar clause =
    let compare tvar ty =
      let ty' = Absyn.dereferenceType ty in
      if Absyn.isVariableType ty' then
        let tvar' = Absyn.getTypeFreeVariableVariableData ty' in
        (tvar == tvar')
      else
        false
    in
    
    let Absyn.TypeVarMap(tymap) = Absyn.getClauseTypeVarMaps clause in
    let tenv = Absyn.getClauseTypeArgs clause in
    try
      let tvar' = List.assoc tvar tymap in
      let i = getIndex tenv (compare tvar') in
      Some (Mapping(i,tvar'))
    with Not_found -> None
  in

  (********************************************************************
  *makeConstantNeededness:
  * Creates a neededness vector for a given constant and initializes it
  * to all variables not needed.
  ********************************************************************)
  let makeConstantNeededness sym constant =
    let neededness = Absyn.getConstantNeedednessRef constant in
    if Option.isNone (!neededness) then
      let neededness' = 
        Array.make (Absyn.getConstantTypeEnvSize false constant) false in
      neededness := Some(neededness')
    else
      ()
  in
      
  (********************************************************************
  *initializeNeeded:
  * Initializes the neededness vector for each clause head.
  ********************************************************************)
  let initializeNeeded clause =
    (******************************************************************
    *initEmbeddedClauses:
    * Recurses over the structure of a goal.  For any implication goals,
    * it marks the neededness of each type variable that is mapped
    * to in the implication goals map.
    ******************************************************************)
    let rec initEmbeddedClauses neededness goal =
      match goal with
          Absyn.ImpGoal(defs,_,_,g) ->
            (**********************************************************
            *checkToMap:
            * Given a neededness vector, an index, a type map, and a
            * type, sets neededness[index] to true if the type is mapped
            * to in the type map.
            **********************************************************)
            let checkToMap neededness tymap i ty =
              let ty' = Absyn.dereferenceType ty in
              if Absyn.isVariableType ty' then
                let tvar = Absyn.getTypeFreeVariableVariableData ty' in
                let result = List.exists 
                               (function (_,tvar') -> (tvar == tvar')) tymap in
                if result then
                  (Array.set neededness i true)
                else
                  ()
              else
                ()
            in
            
            (**********************************************************
            *checkClause:
            * For each type argument, marks its neededness to true if
            * it is mapped to in the type map.
            **********************************************************)
            let checkClause clause =
              let constant = Absyn.getClausePred clause in
              let neededness' = Absyn.getConstantNeedednessValue constant in
              let targs = Absyn.getClauseTypeArgs clause in
              let Absyn.TypeVarMap(tymap) = Absyn.getClauseTypeVarMaps clause in
              
              (iteri (checkToMap neededness' tymap) targs)
            in
            let Absyn.Definitions(cbs) = defs in
            let clauses = List.flatten (getClauses cbs) in
            (List.iter checkClause clauses)
          
        | Absyn.AtomicGoal(_) -> ()
        | Absyn.AndGoal(g1,g2) ->
            (initEmbeddedClauses neededness g1;
            initEmbeddedClauses neededness g2)
        | Absyn.AllGoal(_,g) ->
            (initEmbeddedClauses neededness g)
        | Absyn.SomeGoal(_,g) ->
            (initEmbeddedClauses neededness g)
        | Absyn.CutFailGoal -> ()
    in
    
    (********************************************************************
    *checkGroundType:
    * Given a neededness vector, an index, and a type, sets
    * neededness[index] to true if the given type is not a variable.
    ********************************************************************)
    let checkGroundType neededness index ty =
      let ty' = Absyn.dereferenceType ty in
      if not (Absyn.isVariableType ty') then
        (Array.set neededness index true;
        true)
      else
        false
    in

    (********************************************************************
    *checkTypeForVar:
    * Checks the given type t for the given var tvar.
    ********************************************************************)
    let rec checkTypeForVar index tvar i t =
      if i == index then
        false
      else match t with
          Absyn.TypeVarType(_) ->
            let tvar' = Absyn.getTypeFreeVariableVariableData t in
            if tvar == tvar' then
              true
            else
              false
        | Absyn.ArrowType(l,r) ->
            (checkTypeForVar index tvar i l) || (checkTypeForVar index tvar i r)
        | Absyn.ApplicationType(_,args) ->
            let result = mapi (checkTypeForVar index tvar) args in
            (List.exists ((==) true) result)
        | _ ->
            (Errormsg.impossible Errormsg.none 
               "Typereduction.checkTypeForVar: invalid type")
    in

    (********************************************************************
    *checkOccurs:
    * Checks if ty occurs in any type in targs where the index of the
    * occurence is not the given index.
    ********************************************************************)
    let checkOccurs neededness targs index tvar =
      let result = mapi (checkTypeForVar index tvar) targs in
      if (List.exists ((==) true) result) then
        (Array.set neededness index true)          
      else
        ()
    in

    (********************************************************************
    *checkTerms:
    * Checks a list of terms for the given type variable.  If the variable
    * is located, marks the neededness at i to true.
    ********************************************************************)
    let checkTerms neededness terms i tvar =
      let rec checkTerm tvar t =
        match t with
            Absyn.ConstantTerm(_,env,_) ->
              let result = mapi (checkTypeForVar (-1) tvar) env in
              if (List.exists ((==) true) result) then
                (Array.set neededness i true)
              else
                ()

          | Absyn.IntTerm(_) -> ()
          | Absyn.RealTerm(_) -> ()
          | Absyn.StringTerm(_) -> ()
          | Absyn.BoundVarTerm(_) -> ()
          | Absyn.FreeVarTerm(_) -> ()
          
          | Absyn.AbstractionTerm(Absyn.NestedAbstraction(_,term),_) ->
              (checkTerm tvar term)
          | Absyn.AbstractionTerm(Absyn.UNestedAbstraction(_,_,term),_) ->
              (checkTerm tvar term)
          | Absyn.ApplicationTerm(Absyn.FirstOrderApplication(t,tl,_),_) ->
              (checkTerm tvar t;
              List.iter (checkTerm tvar) tl)
          | Absyn.ApplicationTerm(Absyn.CurriedApplication(t1,t2),_) ->
              (checkTerm tvar t1;
              checkTerm tvar t2)
          | Absyn.ErrorTerm ->
              Errormsg.impossible Errormsg.none 
                "Typereduction.checkTerm: invalid term"         
      in
      (List.iter (checkTerm tvar) terms)
    in

    (********************************************************************
    *checkEmbeddedClauses:
    * Checks the from part of variable mappings in embedded clauses.  If
    * the given type occurs, then the neededness of the given type is set
    * to the neededness of the type to which it is mapped.
    ********************************************************************)
    let rec checkEmbeddedClauses neededness goal index ty =
      match goal with
          Absyn.ImpGoal(defs,_,_,g) ->
            (**********************************************************
            *checkClause:
            **********************************************************)
            let checkClause clause =
              let mapping = computeMapping ty clause in
              if Option.isSome mapping then
                let index' = getMappingIndex (Option.get mapping) in
                let ty' = getMappingTypeVar (Option.get mapping) in

                let constant = Absyn.getClausePred clause in
                let neededness' = Absyn.getConstantNeedednessValue constant in
                let goal' = Absyn.getClauseGoal clause in
                
                (*  Mark the embedded clause with respect to the mapped to
                    type before marking this clause with respect to the current
                    type. *)
                let _ = checkEmbeddedClauses neededness' goal' index' ty' in
                
                let needed = Array.get neededness' index' in
                let current = Array.get neededness index in
                (Array.set neededness index (current || needed))
              else
                ()
            in
            let _ = checkEmbeddedClauses neededness g index ty in
            let Absyn.Definitions(cbs) = defs in
            let clauses = List.flatten (getClauses cbs) in
            (List.iter checkClause clauses)
        | Absyn.AtomicGoal(_) -> ()
        | Absyn.AndGoal(g1,g2) ->
            (checkEmbeddedClauses neededness g1 index ty;
            checkEmbeddedClauses neededness g2 index ty)
        | Absyn.AllGoal(_,g) ->
            (checkEmbeddedClauses neededness g index ty)
        | Absyn.SomeGoal(_,g) ->
            (checkEmbeddedClauses neededness g index ty)
        | Absyn.CutFailGoal -> ()
    in
    
    let constant = Absyn.getClausePred clause in
    let args = Absyn.getClauseTermArgs clause in
    let targs = Absyn.getClauseTypeArgs clause in
    let goal = Absyn.getClauseGoal clause in
    
    let neededness = (Absyn.getConstantNeedednessValue constant) in
    
    (*  Fill in the neededness as true if the constant isn't reducible. *)
    if not (Absyn.getConstantReducible constant) then
      (Array.fill neededness 0 (Array.length neededness) true)
    else
      (*  Initialize the embedded clauses.  *)
      let _ = (initEmbeddedClauses neededness goal) in    
      
      (*  Check for non-variable types.  *)
      let _ = mapi (checkGroundType neededness) targs in
      
      (*  Perform an occurs check for each variable.  *)
      let _ = mapi (checkOccurs neededness targs) (getVars targs) in
      
      (*  Check for occurences in constants in the head.  *)
      let _ = iteri (checkTerms neededness args) (getVars targs) in
      
      (*  Check embedded clauses and constants in the body. *)
      let _ = mapi (checkEmbeddedClauses neededness goal) (getVars targs) in
      ()    
  in
  
  (********************************************************************
  *computeNeededness:
  * Implements the fixed point
  ********************************************************************)
  let computeNeededness clauses =
    (******************************************************************
    *setNeededness:
    * Sets the specified index of a neededness vector to the given
    * value, and keeps track of whether this causes a change to the
    * vector.
    ******************************************************************)
    let changed = ref false in
    let setNeededness vector index value =
      let value' = Array.get vector index in
      if value <> value' then
        (Array.set vector index value;
        changed := true)
      else
        ()
    in
  
    (******************************************************************
    *processEmbeddedClause:
    * Processes the body of an embedded clause.  First, it flattens the
    * lists of clauses in the defs list.  It then processes each clause
    * and stores the results in a list.  If this list contains true, the
    * function returns true.  Otherwise, it returns false.
    ******************************************************************)
    let rec processEmbeddedClause defs tvinits ty =
      (****************************************************************
      *processEmbeddedClause:
      * Given a type and a clause, it first maps the type to the
      * associated type in the clause.
      ****************************************************************)
      let processEmbeddedClause ty clause =
        let mapping = computeMapping ty clause in
        if Option.isSome mapping then
          let ty' = getMappingTypeVar (Option.get mapping) in
          (match clause with
            Absyn.Fact(_) -> false
          | Absyn.Rule(_) ->
              let goal = Absyn.getClauseGoal clause in
              processGoal goal ty')
        else
          false
      in
      
      let Absyn.Definitions(cbs) = defs in
      let clauses = List.flatten (getClauses cbs) in
      let result = List.map (processEmbeddedClause ty) clauses in
      (List.exists ((==) true) result)

    (******************************************************************
    *processGoal:
    * Processes the body of a top-level clause.
    ******************************************************************)
    and processGoal goal ty =
      (****************************************************************
      *findUsage:
      * Given a neededness vector and a list of usages, return true
      * if any such usage is true, and false otherwise.
      ****************************************************************)
      let rec findUsage neededness uses =
        match uses with
            [] -> false
          | (Some i)::us ->
              if (Array.get neededness i) then
                true
              else
                (findUsage neededness us)
          | None::us -> (findUsage neededness us)
      in
      
      (****************************************************************
      *findType:
      * Given a type, an index, and a target type, returns Some i if
      * the given type occurs within the target type, and None otherwise.
      ****************************************************************)
      let rec findType tvar i targty =
        let targty' = Absyn.dereferenceType targty in
        match targty' with
            Absyn.TypeVarType(_) ->
              let tvar' = Absyn.getTypeFreeVariableVariableData targty' in
              if tvar' == tvar then
                Some i
              else
                None
          | Absyn.ArrowType(l,r) ->
              let l' = findType ty i l in
              if Option.isSome l' then
                l'
              else
              (findType ty i r)
          | Absyn.ApplicationType(_,tl) ->
              let result = List.map (findType ty i) tl in
              if List.exists (Option.isSome) result then
                Some i
              else
                None
          | _ -> Errormsg.impossible Errormsg.none 
                   "Typereduction.findType: invalid type"
      in
      
      match goal with
          Absyn.AtomicGoal(constant, _, _, args, targs) ->
            let result = (mapi (findType ty) targs) in
            if List.length result > 0 then
              let neededness = Absyn.getConstantNeedednessValue constant in
              (findUsage neededness result)
            else
              false
        | Absyn.ImpGoal(defs,_,tvinits,goal') ->
            (processGoal goal' ty) || (processEmbeddedClause defs tvinits ty)
        | Absyn.AndGoal(l,r) ->
            (processGoal l ty) || (processGoal r ty)
        | Absyn.AllGoal(_,goal') ->
            (processGoal goal' ty)
        | Absyn.SomeGoal(_,goal') ->
            (processGoal goal' ty)
        | Absyn.CutFailGoal ->
            false
    in

    (******************************************************************
    *processClause:
    * Process a single clause.  If the clause is a fact, then no
    * processing need be done.
    ******************************************************************)
    let processClause clause =
      let constant = Absyn.getClausePred clause in
      let targs = Absyn.getClauseTypeArgs clause in
      let goal = Absyn.getClauseGoal clause in
      let neededness = (Absyn.getConstantNeedednessValue constant) in
      
      (****************************************************************
      *process:
      * Given a type and its index in the current constant's neededness
      * vector, process the body of the current clause with respect to
      * said type, and enter the result into the neededness vector.
      ****************************************************************)
      let process i ty =
        let tvar = Absyn.getTypeFreeVariableVariableData ty in
        let result = processGoal goal tvar in
        (setNeededness neededness i result)
      in
      (iteri process targs)
    in
    
    
    (******************************************************************
    *processClauses:
    * Calls processClause on each clause.  If this causes a change to 
    * any neededness vector, calls processClauses again.
    ******************************************************************)
    let rec processClauses () =
      (changed := false;
      List.iter processClause clauses;
      if !changed then
        processClauses ()
      else
        ())
    in
    processClauses ()
  in
  
  (*  Create neededness vectors for all constants.  *)
  let ctable = Absyn.getModuleConstantTable amod in
  let _ = Table.iter makeConstantNeededness ctable in
  
  (*  Initialize the fixed-point data.  *)
  let cinfo = Absyn.getModuleClauses amod in
  let clauseblocks = Absyn.getClauseInfoClauseBlocks cinfo in
  let clauses = 
    List.flatten 
      (List.map (fun cb -> (Absyn.getClauseBlockClauses cb)) clauseblocks) in
  let _ = List.iter initializeNeeded clauses in
  
  (*  Run the fixed-point algorithm to compute neededness.  *)
  let _ = computeNeededness clauses in

  amod

(* This initializes constant and constant skeleton neededness 
 * for queries. The expected behavior is that a query should
 * only have external access to a module, as if it was importing
 * the module. Since neededness values are only local to a module, 
 * any call to a predicat or constant from outside the module 
 * (and thus from a query) must assume maximal neededness.
 *)
let initConstantAndSkeletonNeedednessTopLevel amod =
  (* Note: true must be passed to getConstantTypeEnvSize
   * because we are in the toplevel *)
  let makeConstantNeededness sym constant =
    let neededness = Absyn.getConstantNeedednessRef constant in
    if Option.isNone (!neededness) then
      let size = (Absyn.getConstantTypeEnvSize true constant) in
      let neededness' = 
        Array.make size true in
      neededness := Some(neededness')
    else
      ()
  in
  let makeConstantSkeletonNeededness sym constant =
    (* prerr_endline (Format.sprintf "making skeleton neededness for const %s[%d]"
     *                (Symbol.name sym) (Absyn.getConstantTypeEnvSize true constant)); *)
    let neededness = Absyn.getConstantSkeletonNeedednessRef constant in
    if Option.isNone (!neededness) then
      let size = (Absyn.getConstantTypeEnvSize true constant) in
      let neededness' =
        Array.make size true in
      neededness := Some(neededness')
    else ()
  in
  let ctable = Absyn.getModuleConstantTable amod in
  let _ = Table.iter makeConstantNeededness ctable in
  let _ = Table.iter makeConstantSkeletonNeededness ctable in
  amod
