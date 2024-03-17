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

(*****************************************************************************)
(* Module annvariables:                                                      *)
(* processing clause representations in a module, annotate type and term     *)
(* variables as temporary or permanent, and decide offset for permanent      *)
(* variables. Fill in variable related information for clause                *)
(* representations: hasenv, cutvar and goal-environment size association.    *)
(*****************************************************************************)

(** ********************************************************************** **)
(** GLOBAL FLAGS AND LISTS AND ACCESS FUNCTIONS                            **)
(** ********************************************************************** **)
(**************************************************************************)
(* whether in an embedded context                                         *)
(**************************************************************************)
let embedded : bool ref = ref false
let isEmbedded () = !embedded
let setEmbedded flag = embedded := flag

(**************************************************************************)
(* the goal number of the goal currently being processed                  *)
(**************************************************************************)
let goalNumber : int ref = ref 1
let getGoalNum ()     = !goalNumber
let incGoalNum ()     = goalNumber := !goalNumber + 1
let setGoalNum number = goalNumber := number 

(**************************************************************************)
(* whether the previous goal being processed is a non-register-clobbering *)
(* pervasive                                                              *)
(**************************************************************************)
let pervGoal : bool ref = ref false
let isPervGoal () = !pervGoal
let setPervGoal flag = pervGoal := flag

(**************************************************************************)
(* global variable lists                                                  *)
(**************************************************************************)
(* is a member of the given list? *)
let rec isMember data dataList =
  match dataList with
	[] -> false
  | (data' :: rest) -> 
	  if (data == data') then true
	  else isMember data rest
		  
(* add new element to a global variable list                  *)
let gListAdd var varList     = varList := (var :: !varList)
(* append a list of vars to the end of a global variable list *)
let gListAppend vars varList = varList := (!varList @ vars)

(*********************************************************************)
(* Term variables that are explicitly quantified in the body of the  *)
(* current clause.                                                   *)
(*********************************************************************)
let qVars : Absyn.avar list ref = ref []
let isQVar   var      = isMember var (!qVars)
let addQVars var      = gListAdd var qVars
let getQVars ()       = !qVars
let setQVars dataList = qVars := dataList		  

(*********************************************************************)
(* Term variables that are explicitly quantified at the head of the  *)
(* current clause.                                                   *)
(*********************************************************************)
let hqVars : Absyn.avar list ref = ref []
let isHQVar   var      = isMember var (!hqVars)
let getHQVars ()       = !hqVars
let setHQVars dataList = hqVars := dataList

(*********************************************************************)
(* Term variables that are explicitly quantified at the entire clause*)
(* context.                                                          *)
(*********************************************************************)
let expqVars : Absyn.avar list ref = ref []
let isExpQVar   var      = isMember var (!expqVars)
let addExpQVars var      = gListAdd var expqVars
let appExpQVars dataList = gListAppend dataList expqVars
let getExpQVars ()       = !expqVars
let setExpQVars dataList = expqVars := dataList


(*********************************************************************)
(* Term and type variables that appear in a clause                   *)
(*********************************************************************)
type termtypevar =
	TermVar of Absyn.avar
  | TypeVar of Absyn.atypevar

let clVars: termtypevar list ref = ref []

let addClTypeVar tyVarData = clVars := TypeVar(tyVarData)::(!clVars)
let addClTermVar tmVarData = clVars := TermVar(tmVarData) :: (!clVars)
let setClVars    dataList  = clVars := dataList
let getClVars    ()        = !clVars

(*****************************************************************************)
(* Auxiliary functions for processing terms and types                        *)
(*****************************************************************************)
let isCrossGoal gNumber = not (gNumber = getGoalNum ())

(** *********************************************************************** **)
(**                          PROCESS TYPES                                  **)
(** *********************************************************************** **)
(*******************************************************************)
(* check whether it is the first occurrence of a type variable, in *)
(* which case, the first goal number field contains a zero         *)
(*******************************************************************)
let firstEncounteredTypeVar var = Absyn.getTypeVariableDataFirstGoal var = 0


(*******************************************************************)
(* fill in fields for a type variable data when it is first        *)
(* encountered and collect the variable data into the clause       *)
(* variable list                                                   *)
(*******************************************************************)
let initTypeVarData varData perm safety heapvar usage =
  match varData with
	Absyn.TypeVar(firstuse, lastuse, lperm, lsafety, lheapvar, _, firstgoal, 
				  lastgoal) ->
					let goalNum = getGoalNum () in
					firstuse  := usage;
					lastuse   := usage;
					lperm     := perm && not (safety && heapvar);
					lsafety   := safety;
					firstgoal := goalNum;
					lastgoal  := goalNum;
					
					addClTypeVar varData

(********************************************************************)
(* update relevant fields for a type variable data for its subsquent*)
(* occurrences                                                      *)
(********************************************************************)
let updateTypeVarData varData perm usage =
  match varData with
	Absyn.TypeVar(_, lastuse, lperm, _, _, _, firstgoal, lastgoal) ->
	  lastuse  := usage;
	  lperm    := (perm || isCrossGoal (!firstgoal));
	  lastgoal := getGoalNum ()
		  
  
(**********************************************************************)
(* processTypeVar:                                                    *)
(* Annotating a type variable data (and its type variable occurrence).*)
(* The same kind of (parameterized action can be used in both analysis*)
(* and synthesis modes:                                               *)
(* If the variable is first encountered, the variable data is         *)
(* initialized according to the given parameters and the current goal *)
(* number; the variable occurrrence is marked at first occurrence;    *)
(* and the variable data is collected into the type/term variable     *)
(* list of the clause.                                                *)
(* Otherwise, the variable data's perm, lastuse and lastgoal fields   *)
(* are updated; the variable occurrence is marked as subsequent       *)
(* occurrence.                                                        *)
(**********************************************************************) 
let processTypeVar varData firstRef tyVarOcc perm safety heapvar =
  if firstEncounteredTypeVar varData then
	(initTypeVarData varData perm safety heapvar (Some tyVarOcc);
	 firstRef := Some(true))
  else (* subsequent occurrence *)
	(updateTypeVarData varData (Absyn.getTypeVariableDataPerm varData) 
	   (Some tyVarOcc);
	 firstRef := Some(false))
				 
(** **************************************************************** **)
(**      PROCESSING TYPE IN ANALYSIS MODE                            **)
(** **************************************************************** **)
(**********************************************************************)
(* Annotating a list of argument types of a term constant or a clause *)
(* head. In the latter case, the heapvar argument is FALSE, since the *)
(* head type environment arrives in registers.                        *)
(* Following the analysis processing order, the processing of embedded*)
(* structures (type application and type arrow) is delayed.           *)
(**********************************************************************)
let rec analyseTypeArgs tylist heapvar =
  let rec analyseTypeArgsAux tylist delayed =
	match tylist with
	  [] -> analyseTypes (List.rev delayed)
	| (ty :: rest) ->
		match ty with
		  Absyn.TypeVarType(_) -> 
			processTypeVar (Absyn.getTypeFreeVariableVariableData ty) 
			  (Absyn.getTypeFreeVariableFirstRef ty) ty false true heapvar;
			analyseTypeArgsAux rest delayed 
		| _ -> analyseTypeArgsAux rest (ty :: delayed)
  in
  analyseTypeArgsAux tylist []
			  		
(**********************************************************************)
(* Anotating a list of types in analysis mode delayed from processing *)
(* type args.                                                         *)
(**********************************************************************)
and analyseTypes tylist =
  match tylist with
	[] -> ()
  | (ty :: rest) ->
	  (match ty with
		Absyn.ArrowType(arg, target) ->	
		  analyseTypeArgs [arg ; target] true
	  | Absyn.ApplicationType(_, args) ->
		  analyseTypeArgs args true
	  | _ -> Errormsg.impossible Errormsg.none "analyseTypes: invalid type");
	  analyseTypes rest 
		
(** **************************************************************** **)
(**      PROCESSING TYPE IN SYNTHESIS MODE                           **)
(** **************************************************************** **)
(**********************************************************************)
(* Annotating a list of argument types of a term constant in          *)
(* synthesis mode: the processing of variables are delayed after all  *)
(* the embedded structures (type application and type arrow).         *)
(**********************************************************************)
let rec synthesizeTypeArgs tylist =
  let rec synthesizeTypeArgsAux tylist delayed =
	match tylist with
	  [] -> (* processing delayed type variables *)
		let rec synthesizeTypeVars typeVars =
		  match typeVars with
			[] -> ()
		  | (typeVar :: rest) ->
			  processTypeVar (Absyn.getTypeFreeVariableVariableData typeVar)
				(Absyn.getTypeFreeVariableFirstRef typeVar) typeVar 
				false true true;
			  synthesizeTypeVars rest
		in
		synthesizeTypeVars (List.rev delayed)
	| (ty :: rest) -> 
		match ty with
		  Absyn.TypeVarType(_) -> synthesizeTypeArgsAux rest (ty :: delayed)
		| _ -> 
			synthesizeType ty false;
			synthesizeTypeArgsAux rest delayed
  in
  synthesizeTypeArgsAux tylist []
				
(**********************************************************************)
(* Annotating a type in synthesis mode: the incoming type could be one*) 
(* as a type argument of a term constant (invoked from                *)
(* synthesizeTypeArgs) or one as a type argument of a goal head       *)
(* (invoked from synthesizeTypes).                                    *)
(**********************************************************************)
and synthesizeType ty perm =
  match ty with 
	Absyn.TypeVarType(_)           ->
	  processTypeVar (Absyn.getTypeFreeVariableVariableData ty) 
		(Absyn.getTypeFreeVariableFirstRef ty) ty perm false false
  | Absyn.ArrowType(arg, target)   -> synthesizeTypeArgs [arg ; target]
  | Absyn.ApplicationType(_, args) -> synthesizeTypeArgs args
  | _ -> Errormsg.impossible Errormsg.none "synthesizeType: invalid type exp"

(***********************************************************************)
(* Annotating a list of argument types of a goal head in synthesis mode*)
(***********************************************************************)
let rec synthesizeTypes tylist perm =
  match tylist with
	[] -> ()
  | (ty :: rest) -> 
	  (synthesizeType ty perm; synthesizeTypes rest perm)

(** *********************************************************************** **)
(**                          PROCESS TERMS                                  **)
(** *********************************************************************** **)
(*******************************************************************)
(* check whether it is the first occurrence of a type variable, in *)
(* which case, the first goal number field contains a zero         *)
(*******************************************************************)
let firstEncounteredVar var =
  match var with
	Absyn.Var(_, _, _, _, _, firstgoal, _, _) -> (!firstgoal) = 0

(*******************************************************************)
(* fill in fields for a variable data when it is first encountered *)
(* and collect the variable data into the clause variable list     *)
(*******************************************************************)
let initVarData varData oneuse perm safety heapvar varOcc =
  match varData with
	Absyn.Var(loneuse, lperm, lsafety, lheapvar, _, firstgoal, lastgoal,
			  lastuse) ->
				let goalNum = getGoalNum () in
				loneuse := oneuse;
				lperm := perm && (not (safety && heapvar));
				lsafety := safety;
				lheapvar := heapvar;
				firstgoal := goalNum;
				lastgoal := goalNum;
				lastuse := varOcc;
				
				addClTermVar varData

(********************************************************************)
(* update relevant fields for a variable data for its subsquent     *)
(* occurrences                                                      *)
(********************************************************************)
let updateVarData varData perm usage =
  match varData with
	Absyn.Var(oneuse, lperm, _, _, _, firstgoal, lastgoal, lastuse) ->
	  oneuse   := Some(false);
	  lperm    := (perm || isCrossGoal (!firstgoal));
	  lastgoal := getGoalNum ();
	  lastuse  := usage		  

(** **************************************************************** **)
(**      PROCESSING TERM IN SYNTHESIS MODE                           **)
(** **************************************************************** **)
(**************************************************************************)
(* Annotating a list of terms in synthesis mode. The list of terms could  *)
(* be the arguments of an atomic goal or those delayed in the synthesis   *)
(* processing.                                                            *)
(**************************************************************************)
let rec synthesizeTerms termList perm heapvar =
  match termList with
	[] -> ()
  | (term :: rest) -> 
	  (synthesizeTerm term perm heapvar; synthesizeTerms rest perm heapvar)
		 
(**************************************************************************)
(* Annotating a term in synthesis mode.                                   *)
(**************************************************************************)
and synthesizeTerm term perm heapvar =
  match term with 
	Absyn.ConstantTerm(_, tyenv, _) ->
	  synthesizeTypeArgs tyenv
  | Absyn.FreeVarTerm(freeVarInfo, _) ->
	  synthesizeFreeVarTerm term perm heapvar
  | Absyn.AbstractionTerm(abstInfo, _) ->
	  synthesizeAbstractionTerm abstInfo
  | Absyn.ApplicationTerm(appInfo, _) ->
	  synthesizeApplicationTerm appInfo 
  | _ -> ()

(* Annotating an abstraction term *)
and synthesizeAbstractionTerm abstInfo =
  match abstInfo with 
	Absyn.UNestedAbstraction(_, _, abstBody) -> 
	  synthesizeTerm abstBody false false
  | _ -> Errormsg.impossible Errormsg.none 
		"synthesizeAbstractionTerm: invalid abstraction rep"

(* Annotating an application term in synthesis mode *)
and synthesizeApplicationTerm appInfo =
  (* Annotating a list of term arguments of an application in synthesis mode:*)
  (* the processing of (free) variables and constants are delayed after all  *)
  (* the embedded structures (application, abstraction)                      *)
  let rec synthesizeAppArgs termList delayed =
	match termList with
	  [] -> synthesizeTerms (List.rev delayed) false true 
	| (term :: rest) -> 
		match term with
		  Absyn.ConstantTerm(_) ->
			synthesizeAppArgs rest (term :: delayed)
		| Absyn.FreeVarTerm(_) -> 
			synthesizeAppArgs rest (term :: delayed)
		| Absyn.AbstractionTerm(abstInfo, _) ->
			synthesizeAbstractionTerm abstInfo;
			synthesizeAppArgs rest delayed
		| Absyn.ApplicationTerm(appInfo, _) ->
			synthesizeApplicationTerm appInfo;
			synthesizeAppArgs rest delayed 
		| _ -> synthesizeAppArgs rest delayed
  in
  
  (* function body of synthesizeApplicationTerm *)
  match appInfo with
	Absyn.FirstOrderApplication(head, args, _) ->
	  (synthesizeTerm head false false; (* const/fv/bv *)
	   synthesizeAppArgs args [])
  | _ -> Errormsg.impossible Errormsg.none 
		"synthesizeApplicationTerm: invalid application rep"

(* Annotating a variable encountered in synthesis processing.            *)
(* The variable could be one explicitly bound in the body of the clause  *)
(* currently being processed, one explicitly bound at the head of the    *)
(* clause currently being processed,  one implicitly bound at the head   *)
(* of the top-level clause, or one bound in the embedding context of an  *)
(* embedded clause.                                                      *)
and synthesizeFreeVarTerm var perm heapvar =
  let varData = Absyn.getTermFreeVariableVariableData var in
  if (firstEncounteredVar varData) then
	(let (myfirst, myperm, mysafety) =
	  if (isHQVar varData || not (isEmbedded ())) then
		(true, perm && (not heapvar), heapvar)
	  else (* embedded && not a hq var && not a qvar: quantified at*)
           (* the enclosing context for an embedded clause *)
		if (isExpQVar varData) then (*explicitly quant*)
		  (false, perm && (not heapvar), true)
		else (*implicitly quant at the head of top-level clause*)
		  (false, perm, true)
	in
	initVarData varData (Some myfirst) myperm mysafety heapvar (Some var);
	Absyn.setTermFreeVariableFirst var myfirst)
	
  else (* subsequent occurrence *)
    (let oldPerm = Absyn.getVariableDataPerm varData in
    let newPerm = 
      if (isQVar varData) then (*explicitly body quant*)
	(oldPerm || (isCrossGoal (Absyn.getVariableDataFirstGoal varData)) ||
	perm)
      else (oldPerm || (isCrossGoal (Absyn.getVariableDataFirstGoal varData)))
    in
    updateVarData varData newPerm (Some var);
    Absyn.setTermFreeVariableFirst var false)
  
		
(** **************************************************************** **)
(**      PROCESSING TERM IN ANALYSIS MODE                            **)
(** **************************************************************** **)	
(*************************************************************************)
(* Annotating a list of term arguments which might be those of a head of *)
(* a clause or those of a first-order application in analysis mode.      *)
(* The value of heapargs indicates which; clause head args arrive in     *)
(* registers.                                                            *)
(* Following the analysis processing order, the processing of embedded   *)
(* compound terms is delayed.                                            *)	
(*************************************************************************)
let rec analyseAppArgs terms heaparg =
  let rec analyseAppArgsAux terms delayed =
	match terms with
	  [] -> analyseTerms (List.rev delayed)
	| (term :: rest) ->
	  match term with
		Absyn.ConstantTerm(_, tyenv, _) ->
		  analyseTypeArgs tyenv true;
		  analyseAppArgsAux  rest delayed
	  | Absyn.FreeVarTerm(_, _) ->
		  analyseFreeVarTerm term heaparg;
		  analyseAppArgsAux rest delayed 
	  | Absyn.AbstractionTerm(_) ->
		  analyseAppArgsAux rest (term :: delayed) 
	  | Absyn.ApplicationTerm(_) ->
		  analyseAppArgsAux rest (term :: delayed) 
	  | _ -> analyseAppArgsAux rest delayed 
  in
  analyseAppArgsAux terms []

(***********************************************************************)
(* Annotating a list of terms delayed from processing application args *)
(* Note that a delayed term can only be an abstraction or an           *)
(* application.                                                        *)
(***********************************************************************)
and analyseTerms terms =
  match terms with
	[] -> ()
  | (term :: rest) ->
	  (match term with
		Absyn.AbstractionTerm(abstInfo, _) -> 
		  synthesizeAbstractionTerm abstInfo
	  | Absyn.ApplicationTerm(applInfo, _) -> 
		  analyseApplicationTerm applInfo
	  | _ -> Errormsg.impossible Errormsg.none "analyseTerms: invalid term");
	  analyseTerms rest
			
		
(* Annotating an application term in analysis mode. The head is examined *)
(* first to determine whether to process in analysis or synthesis mode.  *)
(* If the mode is analysis, the type environment of the head constant is *)
(* processed and then the arguments of the term.                         *)
and analyseApplicationTerm applInfo =
  match applInfo with
	Absyn.FirstOrderApplication(
	  Absyn.ConstantTerm(_, tyenv, _), args, _) ->
		analyseTypeArgs tyenv true;
		analyseAppArgs args true
  | _ -> synthesizeApplicationTerm applInfo 			


(* Annotating a free variable encountered in analysis processing.        *)
(* This variable could be bound in the following three situations:       *)
(* 1). explicitly head quantified                                        *)
(* 2). implicitly head quantified if the clause is a top-level one       *)
(* 3). quantified at the embedding context if the clause is an embedded  *)
(*     one.                                                              *)
(* If the variable is not encountered yet, its attributes are assgined   *)
(* according to the parameters and the current goal number (which must   *)
(* be one.) And the variable data is collected into the term/type        *)
(* variable list of the clause. Note the variable occurrence (and the    *)
(* oneuse attribute of the variable data) is viewed as first only        *)
(* when the clause is a top-level one.                                   *)
(* If the variable has been encountered, the oneuse and lastuse          *)
(* attributes are updated to false and the current term occurrence       *)
(* respectively. (lastgoal and perm remains the same since the current   *)
(* goal number must be one.)                                             *)
and analyseFreeVarTerm var heapvar = 
  let varData = Absyn.getTermFreeVariableVariableData var in
  if (firstEncounteredVar varData) then
	let first = (isHQVar varData) || not (isEmbedded ()) in
	initVarData varData (Some first) false true heapvar (Some var);
	Absyn.setTermFreeVariableFirst var first
  else
	(Absyn.setVariableDataOneUse varData false;
	 Absyn.setVariableDataLastUse varData var;
	 Absyn.setTermFreeVariableFirst var false)

(** *********************************************************************** **)
(**                          PROCESS CLAUSES                                **)
(** *********************************************************************** **)

(**************************************************************************)
(* Assigning environment slots to permanent variables. For environment    *)
(* trimming, the longest-lived variable should be assigned the lowest slot*)
(* Lifetime info is available from the lastgoal field.                    *)
(* The idea is to first collect permanent variables into the ith "bucket" *)
(* where i is the last goal number of a permanent variable. Then the      *)
(* offsets of permanent variables are assigned starting from the last     *)
(* bucket. Note that last goal optimization requires the list for the     *)
(* last goal in the body to be merged with that for the penultimate one.  *)
(* In addition to offset assignment, a list of goal numbers with number   *)
(* of `live' permanent variables at these goals is calculated and left    *)
(* with the clause record.                                                *)
(**************************************************************************)
let assignPermVars goalEnvAssoc notrim =
  (* number of permanent variable "buckets" *)
  let tmpGoalNum = getGoalNum () in
  let goalnumber = if (notrim) then tmpGoalNum - 1 else tmpGoalNum - 2 in 
  (* create an array of (termtypevar) lists for the buckets *)
  let buckets = (Array.make goalnumber []) in
  
  (* Auxiliary function for collecting permanent variables into           *)
  (* corresponding bucket depending their last goal number                *)
  let rec collectPermVars clauseVars =
	(* enter a variable into a bucket (to the front of the var list) *)
	let enterVar clauseVar lastgoal =
	  let index = 
		if (not(lastgoal = tmpGoalNum - 1) || notrim) then lastgoal - 1
	    else lastgoal - 2
	  in
	  let bucket = (clauseVar :: (Array.get buckets index)) in
	  Array.set buckets index bucket
	in
	match clauseVars with
	  [] -> ()
	| (clauseVar :: rest) ->
		match clauseVar with
		  TermVar(varData) ->
			if (Absyn.getVariableDataPerm varData) then
			  (enterVar clauseVar (Absyn.getVariableDataLastGoal varData);
			   collectPermVars rest)
			else collectPermVars rest
		| TypeVar(varData) ->
			if (Absyn.getTypeVariableDataPerm varData) then
			  (enterVar clauseVar (Absyn.getTypeVariableDataLastGoal varData);
			   collectPermVars rest)
			else collectPermVars rest
  in  

  (* assign slots for permanent variables collected in the buckets, starting *)
  (* from the last one.                                                      *)
  let rec assignSlots index bucketInd =
	(* assign slots for vars in one bucket *)
	let rec assignOneBucket bucket ind =
	  match bucket with
		[] -> ind
	  | (TermVar(varData) :: rest) ->
		  Absyn.setVariableDataOffset varData ind;
		  assignOneBucket rest (ind + 1)
	  | (TypeVar(varData) :: rest) ->
		  Absyn.setTypeVariableDataOffset varData ind;
		  assignOneBucket rest (ind + 1)
	in
	if (bucketInd >= 0) then 
	  let maxInd = assignOneBucket (Array.get buckets bucketInd) index in
	  ((bucketInd + 1, maxInd - 1) :: (assignSlots maxInd (bucketInd - 1)))
	else []
  in
  
  (* function body of assignPermVars *)
  let gespList = 
	if (goalnumber > 0) then 
	  (collectPermVars (getClVars ());
	   assignSlots 1 (goalnumber - 1))
    else []
  in
  goalEnvAssoc := Absyn.GoalEnvAssoc(List.rev gespList)


(*****************************************************************************)
(*                         PROCESS A CLAUSE                                  *)
(*****************************************************************************)
let rec processClause clause perm =
  (* collect explicitly head quantified variables into hqVars and expQVars *)
  (* The order of this variables in the global lists does not matter.      *)
  let collectHQVars varList =
	setHQVars varList; 
	appExpQVars varList
  in

  (* process a clause head: annotate the type arguments and term arguments *)
  (* in analysis mode                                                      *)
  let processClauseHead args tyArgs =  
	analyseTypeArgs tyArgs false;
	analyseAppArgs args false  
  in
  
  (* function body of processClause *)
  match clause with
	Absyn.Fact(_, args, tyargs, _, _, _, _, expHQVars, _, impMods) ->
	  collectHQVars expHQVars;         
	  processClauseHead args tyargs  (* annotate clause (type) args *) 
  | Absyn.Rule(_, args, tyargs, _, _, _, _, expHQVars, _, goal, 
			   goalEnvAssoc, cutVarRef, hasenv, impmods) ->
	  collectHQVars expHQVars;
	  processClauseHead args tyargs; (* annotate clause (type) args *)
	  let perm' = (not (impmods = []) && perm) in   (* process goal           *)
	  let (myhasenv, notrim) = processGoal goal perm' true cutVarRef in
	  hasenv := myhasenv || notrim || perm'; (* decide whether has environment*)
	  assignPermVars goalEnvAssoc (notrim || perm') (* decide perm var offset *)

(*****************************************************************************)
(*                         PROCESS A GOAL                                    *)
(*****************************************************************************)
and processGoal goal perm last cutVarRef = 
  match goal with
	Absyn.AtomicGoal(pred, _, _, args, tyargs) ->
	  processAtomicGoal pred args tyargs perm last cutVarRef 
  | Absyn.AndGoal(andl, andr) ->
	  let (hasenv1, _)  = processGoal andl true false cutVarRef  in
	  let (hasenv2, notrim) = processGoal andr perm last cutVarRef in
	  (hasenv1 || hasenv2, notrim)
  | Absyn.SomeGoal(varData, body) ->
	  processSomeGoal varData body perm last cutVarRef 
  | Absyn.AllGoal(Absyn.HCVarAssocs(hcVarAssoc), body) ->
	  (processAllGoal hcVarAssoc body last cutVarRef, true) 
  | Absyn.ImpGoal(Absyn.Definitions(clDefs), _, _, body) ->
	  (processImpGoal clDefs body last cutVarRef, true)
  | _ -> Errormsg.impossible Errormsg.none "processGoal: invalid CutFailGoal"

(**************************************************************************)
(* processAtomicGoal:                                                     *)
(* a). annotate type arguments and term arguments of the goal in synthesis*)
(*     mode;                                                              *)
(* b). check whether the goal is deep cut, and set the cutVarRef field for*)
(*     the enclosing clause if it is the situation;                       *)
(* c). calculate hasenv and notrim and set goalNum and PervGoal           *)
(**************************************************************************)
and processAtomicGoal pred args tyargs perm last cutVarRef =
  (* process a cut goal:                                                *)
  (* 1.update the cutVarRef field of the enclosing clause if a deepcut; *)
  (* 2.hasenv is set to false;                                          *)
  (* 3.notrim is set to last && (isPervGoal ())                         *)  
  let processCutGoal () =
	let myGoalNum = getGoalNum () in
	(* manage cutvar *)
	(if (myGoalNum > 1) then (* deep cut *)
	  match (!cutVarRef) with
		None -> 
		  let cutvarData = Absyn.makeCutVariableData myGoalNum in
		  cutVarRef := Some(cutvarData);
		  addClTermVar (cutvarData) (* add to clause variable list *)
	  | Some(cutvarData) -> Absyn.setVariableDataLastGoal cutvarData myGoalNum
	else ());
	(* increate goal number if necessary *)
	(if last then incGoalNum ()
	else ());
	(* decide hasenv and notrim *)
	(false, last && (isPervGoal ())) 
  in
  
  (* calculate hasenv, notrim and set goal number of pervGoal *)
  let hasenvAndNotrim () =
	let notrim = 
	  if (not(Pervasive.isPerv(pred)) || Pervasive.regClobberingPerv(pred) ||
	      Pervasive.backtrackablePerv(pred)) 
	  then
		let myisPervGoal = isPervGoal () in
		incGoalNum ();
		setPervGoal false;
		(last && myisPervGoal)
	  else (*perv goal not damanage reg contents directly or indirectly*) 
		if (last) then (incGoalNum (); isPervGoal ())
		else (setPervGoal true; false)
	in
	(not last, notrim)
  in

  (* function body of processAtomicGoal *)
  if (Pervasive.iscutConstant pred) then processCutGoal ()
  else (* other than cut goal*)
	(synthesizeTypes tyargs perm;     (*process type args in synthesis mode *)
	 synthesizeTerms args perm false; (*process term args in synthesis mode *)
	 hasenvAndNotrim ())		
		
(************************************************************************)
(* process a some goal: initiate the existentially quantitied variable  *)
(* data and collect it into variable list of the clause, qVars and      *)
(* expQVars.                                                            *)
(************************************************************************)
and processSomeGoal varData body perm last cutVarRef =
  initVarData varData None perm false false None;
  addQVars varData;
  addExpQVars varData;
  processGoal body perm last cutVarRef

(************************************************************************)
(* process an all goal: initiate the list of the universally quantified *)
(* variable data and collect them into variable list of the clause,     *)
(* qVars and expQVars.                                                  *)
(************************************************************************)
and processAllGoal hcVarAssoc body last cutvar =
  (* init universally quantified vars and collect them into lists *)
  let rec collectUnivVars hcVarPairs =
	match hcVarPairs with
	  [] -> ()
	| ((varData, _)::rest) -> 
		(initVarData varData None true false false None;
		 addQVars varData;
		 addExpQVars varData;
		 collectUnivVars rest)
  in  
  
  collectUnivVars hcVarAssoc; 
  let (hasenv, _) = processGoal body true last cutvar in
  hasenv

(************************************************************************)
(* process an implication goal:                                         *)
(* a). process the embedded clauses;                                    *)
(* b). process the body goal;                                           *)
(* c). process (type) variable mapping, update their last goal, and set *)
(*     those from variable data that are already processed to permanent;*)
(*     initialize those are not.                                        *)
(************************************************************************)
and processImpGoal clDefs body last cutVarRef =
  let rec processImpDefs clDefs fvs tyfvs =
	match clDefs with
	  [] -> (fvs, tyfvs)
	| ((_, (cls, _, _, _)) :: rest) ->
		let rec processEmbeddedClauses cls fvs tyfvs =
		  match cls with 
			[] -> (fvs, tyfvs)
		  | (cl :: rest) ->
			  let (fvs', tyfvs') = processEmbeddedClause cl fvs tyfvs in
			  processEmbeddedClauses rest fvs' tyfvs' 
		in
		let (fvs', tyfvs') = processEmbeddedClauses (!cls) fvs tyfvs in
		processImpDefs rest fvs' tyfvs'	  
  in

  let updateLastGoal fvs tyfvs newLastGoalNum =
	let rec updateLastGoalFvs fvs =
	  match fvs with
		[] -> ()
	  | (varData :: rest) -> 
		  Absyn.setVariableDataLastGoal varData newLastGoalNum;
		  updateLastGoalFvs rest
	in
	let rec updateLastGoalTyFvs tyfvs =
	  match tyfvs with
		[] -> ()
	  | (varData :: rest) ->
		  Absyn.setTypeVariableDataLastGoal varData newLastGoalNum;
		  updateLastGoalTyFvs rest
	in
	updateLastGoalFvs fvs;
	updateLastGoalTyFvs tyfvs
  in
  
  (* function body of processImpGoal *)
  let myisPervGoal = isPervGoal () in
  (* process embedded definitions (and their variable mapping) *)
  let (fvs, tyfvs) = processImpDefs clDefs [] [] in
  (* process implication body *)
  setPervGoal myisPervGoal;
  let (hasenv, _) = processGoal body true last cutVarRef in
  let myGoalNum   = getGoalNum () in
  updateLastGoal fvs tyfvs (if last then myGoalNum - 1 else myGoalNum);
  hasenv
	
(***********************************************************************)
(* process an embedded clause and its term/type variable mapping       *)
(***********************************************************************)  
and processEmbeddedClause clause fvs tyfvs =
  (* process the embedded clause *)
  let (lqVars, lhqVars, _, lclVars, lembedded, lgoalNum) =
	(getQVars (), getHQVars (), getExpQVars (), getClVars (), 
	 isEmbedded (), getGoalNum ())
  in  
  setQVars []; setHQVars []; setClVars []; setEmbedded true; setGoalNum 1;
  processClause clause false;
  (* process (type) variable mapping lists *) 
  setQVars lqVars; setHQVars lhqVars; setClVars lclVars; 
  setEmbedded lembedded; setGoalNum lgoalNum;
  processVarMaps (Absyn.getClauseTermVarMaps clause) 
	(Absyn.getClauseTypeVarMaps clause) fvs tyfvs

(**********************************************************************)
(* set from variable data in term/type variable mappings              *)
(**********************************************************************)
and processVarMaps tmVarMaps tyVarMaps fvs tyfvs =
  
  let rec processTmVarMaps tmVars fvs =
	match tmVars with
	  [] -> fvs
	| (* ((fromVarData, _) :: rest) -> *)
	  ((fromVarData, toVarData) :: rest) ->
		if firstEncounteredVar fromVarData then (* first encountered *)
		  initVarData fromVarData None true false false None
		else Absyn.setVariableDataPerm fromVarData true;
		processTmVarMaps rest (fromVarData :: fvs)
  in
  
  let rec processTyVarMaps tyVars tyfvs =
	match tyVars with
	  [] -> tyfvs
	| ((fromVarData, toVarData) :: rest) ->
		(* treat fromVarData *)
		(if firstEncounteredTypeVar fromVarData then (* first encountered *)
		  initTypeVarData fromVarData true false false None
		else 
          Absyn.setTypeVariableDataPerm fromVarData true);
          (* treat toVarData *)
          Absyn.setTypeVariableDataSafety toVarData true;
          Absyn.setTypeVariableDataHeapVar toVarData true;
          let firstuse = Absyn.getTypeVariableDataFirstUseOpt toVarData in
            (if (Option.isSome firstuse) then
               Absyn.setTypeFreeVariableFirst (Option.get firstuse) false
             else ());
            (* process others *)
            processTyVarMaps rest (fromVarData :: tyfvs)
  in
  
  let (Absyn.TermVarMap(tmVars)) = tmVarMaps in
  let (Absyn.TypeVarMap(tyVars)) = tyVarMaps in
  let fvs' = processTmVarMaps tmVars fvs in
  let tyfvs' = processTyVarMaps tyVars tyfvs in
    (fvs', tyfvs')

(*****************************************************************************)
(*                   PROCESS TOP LEVEL DEFINITIONS                           *)
(*****************************************************************************)
let rec processTopLevelDefs clDefs =
  match clDefs with
	[] -> []
  | ((p, clausesBlock) :: rest) ->
	  (* process each clause contained in the def block *)
	  let rec processDef cls = 
		match cls with
		  [] -> ()
		| (cl :: restCls) -> 
			setQVars []; setHQVars []; setExpQVars []; setClVars []; 
			setEmbedded false; setGoalNum 1; setPervGoal false;
			processClause cl true;
			processDef restCls
	  in
	  processDef (Absyn.getClauseBlockClauses clausesBlock);
	  (clausesBlock :: (processTopLevelDefs rest))
			
(** *********************************************************************** **)
(**                          INTERFACE FUNCTION                             **)
(** *********************************************************************** **)
let processClauses amod =
  match (Absyn.getModuleClauses amod) with
	  Absyn.PreClauseBlocks(Absyn.Definitions(clauseDefs)) ->
	    let () = Errormsg.log Errormsg.none
	      "Annvariables.processClauses: processing clauses..." in
	    let result =
	      Absyn.setModuleClauses amod
	        (Absyn.ClauseBlocks(processTopLevelDefs clauseDefs)) in
      let () = Errormsg.log Errormsg.none
        "Annvariables.processClauses: processed clauses" in
      result
  | _ -> Errormsg.impossible Errormsg.none "processClauses: invalid clause rep"
