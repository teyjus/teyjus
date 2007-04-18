(****************************************************************************)
(* process clause representations in a module, annotate type and term       *)
(* variables as temporary or permanent, and decide offset for permanent     *)
(* variables. Fill in variable related information for clause               *)
(* representations: hasenv, cutvar and goal-environment size association.   *)
(****************************************************************************)

(****************************************************************************)
(* GLOBAL FLAGS AND LISTS AND ACCESS FUNCTIONS                              *)
(****************************************************************************)
(* whether in an embedded context *)
let embedded : bool ref = ref false
let isEmbedded () = !embedded
let setEmbedded flag = embedded := flag

(* the goal number of the goal currently being processed *)
let goalNumber : int ref = ref 1
let getGoalNum ()     = !goalNumber
let incGoalNum ()     = goalNumber := !goalNumber + 1
let setGoalNum number = goalNumber := number 

(* whether the previous goal being processed is a non-register-clobbering *)
(* pervasive                                                              *)
let pervGoal : bool ref = ref false
let isPervGoal () = !pervGoal
let setPervGoal flag = pervGoal := flag


(* global variable lists *)

(* is a member of the given list? *)
let rec isMember data dataList =
  match dataList with
	[] -> false
  | (data' :: rest) -> 
	  if (data == data') then true
	  else isMember data rest
		  
(* add new element to a global variable list *)
let gListAdd var varList = varList := (var :: !varList)

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
let addHQVars var      = gListAdd var hqVars
let getHQVars ()       = !hqVars
let setHQVars dataList = hqVars := dataList

(*********************************************************************)
(* Term variables that are explicitly quantified at the entire clause*)
(* context.                                                          *)
(*********************************************************************)
let expqVars : Absyn.avar list ref = ref []
let isExpQVar   var      = isMember var (!expqVars)
let addExpQVars var      = gListAdd var expqVars
let getExpQVars ()       = !expqVars
let setExpQVars dataList = expqVars := dataList

(*********************************************************************)
(* Term and type variables that appear in a clause                   *)
(*********************************************************************)
type termtypevar =
	TermVar of Absyn.avar
  | TypeVar of Absyn.atypevar

let clVars: termtypevar list ref = ref []

let addClTypeVar tyVarOpt  = clVars := TypeVar(Option.get tyVarOpt) :: (!clVars)
let addClTermVar tmVarData = clVars := TermVar(tmVarData) :: (!clVars)
let setClVars    dataList  = clVars := dataList
let getClVars    ()        = !clVars

(****************************************************************************)
(*                       PROCESS TERMS AND TYPES                            *)
(****************************************************************************)
let isCrossGoal gnumber = not(gnumber = getGoalNum ())
let firstEncountered firstgoal = ((!firstgoal) = 0)

(*************************************************************************)
(*                          PROCESS TYPES                                *)
(*************************************************************************)
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
let processTypeVar tyVar first tyVarOcc perm safety heapvar =
  let goalNum = getGoalNum () in
  match (!tyVar) with
	Some(Absyn.TypeVar(firstuse, lastuse, lperm, lsafety, lheapvar,
					   _, firstgoal, lastgoal)) ->
	  if (firstEncountered firstgoal) then (*first encountered *)
		(firstuse := Some(tyVarOcc); (*type variable data*) 
		 lastuse := Some(tyVarOcc);
		 lperm := perm && not(safety && heapvar);
		 lsafety := safety;
		 lheapvar := heapvar;
		 firstgoal := goalNum;
		 lastgoal  := goalNum;
		 
		 first := Some(true);        (*type variable occurrence*)
		 addClTypeVar (!tyVar))
	  else (*subsequent occurrence *)
		let myperm = !lperm in
		lastuse := Some(tyVarOcc);  (* type variable data *)
		lperm := (myperm || isCrossGoal (!firstgoal));
		lastgoal := goalNum;
		
		first := Some(false)        (* type variable occurrence *) 
  | _ -> Errormsg.impossible Errormsg.none 
		"processTypeVar: uninitialized type variable"

(***************************************************************************)
(*               PROCESSING TYPES IN ANALYSIS MODE:                        *)
(***************************************************************************)
(**************************************************************************)
(* Annotating a list of types in analysis mode delayed from processing    *)
(* type args.                                                             *)
(**************************************************************************)
let rec aProcessTypes tylist =
  match tylist with
	[] -> ()
  | (ty :: rest) ->
	  match ty with
		Absyn.ArrowType(arg, target) ->
		  aProcessTypeArgs (arg::target::[]) [] true; 
		  aProcessTypes rest
	  | Absyn.ApplicationType(_, args) ->
		  aProcessTypeArgs args [] true; 
		  aProcessTypes rest
	  | _ -> Errormsg.impossible Errormsg.none "aProcessTypes: invalid type"

(***************************************************************************)
(* Annotating a list of argument types of a term constant or a clause head.*)
(* In the latter case, the heapvar argument is FALSE, since the head type  *)
(* environment arrives in registers.                                       *)
(* Following the analysis processing order, the processing of embedded     *)
(* structures (type application and type arrow) is delayed.                *)
(***************************************************************************)
and aProcessTypeArgs tylist delayed heapvar =
  match tylist with
	[] -> aProcessTypes (List.rev delayed) 
  | (ty :: rest) -> 
	  match ty with
		Absyn.TypeVarType(typeVarInfo) ->
		  (match (!typeVarInfo) with
			Absyn.FreeTypeVar(tyVar, first) ->
			  processTypeVar tyVar first ty false true heapvar; 
			  aProcessTypeArgs rest delayed heapvar
		  | _ -> Errormsg.impossible Errormsg.none 
				"aProcessTypeArgs: invalid type variable rep")
	  | _ -> aProcessTypeArgs rest (ty :: delayed) heapvar (*arrow or app *)

(***************************************************************************)
(*               PROCESSING TYPES IN SYNTHESIS MODE:                       *)
(***************************************************************************)
(**************************************************************************)
(* Annotating a type in synthesis mode: the incoming type could be one as *)
(* a type argument of a term constant (invoked from sProcessTypeArgs) or  *)
(* one as a type argument of a goal head (invoked from sProcessTypes).    *)
(**************************************************************************)
let rec sProcessType ty perm =
  match ty with 
	Absyn.TypeVarType(typeVarInfo) ->
	  (match (!typeVarInfo) with 
		Absyn.FreeTypeVar(tyVar, first) ->
		  processTypeVar tyVar first ty perm false false
	  | _ -> Errormsg.impossible Errormsg.none 
			"sProcessType: invalid type variable rep")
  | Absyn.ArrowType(arg, target) ->
	  sProcessTypeArgs (arg::target::[]) []
  | Absyn.ApplicationType(_, args) ->
	  sProcessTypeArgs args []
  | _ -> Errormsg.impossible Errormsg.none "sProcessType: invalid type exp"

(**************************************************************************)
(* Annotating a list of argument types of a term constant in synthesis    *)
(* mode: the processing of variables are delayed after all the embedded   *)
(* structures (type application and type arrow).                          *)
(**************************************************************************)
and sProcessTypeArgs tylist delayed =
  match tylist with 
	[] -> 
	  (* processing delayed type variables *)
	  let rec sProcessTypeVars typeVars =
		match typeVars with
		  [] -> ()
		| (ty :: rest) ->
			match ty with
			  Absyn.TypeVarType(typeVarInfo) ->
				(match (!typeVarInfo) with
				  Absyn.FreeTypeVar(tyVar, first) ->
					processTypeVar tyVar first ty false true true;
					sProcessTypeVars rest
				| _ -> Errormsg.impossible Errormsg.none 
					  "sProcessTypeArgs: invalid type variable rep")
			| _ -> Errormsg.impossible Errormsg.none
				  "sProcessTypeArgs: non-type-variable is delayed"
	  in
	  sProcessTypeVars (List.rev delayed)

  | (ty :: rest) ->
	  match ty with
		Absyn.TypeVarType(_) ->  sProcessTypeArgs rest (ty :: delayed)
	  | _ -> 
		  sProcessType ty false;
		  sProcessTypeArgs rest delayed

(**************************************************************************)
(* Annotating a list of argument types of a goal head in synthesis mode.  *)
(**************************************************************************)
let rec sProcessTypes tylist perm =
  match tylist with
	[] -> ()
  | (ty :: rest) -> 
	  (sProcessType ty perm; sProcessTypes rest perm)

(*************************************************************************)
(*                          PROCESS TERMS                                *)
(*************************************************************************)
(* fill in fields for a variable data when it is first encountered *)
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


(* update relevant fields of a variable data for its subsequent occurrences *)
let subVarDataUpdate oneuseFd permFd lastgoalFd lastuseFd perm lastuse =
  oneuseFd := Some(false);
  permFd := perm;
  lastgoalFd := getGoalNum ();
  lastuseFd := Some(lastuse)
  
(****************************************************************************)
(*               PROCESS TERMS IN SYNTHESIS MODE                            *)
(****************************************************************************)
(**************************************************************************)
(* Annotating a list of terms in synthesis mode. The list of terms could  *)
(* be the arguments of an atomic goal or those delayed in the synthesis   *)
(* processing.                                                            *)
(**************************************************************************)
let rec sProcessTerms termList perm heapvar =
  match termList with
	[] -> ()
  | (term::rest) -> 
	  (sProcessTerm term perm heapvar; sProcessTerms rest perm heapvar)

(**************************************************************************)
(* Annotating a term in synthesis mode.                                   *)
(**************************************************************************)
and sProcessTerm term perm heapvar =
  match term with 
	Absyn.ConstantTerm(_, tyenv, _, _) ->
	  sProcessTypeArgs tyenv []
  | Absyn.FreeVarTerm(freeVarInfo, _, _) ->
	  sProcessFreeVarTerm freeVarInfo term perm heapvar
  | Absyn.AbstractionTerm(abstInfo, _, _) ->
	  sProcessAbstractionTerm abstInfo
  | Absyn.ApplicationTerm(appInfo, _, _) ->
	  sProcessApplicationTerm appInfo 
  | _ -> ()

(* Annotating an abstraction term *)
and sProcessAbstractionTerm abstInfo =
  match abstInfo with 
	Absyn.UNestedAbstraction(_, _, abstBody) -> 
	  sProcessTerm abstBody false false
  | _ -> Errormsg.impossible Errormsg.none 
		"sProcessAbstractionTerm: invalid abstraction rep"

(* Annotating an application term in synthesis mode *)
and sProcessApplicationTerm appInfo =
  match appInfo with
	Absyn.FirstOrderApplication(head, args, _) ->
	  (sProcessTerm head false false; (* const/fv/bv *)
	   sProcessAppArgs args [])
  | _ -> Errormsg.impossible Errormsg.none 
		"sProcessApplicationTerm: invalid application rep"

(* Annotating a list of term arguments of an application in synthesis mode:*)
(* the processing of (free) variables and constants are delayed after all  *)
(* the embedded structures (application, abstraction)                      *)
and sProcessAppArgs termList delayed =
  match termList with
	[] -> sProcessTerms (List.rev delayed) false true 
  | (term :: rest) -> 
	  match term with
		Absyn.ConstantTerm(_) ->
		  sProcessAppArgs rest (term :: delayed)
	  | Absyn.FreeVarTerm(_) -> 
		  sProcessAppArgs rest (term :: delayed)
	  | Absyn.AbstractionTerm(abstInfo, _, _) ->
		  sProcessAbstractionTerm abstInfo;
		  sProcessAppArgs rest delayed
	  | Absyn.ApplicationTerm(appInfo, _, _) ->
		  sProcessApplicationTerm appInfo;
		  sProcessAppArgs rest delayed 
	  | _ -> sProcessAppArgs rest delayed


(* Annotating a variable encountered in synthesis processing.            *)
(* The variable could be one explicitly bound in the body of the clause  *)
(* currently being processed, one explicitly bound at the head of the    *)
(* clause currently being processed,  one implicitly bound at the head   *)
(* of the top-level clause, or one bound in the embedding context of an  *)
(* embedded clause.                                                      *)
and sProcessFreeVarTerm freeVarInfo term perm heapvar =
  match freeVarInfo with
	Absyn.FreeVar(varData, first) ->
	  (match varData with
		Absyn.Var(oneuse, lperm, lsafety, lheapvar, _, firstgoal, lastgoal,
				  lastuse) ->				 
		  if (firstEncountered firstgoal) then (*first encountered*)
			let (myfirst, myperm, mysafety) =
			  if (isHQVar varData) || not (isEmbedded ()) then
				(true, perm && (not heapvar), heapvar)
			  else (* embedded && not a hq var && not a qvar: quantified at*)
                   (* the enclosing context for an embedded clause *)
                if (isExpQVar varData) then (*explicitly quant*)
                  (false, perm && (not heapvar) , true)
                else (*implicitly quant at the head of top-level clause*)
                  (false, (isCrossGoal !firstgoal) || perm, true)
			in
			(* set variable data *)
			initVarData varData (Some(myfirst)) myperm mysafety heapvar 
			  (Some(term));
			(* set variable occurrence *)
			first := Some(myfirst)
		  else (*subsequent occurrence *)
			let myperm =
			  if (isQVar varData) then (*explicitly body quant*)
				(!lperm || (isCrossGoal !firstgoal) || perm)
			  else (!lperm || (isCrossGoal !firstgoal))
			in
			(*update variable data *)
			subVarDataUpdate oneuse lperm lastgoal lastuse myperm term;
			(*set variable occurrence *)
			first := Some(false))
  | _ -> Errormsg.impossible Errormsg.none 
		  "sProcessFreeVarTerm: invalid free variable rep"  

(****************************************************************************)
(*               PROCESS TERMS IN ANALYSIS MODE                             *)
(****************************************************************************)

(* Annotating a list of term arguments which might be those of a head of *)
(* a clause or those of a first-order application in analysis mode.      *)
(* The value of heapargs indicates which; clause head args arrive in     *)
(* registers.                                                            *)
(* Following the analysis processing order, the processing of embedded   *)
(* compound terms is delayed.                                            *)
let rec aProcessAppArgs termList delayed heaparg =
  match termList with
	[] -> aProcessTerms (List.rev delayed)
  | (term :: rest) ->
	  match term with
		Absyn.ConstantTerm(_, tyenv, _, _) ->
		  aProcessTypeArgs tyenv [] true;
		  aProcessAppArgs rest delayed heaparg
	  | Absyn.FreeVarTerm(freeVarInfo, _, _) ->
		  aProcessFreeVarTerm freeVarInfo term heaparg;
		  aProcessAppArgs rest delayed heaparg
	  | Absyn.AbstractionTerm(_) ->
		  aProcessAppArgs rest (term :: delayed) heaparg
	  | Absyn.ApplicationTerm(_) ->
		  aProcessAppArgs rest (term :: delayed) heaparg
	  | _ -> aProcessAppArgs rest delayed heaparg
		
(* Annotating a list of terms delayed from processing application args *)
(* Note that a delayed term can only be an abstraction or an           *)
(* application.                                                        *)
and aProcessTerms termList =
  match termList with
	[] -> ()
  | (term :: rest) ->
	  match term with
		Absyn.AbstractionTerm(abstInfo, _, _)->sProcessAbstractionTerm abstInfo
	  | Absyn.ApplicationTerm(applInfo, _, _)->aProcessApplicationTerm applInfo
	  | _ -> Errormsg.impossible Errormsg.none "aProcessTerms: invalid term"

			
(* Annotating an application term in analysis mode. The head is examined *)
(* first to determine whether to process in analysis or synthesis mode.  *)
(* If the mode is analysis, the type environment of the head constant is *)
(* processed and then the arguments of the term.                         *)
and aProcessApplicationTerm applInfo =
  match applInfo with
	Absyn.FirstOrderApplication(
	  Absyn.ConstantTerm(c, tyenv, _, _), args, _) ->
		aProcessTypeArgs tyenv [] true;
		aProcessAppArgs args [] true
  | _ -> sProcessApplicationTerm applInfo 


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
and aProcessFreeVarTerm freeVarInfo varOcc heapvar = 
  match freeVarInfo with
	Absyn.FreeVar(varData, first) ->
	  (match varData with
		Absyn.Var(oneuse, lperm, lsafety, lheapvar, _, firstgoal, lastgoal,
				  lastuse) ->
		  if (firstEncountered firstgoal) then (*not encountered*)
			(initVarData varData (Some (not (isEmbedded ()))) false true heapvar
						 (Some varOcc);  (* variable data *)
			 
			 first := Some (not (isEmbedded ())))   (* variable occurrence *)
	  
		  else (* has been processed *)
			(oneuse := Some(false);           (* variable data *)
			 lastuse := Some(varOcc);

			 first := Some(false)             (* variable occurrence *)
			))
  | _ -> Errormsg.impossible Errormsg.none 
		   "aProcessFreeVarTerm: invalid variable rep"

(****************************************************************************)
(*                        PROCESS CLAUSES                                   *)
(****************************************************************************)
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
	  let index = if ((lastgoal = tmpGoalNum - 1) || notrim) then lastgoal - 2
	              else lastgoal - 1
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
  goalEnvAssoc := Absyn.GoalEnvAssoc(gespList)


(**********************************************************************)
(* process a clause:                                                  *)
(**********************************************************************)
let rec processClause clause =  
  (* collect explicitly head quantified variables into hqVars and expQVars *)
  (* The order of this variables in the global lists does not matter.      *)
  let collectHQVars varList =
	match varList with
	  [] -> ()
	| (var :: rest) -> addHQVars var; addExpQVars var
  in

  (* function body of processClause *)
  match clause with
	Absyn.Fact(_, args, tyargs, _, _, varMaps, tyVarMaps, expHQVars, _, 
			   impMods) ->
	  collectHQVars expHQVars;
	  processClauseHead args tyargs;
	  (varMaps, tyVarMaps)
  | Absyn.Rule(_, args, tyargs, _, _, varMaps, tyVarMaps, expHQVars, _, goal, 
			   goalEnvAssoc, cutVar, hasenv, impmods) ->
	  collectHQVars expHQVars;
	  processClauseHead args tyargs;
	  let perm = (impmods = []) in
	  let (myhasenv, notrim) = processGoal goal perm true cutVar in
	  hasenv := myhasenv || notrim || perm;
	  assignPermVars goalEnvAssoc (notrim || perm);
      (varMaps, tyVarMaps)


(******************************************************************)
(* process a clause head: process the type arguments and term     *)
(* arguments of the clause head in analysis mode.                 *) 
(******************************************************************)
and processClauseHead args tyArgs =  
  aProcessTypeArgs tyArgs [] false;
  aProcessAppArgs args [] false

(******************************************************************)
(* process a goal:                                                *)
(******************************************************************)
and processGoal goal perm last cutvar = 
  match goal with
	Absyn.AtomicGoal(pred, _, _, args, tyargs) ->
	  processAtomicGoal pred args tyargs perm last cutvar
  | Absyn.AndGoal(andl, andr) ->
	  let _ = processGoal andl true false cutvar in
	  processGoal andr perm last cutvar 
  | Absyn.SomeGoal(varData, body) ->
	  processSomeGoal varData body perm last cutvar
  | Absyn.AllGoal(Absyn.HCVarAssocs(hcVarAssoc), body) ->
	  (processAllGoal hcVarAssoc body last cutvar, true) 
  | Absyn.ImpGoal(Absyn.Definitions(clDefs), _, body) ->
	  (processImpGoal clDefs body last cutvar, true)
  | _ -> Errormsg.impossible Errormsg.none "processGoal: invalid CutFailGoal"
  
(**************************************************************************)
(* process an atomic goal (assumed to be rigid). In the situation of a    *)
(* deep cut, the cutvar of the entire clause is set or updated. hasenv and*)
(* notrim are decided and pervGoal and goalNum are updated if necessary.  *)
(**************************************************************************)
and processAtomicGoal pred args tyargs perm last cutvar =

  (* process a cut goal *)
  let processCutGoal cutvar last =
	(* initiate a cutvar for the clause or update an existing one *)
	let processCutVar cutvar gnum deepcut=
	  if (deepcut) then
		match (!cutvar) with
		  None ->
			let cutvarData = Absyn.makeCutVariableData gnum in
			cutvar := Some(cutvarData);
			addClTermVar (cutvarData) (* add to clause variable list *)
		| Some(cutvarData) -> Absyn.setVariableDataLastGoal cutvarData gnum
	  else () 
	in
	let hasenv = false in (* hasenv set to false for a cut goal *)
	let notrim = last && (isPervGoal ()) in(*notrim set to isPervGoal if last *)
	let goalNum = getGoalNum () in
	processCutVar cutvar goalNum (goalNum > 1);
	if (last) then (incGoalNum (); (hasenv, notrim))
	else (hasenv, notrim)
  in

  (* calculate hasenv, notrim and set goalNum and pervGoal *)
  let collectHasenvAndNotrim last pred =
	let hasenv = not last in (*hasenv*)
	let notrim = (* calculate notrim and set goalNum and pervGoal *)
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
	(hasenv, notrim)
  in

  (* function body of processAtomicGoal *)
  if (Pervasive.iscutConstant pred) then processCutGoal cutvar last
  else (* other than cut goal*)
	 (sProcessTypes tyargs perm;     (*process type args in synthesis mode *)
	  sProcessTerms args perm false; (*process term args in synthesis mode *)
	  collectHasenvAndNotrim last pred)		


(************************************************************************)
(* process a some goal: initiate the existentially quantitied variable  *)
(* data and collect it into variable list of the clause, hqVars and     *)
(* expQVars.                                                            *)
(************************************************************************)
and processSomeGoal varData body perm last cutvar =
  initVarData varData None perm false false None;
  addHQVars varData;
  addExpQVars varData;
  processGoal body perm last cutvar

(************************************************************************)
(* process an all goal: initiate the list of the universally quantified *)
(* variable data and collect them into variable list of the clause,     *)
(* hqVars and expQVars.                                                 *)
(************************************************************************)
and processAllGoal hcVarAssoc body last cutvar =
  (* init universally quantified vars and collect them into lists *)
  let rec collectUnivVars hcVarPairs =
	match hcVarPairs with
	  [] -> ()
	| ((varData, _)::rest) -> 
		(initVarData varData None true false false None;
		 addHQVars varData;
		 addExpQVars varData)
  in  
  
  collectUnivVars hcVarAssoc; 
  let (hasenv, _) = processGoal body true last cutvar in
  hasenv

(************************************************************************)
(* process an implication goal:                                         *)
(************************************************************************)
and processImpGoal clDefs body last cutvar =
  (* process embedded clauses *)
  let rec processImpDefs clDefs clVars clTyVars=
	match clDefs with
	  [] -> (clVars, clTyVars)
	| ((_, (clauses, _, _, _, _, _)) :: rest) ->
		let rec processImpClauses cls clVars clTyVars =
		  match cls with
			[] ->  (clVars, clTyVars)
		  | (cl :: rest) -> 
			  let (newClVars,newClTyVars)=processImpClause cl clVars clTyVars in
			  processImpClauses rest newClVars newClTyVars
		in
		let (newClVars, newClTyVars) = 
		  processImpClauses (!clauses) clVars clTyVars 
		in
		processImpDefs rest newClVars newClTyVars
  in 
 
  (* update lastgoal for a list of variabe data *)
  let rec setLastGoalVars vars lastGoal =
	match vars with 
	  [] -> ()
	| (varData :: rest) -> 
		(Absyn.setVariableDataLastGoal varData lastGoal;
		 setLastGoalVars rest lastGoal)
  in

  (* update lastgoal for a list of type variable data *)
  let rec setLastGoalTyVars tyVars lastGoal =
	match tyVars with 
	  [] -> ()
	| (tyVarData :: rest) -> 
		(Absyn.setTypeVariableDataLastGoal tyVarData lastGoal;
		 setLastGoalTyVars rest lastGoal)
  in  

  (* function body of processImpGoal *)
  let myisPervGoal = isPervGoal () in
  let (clVars, clTyVars) = processImpDefs clDefs [] [] in
  setPervGoal myisPervGoal;
  let (hasenv, _) = processGoal body true last cutvar in
  let goalNum = getGoalNum () in
  let lastGoal = if (last) then (goalNum - 1) else goalNum in 
  setLastGoalVars clVars lastGoal; 
  setLastGoalTyVars clTyVars lastGoal;
  hasenv	

(***********************************************************************)
(* process an embedded clause including the (types) variables in its   *)
(* map lists; and collect lists of (type) variable data whose lastgoal *)
(* fields should be updated.                                           *)
(***********************************************************************) 
and processImpClause clause clVars clTyVars =

  (* process variable data in the termVarMap list *)
  let rec processVarMaps varMaps clVars=
	match varMaps with
	  [] -> clVars
	| ((fromVarData, toVarData)::rest) ->
		match fromVarData with
		  Absyn.Var(_, perm, _, _, _, firstgoal, _, _) ->
			if (firstEncountered firstgoal) then (*first encountered*)
			  ((initVarData fromVarData None true false false None);
			   processVarMaps rest (fromVarData :: clVars))
			else (* has encountered *)
			  (perm := true;
  			   processVarMaps rest (fromVarData :: clVars))
  in

  (* process type variable data in the typeVarMap list *)
  let rec processTyVarMaps tyVarMaps clTyVars =
	let processToVar varData =
	  match varData with
		Absyn.TypeVar(firstuse, _, _, safety, heapvar, _, _, _) ->
		  safety := true;
		  heapvar := false;
		  match (!firstuse) with
			Some (Absyn.TypeVarType tyvarInfo) ->
			  (match (!tyvarInfo) with
				Absyn.FreeTypeVar(tyvarData, first) -> first := Some false
			  | _ -> Errormsg.impossible Errormsg.none 
					   "processTyVarMaps: invalid type variable rep")
		  | _ -> ()
	in			
	match tyVarMaps with
	  [] -> clTyVars
	| ((fromTyVarData, toTyVarData) :: rest) ->
		let goalNum = getGoalNum () in
		processToVar toTyVarData;
		match fromTyVarData with
		  Absyn.TypeVar(_, _, perm, safety, heapvar, _, firstgoal, 
						lastgoal) ->
			if (firstEncountered firstgoal) then (* first encountered *)
			  (perm := true;
			   safety := true;
			   heapvar := false;
			   firstgoal := goalNum;
			   lastgoal := goalNum;
			   
			   addClTypeVar (Some(fromTyVarData));
			   processTyVarMaps rest (fromTyVarData :: clTyVars))
			else 
			  (perm := true;
			   processTyVarMaps rest (fromTyVarData :: clTyVars))
  in  

  (* function body of processImpClause *)
  (*bookkeeping global info *)
  let (lqVars, lhqVars, lexpqVars, lclVars, lembedded, lgoalNum) =
	(getQVars (), getHQVars (), getExpQVars (), getClVars (), 
	 isEmbedded (), getGoalNum ())
  in
  (*processing the embedded clause with empty global lists *)
  setQVars []; setHQVars []; setClVars []; setEmbedded true; setGoalNum 1;
  let (Absyn.TermVarMap(varMaps), Absyn.TypeVarMap(tyVarMaps)) =  
	processClause clause
  in
  setQVars lqVars; setHQVars lhqVars; setClVars lclVars; 
  setEmbedded lembedded; setGoalNum lgoalNum;
  (* processing variable data in varMaps *)
  let newClVars = processVarMaps varMaps clVars in
  (* processing type variable data in tyVarMaps *)
  let newClTyVars = processTyVarMaps tyVarMaps clTyVars in
  (newClVars, newClTyVars)  


(**********************************************************************)
(* process top-level clauses of the module                            *)
(**********************************************************************)
let rec processTopLevelClDefs clDefs = 
  match clDefs with
	[] -> ()
  | ((_, clausesBlock) :: rest) ->
	  let processDef cls = (* annotate each clause in the clause block *)
		match cls with
		  [] -> ()
		| (cl :: rest) ->
			setQVars []; setHQVars []; setClVars []; 
			setEmbedded false; setGoalNum 1; setPervGoal false;
			let _ = processClause cl in ()
	  in
	  processDef (Absyn.getClauseBlockClauses clausesBlock);
	  processTopLevelClDefs rest

(****************************************************************************)
(*                        INTERFACE FUNCTION                                *)
(****************************************************************************)
let processClauses amod =
  match (Absyn.getModuleClauses amod) with
	Absyn.PreClauseBlocks(Absyn.Definitions(clauseDefs)) ->
	  processTopLevelClDefs clauseDefs;
  | _ -> Errormsg.impossible Errormsg.none "processClauses: invalid clause rep"
