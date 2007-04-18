(*****************************************************************************)
(* Module Processclauses:                                                    *)
(* Transform the term representations of clauses into their clauses          *)
(* representations.                                                          *)
(* 1. Type environments associated with constants occurrences are trimmed    *)
(*    according to the type skeleton optimization.                           *)
(* 2. Lambda-bound variables are transformed into de Bruijn indexes and the  *)
(*    list of binders in an abstraction term is removed.                     *)
(* 3. Other variables (type variables) are transformed into logic (type)     *)
(*    variables with a form suitable for variable annotations; their scope   *)
(*    information are collected and recorded along with clauses and goals.   *)
(* 4. Clauses defining a predicate are collected and associated with the     *)
(*    predicate name being defined.                                          *)
(* 5. String arguments are collected.                                        *)
(*****************************************************************************)

(***************************************************************************)
(* Global variables and access functions                                   *)
(***************************************************************************)

(*********************************************************************)
(* the string list of a module                                       *)
(*********************************************************************)
let modStr : Absyn.astringinfo list ref = ref []
let addModStr strinfo = modStr := strinfo :: (!modStr)

(*********************************************************************)
(* a flag indicating whether the processing is under embedded clause *)
(* context                                                           *)
(*********************************************************************)
let embeddedClause : bool ref = ref false
let isEmbedded () = !embeddedClause
let setEmbedded flag = embeddedClause := flag

(*********************************************************************)
(* Term variables (the association of variable name and variable     *)
(* information) appearing free in a clause                           *)
(*********************************************************************)
let tVars : (Absyn.atypesymbol * Absyn.avar) list ref = ref []

(*********************************************************************)
(* Type variables appearing free in a clause                         *)
(*********************************************************************)
let tyVars : (Absyn.atype * Absyn.atypevar) list ref = ref []

(*********************************************************************)
(* Term variables (the association of variable name and variable     *)
(* information) that are (explicitly) quantified in the body of a    *)
(* clause                                                            *)
(*********************************************************************)
let qVars: (Absyn.atypesymbol * Absyn.avar) list ref = ref []

(*********************************************************************)
(* Term variables that are explicitly quantified at the head of a    *)
(* clause                                                            *)
(*********************************************************************)
let hqVars : (Absyn.atypesymbol * (Absyn.avar option ref)) list ref = ref []

(*********************************************************************)
(* gListAdd: add a new item to the front of one of the global list   *)
(*********************************************************************)
let gListAdd index data glist = (glist := (index, data) :: !glist)

(*********************************************************************)
(* gListFind:                                                        *)
(* find information associated with a given index in one of the      *)
(* global list                                                       *)
(* None is returned if the index does not occur.                     *)
(*********************************************************************)
let gListFind index glist =
  let rec find assocs =
	match assocs with
	  [] -> None
	| ((index' , data) :: rest) ->
		if (index' == index) then Some(data)
		else find rest
  in (find (!glist))

(*********************************************************************)
(* gListsSet:                                                        *)
(* destructively update tVars, tyVars, qVars and hqVars to           *)
(* references to given lists.                                        *)
(*********************************************************************)
let gListsSet ntVars ntyVars nqVars nhqVars =
  (tVars := ntVars; tyVars := ntyVars; qVars := nqVars; hqVars := nhqVars)


(****************************************************************************)
(*                         PROCESS TYPES                                    *)
(****************************************************************************)
(*****************************************************************************)
(* Processing type arguments of a clause:                                    *)
(* transform a type into a form suitable for variable annotation; as a side  *)
(* effect, free type variables are collected into tyVars.                    *)
(*****************************************************************************)
let rec transType tyExp =
  match tyExp with 
	Absyn.TypeVarType(typeVarInfo) ->
	  (match (!typeVarInfo) with
		Absyn.FreeTypeVar(_) -> (* free type variable *)
	   (* If the type variable has already occurred, used the associated type*)
	   (* variable information; otherwise, create a new type variable cell   *)
	   (* and enter it into tyVars together with the type variable.          *)
		  let tyVarData =
			let tyVarDataInfo = gListFind tyExp tyVars in 
			match tyVarDataInfo with
			  None -> 
				let varData = Absyn.makeNewTypeVariableData () in
				gListAdd tyExp varData tyVars;
				varData
			| Some(varData) -> varData
 		  in 
		  Absyn.makeNewTypeVariable tyVarData;
	  | Absyn.BindableTypeVar(target) ->(*bound type variable: type reference*)
		  transType (Option.get (!target)))
  | Absyn.ArrowType(arg, target) ->
	  Absyn.ArrowType(transType arg, transType target)
  | Absyn.ApplicationType(kind, args) ->
	  Absyn.ApplicationType(kind, List.map transType args)
  | _ -> Errormsg.impossible Errormsg.none "transType: invalid type expression"
	  

(****************************************************************************)
(*                         PROCESS TERMS                                    *)
(****************************************************************************)
(*****************************************************************************)
(* Processing term arguments of a clause:                                    *)
(* transform a term into a form suitable for variable annotation             *)
(*****************************************************************************)
let rec transTerm bvs tm = 
  match tm with
	Absyn.IntTerm(v, _, _)            -> Absyn.IntTerm(v, false, Errormsg.none)
  | Absyn.RealTerm(v, _, _)           -> Absyn.RealTerm(v, false,Errormsg.none)
  | Absyn.StringTerm(s, _, _)         -> (transTermStr s)
  | Absyn.ConstantTerm(c, tyenv, _, _)-> (transTermConst c tyenv)
  | Absyn.FreeVarTerm(varInfo, _, _)  -> (transTermFreeVar varInfo)
  | Absyn.BoundVarTerm(varInfo, _, _) -> (transTermBoundVar varInfo bvs) 
  | Absyn.AbstractionTerm(abst, _, _) -> (transTermAbst bvs abst) 
  | Absyn.ApplicationTerm(app, _, _)  -> (transTermAppl bvs app)
  | _ -> Errormsg.impossible Errormsg.none "transTerm: invalid term structure"

(***********************************************************************)
(* transform strings:                                                  *)
(* relevant string information is added into the global string list as *)
(* side effect.                                                        *)
(***********************************************************************)
and transTermStr s = 
  match s with
	Absyn.StringLiteral(chs) -> 
	  let strdata = Absyn.StringData(chs, ref None, ref None) in
	  (addModStr strdata);
	  Absyn.StringTerm(strdata, false, Errormsg.none)
  | _ -> Errormsg.impossible Errormsg.none "transTermStr: invalid string rep"

(**************************************************************************)
(* transform constants:                                                   *)
(* TO ADD: rearrange type environment according to information provided   *)
(* by type reduction.                                                     *)
(**************************************************************************)
and transTermConst c tyenv = 
  (* May need to rearrange type environment according to type skel opt *)
  Absyn.ConstantTerm(c, (List.map transType tyenv), false, Errormsg.none)

(**************************************************************************)
(* transform variables:                                                   *)
(* The variable could be quantified in the following three situations:    *)
(* 1. explicitly universal quantified at the head of the current clause;  *)
(* 2. explicitly universal/exstential quantified in the body of the       *)
(*    current clause;                                                     *)
(* 3. explicitly or implicitly quantified at the embedding context for    *)
(*    an embedded goal; implicitly quantified at the head for a top-level *)
(*    goal.                                                               *)
(**************************************************************************) 
and transTermVar tysy =
  let qVar = gListFind tysy qVars in (*body quantified? *)
  match qVar with
	Some(qVarData) -> qVarData
  | None ->
	  let hqVar = gListFind tysy hqVars in (* exp head quantified?*)
	  match hqVar with
		Some(hqVarInfo) ->
		  (match (!hqVarInfo) with
			Some(hqVarData) -> hqVarData
		  | None -> (*exp head quant; but first encountered *)
			  let hqVarData = Absyn.makeNewVariableData () in
			  hqVarInfo := Some(hqVarData); (*update hqVars*)
			  hqVarData)
	  | None -> (* implicitly quantified at head of top-level clause or     *) 
				(* implicitly/explicitly quantified in embedding context of *)
                (* embedded clauses.                                        *)
          let var = gListFind tysy tVars in
          match var with
            Some(myVarData) -> myVarData 
          | None -> 
              let myVarData = Absyn.makeNewVariableData () in
			  gListAdd tysy myVarData tVars; (*update tVars*)
			  myVarData

(**************************************************************************)
(* transform free variables:                                              *)
(* This variable could be implicitly quantified at the head of the top    *)
(* level clause or could be an explicitly quantified one that appears as  *)
(* the head of a flex goal.                                               *)
(**************************************************************************)
and transTermFreeVar var =
  match var with
	Absyn.NamedFreeVar(tysy) ->
	  Absyn.FreeVarTerm(Absyn.FreeVar(transTermVar tysy, ref None), false, 
						Errormsg.none)
  | _ -> Errormsg.impossible Errormsg.none "transTermFreeVar: invalid var rep"

(***************************************************************************)
(* transform bound variables:                                              *)
(* The variable must be bound explicitly in one of the three situtions:    *)
(* 1. it is lambda bound.                                                  *)
(* 2. it is (explicitly) universally bound at the head of the current      *)
(*    clause; it is universally or existentially bound in the body of the  *)
(*    current clause.                                                      *)
(* 3. it is (explicitly) universally or existentially bound in the         *)
(*    enclosing context of the current clause (only relevant for embedded  *)
(*    clauses.                                                             *)
(* In the first situation, the variable is transformed into a de Bruijn    *)
(* index and in the latter two situations, the variable is transformed into*)
(* a logic (free) variable representation which is suitable for variable   *)
(* annotation.                                                             *)
(***************************************************************************)
and transTermBoundVar var bvs =
  match var with
	Absyn.NamedBoundVar(tysy) ->
	  let rec ith tysys ind =
		match tysys with
		  [] -> (ind, false)
		| (tysy' :: rest) -> 
			if (tysy' == tysy) then (ind, true)
			else ith rest (ind + 1)
	  in
	  let (dbInd, found) = ith bvs 1 in
	  if (found) then (* lambda-bound? *)
		Absyn.BoundVarTerm(Absyn.DBIndex(dbInd), false, Errormsg.none)
	  else 
		Absyn.FreeVarTerm(Absyn.FreeVar(transTermVar tysy, ref None), false, 
						  Errormsg.none)
  | _ -> Errormsg.impossible Errormsg.none "transTermBoundVar: invalid var rep"


(**************************************************************************)
(* transform abstractions:                                                *)
(**************************************************************************)
and transTermAbst bvs abstTerm =
  match abstTerm with
	Absyn.UNestedAbstraction(binders, nabs, body) ->
	  (* Note the order of the binders are reversed in the collected list *)
	  let rec collectBinders mybds newbd =
		match mybds with
		  [] -> newbd
		| (bd::rest) -> collectBinders rest (bd::newbd)
	  in
	  let newbody = transTerm (collectBinders binders bvs) body in  
	  Absyn.AbstractionTerm(Absyn.UNestedAbstraction([], nabs, newbody), false,
 							Errormsg.none)
  | _ -> Errormsg.impossible Errormsg.none "transTermAbst: invalid abst rep"

(**************************************************************************)
(* transform applications:                                                *)
(**************************************************************************)
and transTermAppl bvs applTerm =
  match applTerm with
	Absyn.FirstOrderApplication(func, args, nargs) ->
	  Absyn.ApplicationTerm(
	    Absyn.FirstOrderApplication(transTerm bvs func,
									List.map (transTerm bvs) args,
									nargs), false, Errormsg.none)
  | _ -> Errormsg.impossible Errormsg.none "transTermAppl: invalid app rep"

(****************************************************************************)
(*                        PROCESS CLAUSES                                   *)
(****************************************************************************)

(*******************************************************************)
(* Insert a clause into clauses definition list:                   *)
(* If the clause is not embedded in an implication goal, it should *)
(* also be inserted into the clause block associated with its      *)
(* predicate name.                                                 *)
(* NOTE: The clauses in a clause block are inserted in an order    *)
(* reversed to that in which they are encountered; the definitions *)
(* are the reversed order to that in which they are encountered.   *) 
(*******************************************************************)
let insertClause pred clause clDefs embedded closed =

  (* looking for the clause block defining a predicate name from a given *)
  (* definition list.                                                    *)
  let rec findClBlock clDefs =
	match clDefs with
	  [] -> None
	| ((pred', clBlock)::rest) ->
		if (pred == pred') then Some(clBlock)
		else findClBlock rest
  in

  let clBlock = findClBlock clDefs in
  match clBlock with
	None -> 
	  let newClBlock = Absyn.makeNewClauseBlock clause closed in
	  (* record the clause block into the code info field of the predicate *)
	  (* name if top-level *)
	  (if (not embedded) then
		Absyn.setConstantCodeInfo pred (Some (Absyn.Clauses newClBlock))
	   else ());
	  ((pred, newClBlock) :: clDefs) 
  | Some(cls, _, _, _, _, _) ->
	  cls := clause :: !cls;
	  clDefs

(*******************************************************************)
(* A pre clause representation.                                    *)
(*******************************************************************)
(* (pred, term args, type args, number term args, number type args (, body)) *)
type preclause = 
    Fact of Absyn.aconstant * Absyn.aterm list * Absyn.atype list * int * int
  | Rule of Absyn.aconstant * Absyn.aterm list * Absyn.atype list * int * int
		* Absyn.agoal

(**********************************************************************)
(* Process Clause:                                                    *)
(* Absyn.aterm -> (preclause * (Absyn.atysy * Absyn.avar) list *      *)
(*              (Absyn.atype * Absyn.atypevar) list * Absyn.avar list)*)
(* Transform term representation of a clause into a pre clause paired *)
(* with the association lists of term variables and type variables    *)
(* appearing free in the clause and the list of universal variables   *)
(* explicitly quantified at the clause head.                          *) 
(**********************************************************************)
let rec processClause clauseTerm =
  (* collect variable data of explicitly head quantified variables that are*)
  (* used in the clause *)
  let collectHQVars hqVarAssocs =
	let rec collectHQVarsAux hqVarAssocs vars =
	  match hqVarAssocs with
		[] -> vars
	  | ((tysy, varInfo):: rest) ->
		  match (!varInfo) with
			None -> collectHQVarsAux rest vars
		  | Some(hqVarData) ->  collectHQVarsAux rest (hqVarData :: vars)
	in
	collectHQVarsAux hqVarAssocs []
  in
  match clauseTerm with
    Absyn.ConstantTerm(head, tyenv, _, _) -> (* proposition fact *)
      let (preClause, freeVars, freeTyVars) = processFact head tyenv [] 0 in
      (preClause, freeVars, freeTyVars, collectHQVars (!hqVars))
  | _ -> (* must be an application *)
      let func = Absyn.getTermApplicationFunc clauseTerm in
      let args = Absyn.getTermApplicationArgs clauseTerm in
      let head = Absyn.getTermConstant func in
      if (Pervasive.isallConstant head) then
	let arg = List.hd args in
	gListAdd (List.hd (Absyn.getTermAbstractionVars arg)) (ref None) hqVars; 
	processClause (Absyn.getTermAbstractionBody arg)
      else
	let (preClause, freeVars, freeTyVars) =
	if (Pervasive.isimplConstant head) then (* process rule *)
	  processRule (List.hd args) (List.hd (List.tl args))
	else (* process fact *)
	  processFact head (Absyn.getTermTypeEnv func) args
	    (Absyn.getTermApplicationArity clauseTerm)
	in
	(preClause, freeVars, freeTyVars, collectHQVars (!hqVars))
	
(***************************************************************************)
(* process clause head:                                                    *)
(* Absyn.aterm list -> Absyn.atype list ->                                 *)
(*                            (Absyn.aterm list * Absyn.atype list)        *)
(* transform term arguments, type arguments of a clause head into a form   *)
(* suitable for variable annotation.                                       *)
(* Note the type skeleton reductions should have no effect for predicate   *)
(* names, and so are not concerned here.                                   *)
(***************************************************************************)
and processClauseHead args tyargs =
  (List.map (transTerm []) args, List.map transType tyargs) 

(***************************************************************************)
(* process a fact:                                                         *)
(* predicate name, predicate arguments, predicate type arguments, the      *)
(* number of arguments (term and type), the number of term arguments are   *)
(* collected for a fact.                                                   *)
(***************************************************************************)
and processFact pred tyargs tmargs ntmargs =
  let (predArgs, predTyArgs) = processClauseHead tmargs tyargs in
  let preClause = Fact(pred, predArgs, predTyArgs, 
					   ntmargs + Absyn.getConstantTypeEnvSize(pred), ntmargs) 
  in
  (preClause, !tVars, !tyVars)

(***************************************************************************)
(* process a rule:                                                         *)
(* predicate name, predicate arguments, predicate type arguments, the      *)
(* number of arguments (term and type), the number of term arguments and   *)
(* the goal representation of the clause body are collected for a rule.    *)
(***************************************************************************)
and processRule clauseHead clauseBody =
  let (pred, tyenv, args, arity) =
	match clauseHead with
	  (*proposition*)
	  Absyn.ConstantTerm(pred, tyenv, _, _) -> (pred, tyenv, [], 0) 
	| _ -> (* must be an application *)
		let head = Absyn.getTermApplicationFunc clauseHead in
		(Absyn.getTermConstant head, Absyn.getTermTypeEnv head,
		 Absyn.getTermApplicationArgs clauseHead, 
		 Absyn.getTermApplicationArity clauseHead)
  in
  let (predArgs, predTyArgs) = processClauseHead args tyenv in
  let goal = processGoal clauseBody in
  (Rule(pred, predArgs, predTyArgs, Absyn.getConstantTypeEnvSize pred + arity,
		arity, goal),
   !tVars, !tyVars)

(*****************************************************************************)
(* Process Goal:                                                             *)
(*          Absyn.aterm -> Absyn.agoal                                       *)
(* transform a goal from its term representation into its goal representation*)
(*****************************************************************************)
and processGoal gltm = 
  match gltm with
    Absyn.ApplicationTerm(_) ->
      if (gltm == Pervasiveutils.cutFail) then Absyn.CutFailGoal
      else 
		let head = Absyn.getTermApplicationFunc gltm in
		let args = Absyn.getTermApplicationArgs gltm in
		(match head with
		  Absyn.ConstantTerm(pred, _, _, _) ->
			if Pervasive.isandConstant pred then               (*and goal *)
			  processAndGoal (List.hd args) (List.hd (List.tl args))
			else if Pervasive.issomeConstant pred then         (*some goal*)
			  processSomeGoal (List.hd args) 
			else if Pervasive.isallConstant pred then          (*all goal *)
			  processAllGoal (List.hd args)
			else if Pervasive.isimplConstant pred then         (*imp goal *)
			  processImpGoal (List.hd args) (List.hd (List.tl args))
			else                                     (* rigid atomic goal *)
			  processAtomicGoal gltm head (Absyn.getTermApplicationArgs gltm)
				(Absyn.getTermApplicationArity gltm)
		| _ -> processAtomicGoal gltm head [] 0)     (* flex atomic goal *)
  | _-> processAtomicGoal gltm gltm [] 0 (* proposition goal: flex or rig*)
		
		
(**************************************************************************)
(* process atomic goal:                                                   *)
(* Note a flex goal of form (F t1... tn) is transformed into              *)
(*      solve (F t1 ... tn)                                               *)
(**************************************************************************)
and processAtomicGoal gltm head args arity =
  match head with
	Absyn.FreeVarTerm(Absyn.NamedFreeVar(_), _, _) -> (* free var head *)
	  Absyn.AtomicGoal(Pervasive.solveConstant, 1, 1, [(transTerm [] gltm)], [])
  | Absyn.ConstantTerm(pred, tyenv, _, _) ->
	  Absyn.AtomicGoal(pred, arity + Absyn.getConstantTypeEnvSize(pred), arity,
					   List.map (transTerm []) args, List.map transType tyenv)
  | _ -> Errormsg.impossible Errormsg.none "processAtomicGoal: invalid pred"

(***************************************************************************)
(* process and goal:                                                       *)
(***************************************************************************)
and processAndGoal andl andr =
  Absyn.AndGoal(processGoal andl, processGoal andr)

(***************************************************************************)
(* process some goal:                                                      *)
(* The existential quantified variable is added into qVars as side effect, *)
(* and its variable data information is recorded with the goal structure   *)
(***************************************************************************)
and processSomeGoal goalBody =
  let tysy = List.hd (Absyn.getTermAbstractionVars goalBody) in
  let varData = Absyn.makeNewVariableData () in
  gListAdd tysy varData qVars; (* update qVars *)
  Absyn.SomeGoal(varData, processGoal (Absyn.getTermAbstractionBody goalBody))

(***************************************************************************)
(* process all goal:                                                       *)
(* Contiguous unviersal quantifed variables are added into qVars as side   *)
(* effect, and their variable data information paired with corresponding   *)
(* hidden constants are collected and recorded  with the goal structure.   *) 
(***************************************************************************)
and processAllGoal goalBody =
  (* enter the given variable into the qVars list; and create a hidden     *)
  (* constant variable association pair. *)
  let collectHCpair tysy =
    let hcData = Absyn.getTypeSymbolHiddenConstant tysy in
    let varData = Absyn.makeNewVariableData () in
    gListAdd tysy varData qVars; (* update qVars *)
    (varData, hcData)
  in
  
  let rec processAllGoalAux goal hcPairs =
    match goal with
      Absyn.ApplicationTerm(_) ->
		let head = Absyn.getTermApplicationFunc goal in
		let args = Absyn.getTermApplicationArgs goal in
		if Pervasive.isallConstant (Absyn.getTermConstant head) then
		   (*all goal*)
		  let arg = List.hd args in
		  let tysy = List.hd (Absyn.getTermAbstractionVars arg) in
		  processAllGoalAux (Absyn.getTermAbstractionBody arg) 
			((collectHCpair tysy)::hcPairs)
		else (* other than all goal *)
		  Absyn.AllGoal(Absyn.HCVarAssocs(List.rev hcPairs), processGoal goal)
    | _ -> (* other than all goal *)
		Absyn.AllGoal(Absyn.HCVarAssocs(List.rev hcPairs), processGoal goal)
  in
  
  let tysy = List.hd (Absyn.getTermAbstractionVars goalBody) in
  processAllGoalAux (Absyn.getTermAbstractionBody goalBody) [collectHCpair tysy]
   
(**************************************************************************)
(* process imp goal:                                                      *)
(* a). transform the body goal;                                           *)
(* b). collect definitions in the ancester;                               *)
(* c). collect the variable init list                                     *)
(**************************************************************************)
and processImpGoal clauseTerm goalTerm =
  let (clauseDefs, varInits) = processImpClauses clauseTerm [] [] in
  Absyn.ImpGoal(Absyn.Definitions(clauseDefs), 
				Absyn.VarInits(List.rev varInits), 
				processGoal goalTerm)		
    
(********************************************************************)
(* process clauses embedded in an implication goal; collect their   *)
(* definitions in suitable form and collect variables that should   *)
(* be initialized before using the clause.                          *)
(********************************************************************)
and processImpClauses clauseTerm clauseDefs varInits =
  let collectDefsAndVarInits cltm clDefs myvarInits =
	let (pred, clause, newVarInits) = processImpClause cltm varInits in
	let newClDefs = insertClause pred clause clDefs true false in
	(newClDefs, newVarInits)
  in

  match clauseTerm with
    Absyn.ApplicationTerm(_) -> (* recurse over the conjunctive structure *)
      let args = Absyn.getTermApplicationArgs clauseTerm in
      let head = Absyn.getTermApplicationFunc clauseTerm in
      if Pervasive.isandConstant (Absyn.getTermConstant head) then
		let (newClDefs, newVarInits) = 
		  processImpClauses (List.hd args) clauseDefs varInits
		in
		processImpClauses (List.hd (List.tl args)) newClDefs newVarInits
      else
		collectDefsAndVarInits clauseTerm clauseDefs varInits
  | _ -> collectDefsAndVarInits clauseTerm clauseDefs varInits
		
(********************************************************************)
(* process an embedded clause:                                      *)
(* Mapping of type and term variables free in the embedded clause   *)
(* are generated; free term variables having their first occurrence *)
(* in the embedded clause is collected in variable initialization   *)
(* list. The scope of free type variables are reflected in the type *)
(* variable mapping list: those occurring in the type association   *)
(* of a free term variable are assumed to have the same scopes of   *)
(* the corresponding term variables and therefore are collected in  *)
(* the type variable mapping; other free type variables are assumed *)
(* to have their scopes at the head of the embedded clause.         *)  
(********************************************************************)
and processImpClause clauseTerm varInits =
  (* book keeping global lists and flags *)
  let (ltVars, ltyVars, lqVars, lhqVars, lembedded) =
	(!tVars, !tyVars, !qVars, !hqVars, isEmbedded ()) 
  in
  (* process the embedded clause with empty global lists *) 
  gListsSet [] [] [] []; 
  setEmbedded true;
  let (preClause, fvAssoc, tyfvAssoc, expHQVars) = processClause clauseTerm in
  (* recover global lists and flags; generate variable mappings and *)
  (* accumulate variable initialization list *)
  gListsSet ltVars ltyVars lqVars lhqVars;
  setEmbedded lembedded;
  let (fvMaps, tyfvMaps, newVarInits)= mapFreeVars fvAssoc tyfvAssoc varInits in
  let fvMaps' = Absyn.TermVarMap(fvMaps) in
  let tyfvMaps' = Absyn.TypeVarMap(tyfvMaps) in
  match preClause with
	Fact(pred, args, tyargs, nargs, ntargs) ->
	  (pred, 
	   Absyn.Fact(pred, args, tyargs, nargs, ntargs, fvMaps', tyfvMaps', 
				  expHQVars, ref None, []), 
	   newVarInits)
  | Rule(pred, args, tyargs, nargs, ntargs, goal) ->
	  (pred, 
	   Absyn.Rule(pred, args, tyargs, nargs, ntargs, fvMaps', tyfvMaps', 
				  expHQVars, ref None, goal, ref (Absyn.GoalEnvAssoc []), 
				  ref None, ref false, []), 
	   newVarInits)  

(*********************************************************************)
(* Generate variable mappings for type and term variables free in    *)
(* the embedded clause. Accumulate variable initialization lists for *)
(* free term variables having first occurrence in the embedded clause*)  
(*********************************************************************)
and mapFreeVars fvAssocs tyfvAssocs varInits =
  
  (*******************************************************************)
  (* Collect term variable mapping; collect free type variables that *)
  (* global to the embedded clause; accumulate variable              *)
  (* initialization list.                                            *)
  (*******************************************************************)
  let rec collectFvMaps fvAssocs fvMaps varInits globalTyVars =
	match fvAssocs with
	  [] -> (fvMaps, varInits, globalTyVars) 
	| ((tysy, toVarData) :: rest) ->
		(* collect global type variables *)
		let newGlobalTyVars = 
		  Types.freeTypeVars (Absyn.getTypeSymbolType tysy) globalTyVars
		in
		(* collect variable mapping and initialization *)
		let (fromVarData, newVarInits) =
		  let qVar = gListFind tysy qVars in (* body quantified? *)
		  match qVar with
			Some(qVarData) -> (qVarData, varInits)
		  | None -> 
			  let hqVar = gListFind tysy hqVars in (* exp head quantified? *)
			  match hqVar with
				Some(hqVarInfo) ->
				  (match (!hqVarInfo) with
					Some(hqVarData) -> (hqVarData, varInits)
				  | None -> (*exp head quant with first occ in embedded cl:*)
                            (*should be initiated*)
				      let hqVarData = Absyn.makeNewVariableData () in
				      hqVarInfo := Some(hqVarData); (*update hqVars *)
				      (hqVarData, (hqVarData :: varInits)))
			  | None -> 
			      (* implicitly quantified at head of top-level clause or     *)
			      (* implicitly/explicitly quantified in embedding context of *)
			      (* embedded clauses.                                        *)
			      let var = gListFind tysy tVars in
			      match var with
					Some(varData) -> (varData, varInits)
			      | None ->
					  let varData = Absyn.makeNewVariableData () in
					  let newVarInits =
						if not (isEmbedded ()) then (varData :: varInits)
						else varInits
					  in
					  gListAdd tysy varData tVars; (*update tyVars*)
					  (varData, newVarInits)
		in
		collectFvMaps rest ((fromVarData, toVarData)::fvMaps) newVarInits
		  newGlobalTyVars
  in

  (******************************************************************)
  (* Decide the scopes of free type variables, and collect those    *)
  (* that are not bound at the head of the embedded clause to the   *)
  (* free variable mapping.                                         *)
  (******************************************************************)
  let rec collectTyFvMaps tyfvAssocs globalTyVars tyfvMaps =
	match tyfvAssocs with
	  [] -> tyfvMaps
	| ((tyVar, toTyVarData)::rest) ->
		if (List.mem tyVar globalTyVars)
		then (*have scope outside of the embedded clause*)
		  let fromTyVarData =
			let tyvarInfo = gListFind tyVar tyVars in
			match tyvarInfo with
			  Some(tyVarData) -> tyVarData
			| None ->
				let tyVarData = Absyn.makeNewTypeVariableData () in
				gListAdd tyVar tyVarData tyVars; (*update tyVars *)
				tyVarData
		  in
		  collectTyFvMaps rest globalTyVars 
		    ((fromTyVarData, toTyVarData)::tyfvMaps)
		else collectTyFvMaps rest globalTyVars tyfvMaps
  in

  (* function body of mapFreeVars *)
  let (fvMaps,newVarInits,globalTyVars)=collectFvMaps fvAssocs [] varInits [] in
  let tyfvMaps = collectTyFvMaps tyfvAssocs globalTyVars [] in
  (fvMaps, tyfvMaps, newVarInits)

(****************************************************************************)
(*      PROCESS TOP LEVEL (INCLUDING ANONYMOUS) CLAUSES                     *)
(****************************************************************************)
(* a) transform term representations of top-level clauses into their clause *)
(*    representations;                                                      *)
(* b) collect defintitions for predicates:                                  *)
(*    NOTE: the order of definitions and clauses inside one definition are  *)
(*          reversed from those of their appearence in the input term --    *)
(*          THE INPUT TERMS SHOULD BE IN THE REVERSED ORDER OF THEIR        *)
(*          APPEARENCES IN PROGRAM!                                         *)
(****************************************************************************)
let rec processTopLevelClauses clauseTerms impmods clauseDefs anonymous =
  match clauseTerms with
	[] -> clauseDefs
  | (clauseTerm :: rest) ->
	  (* transform from term representation to clause representation *)
	  gListsSet [] [] [] []; (* initialize global variable info lists *)
	  setEmbedded false;     (* initialize embeddedClause flag *)
	  (* transfer a clause *)
	  let (preClause, _, _, expHQVars) = processClause clauseTerm in
	  (* term/type variable mapping is always empty for top-level clauses *)
	  let fvMaps = Absyn.TermVarMap([]) in
	  let tyfvMaps = Absyn.TypeVarMap([]) in
	  let (pred, clause) = 
		(* transfer preclause into clause:                           *)
		(* the offset field for facts and rules and goalNum-envSize  *)
        (* association, cutvar and hasenv fields for rules are left  *)
        (* to be filled in the next phase.                           *) 
		match preClause with 
		  Fact(pred, args, tyargs, nargs, ntermargs) ->
			(pred, Absyn.Fact(pred, args, tyargs, nargs, ntermargs, fvMaps, 
							  tyfvMaps, expHQVars, ref None, impmods))
		| Rule(pred, args, tyargs, nargs, ntermargs, goal) ->
			(pred, Absyn.Rule(pred, args, tyargs, nargs, ntermargs, fvMaps, 
							  tyfvMaps, expHQVars, ref None, goal, 
							  ref (Absyn.GoalEnvAssoc([])), ref None, 
							  ref false, impmods))
	  in
	  (* collect predicate definitions *)
	  let newClDefs = insertClause pred clause clauseDefs false anonymous in
	  processTopLevelClauses rest impmods newClDefs anonymous	  
 


(*****************************************************************************)
(*                          PROCESS CLAUSES                                  *)
(*****************************************************************************)
let processClauses amod clTerms newClTerms =
  match amod with
	Absyn.Module(modname, modimps, modaccs, ctable, ktable, atable, _,
				 gkinds, lkinds, gconsts, lconsts, hconsts, skels, hskels, _)
	->
	  (* process clauses *)
	  let clDefs = processTopLevelClauses clTerms modimps [] false in
	  (* process anonymous clauses: (new clauses introduced in deorification*) 
	  (* Note: the import module field of anonymous clauses should be empty *)
	  let newClDefs = processTopLevelClauses newClTerms [] clDefs true in
      (* Insert the clauses definitions and the string list into the module *)
      (* abstract syntax.                                                   *)
	  Absyn.Module(modname, modimps, modaccs, ctable, ktable, atable, 
				   !modStr, gkinds, lkinds, gconsts, lconsts, hconsts, skels,
				   hskels, 
				   ref (Absyn.PreClauseBlocks(Absyn.Definitions(newClDefs))))
  | _ -> Errormsg.impossible Errormsg.none "processClauses: invalid module"
