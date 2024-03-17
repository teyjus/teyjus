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
(*    predicate name being defined;                                          *)
(* 5. String arguments are collected.                                        *)
(*****************************************************************************)

(** ********************************************************************** **)
(** Global variables and access functions                                  **)
(** ********************************************************************** **)

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

(*****************************************************************************)
(*               Global variable (type variable) lists                       *)
(*****************************************************************************)
(*********************************************************************)
(* 1.Term variables (the association of variable name and variable   *)
(*   information) appearing free in a clause                         *)
(*********************************************************************)
let tVars : (Absyn.atypesymbol * Absyn.avar) list ref = ref []

(*********************************************************************)
(* 2.Type variables appearing free in a clause                       *)
(*********************************************************************)
let tyVars : (Absyn.atype * Absyn.atypevar) list ref = ref []

(*********************************************************************)
(* 3.Term variables (the association of variable name and variable   *)
(*   information) that are (explicitly) quantified in the body of a  *)
(*   clause                                                          *)
(*********************************************************************)
let qVars: (Absyn.atypesymbol * Absyn.avar) list ref = ref []

(*********************************************************************)
(* 4. Term variables that are explicitly quantified at the head of a *)
(*    clause                                                         *)
(*********************************************************************)
let hqVars : (Absyn.atypesymbol * (Absyn.avar option ref)) list ref = ref []

(*****************************************************************************)
(* Global variable (type variable) lists access functions:                   *)
(*****************************************************************************)
(*********************************************************************)
(* gListAdd: add a new item to the front of one of the global list   *)
(*********************************************************************)
let gListAdd index data glist = (glist := (index, data) :: !glist)

(*********************************************************************)
(* gListFind:                                                        *)
(* find information associated with a given "index" in one of the    *)
(* global list                                                       *)
(* None is returned if the "index" does not occur.                   *)
(* Note that the index is usually an address (of mutable structures) *)
(* and so "==" is used for their comparison.                         *)
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


(*********************************************************************)
(* closed definition list : used in insert embedded clauses          *)
(*********************************************************************)
let closedDefs : ((Absyn.aconstant * Absyn.aterm) list) ref = ref []
let setClosedDefs defs = closedDefs := defs	  

let inClosedDefs const impterm =
  let rec inClosedDefsAux defs =
  match defs with
    [] -> false
  | (hc, def) :: rest ->
    if (const == hc) && (Absyn.sameTermStructure impterm def) then true
    else inClosedDefsAux rest 
  in
  inClosedDefsAux (!closedDefs)


(** ********************************************************************** **)
(**                        PROCESS TYPES                                   **)
(** ********************************************************************** **)
(**************************************************************************)
(* transType:                                                             *)
(* Transforming a type into a form suitable for variable annotation:      *)
(* a). the result type expression is reference free;                      *)
(* b). the occurrences of free type variables and their actual variable   *)
(*     data are seperated;                                                *)
(* c). as a side effect, the association of the (original) type variable  *)
(*     and the newly introduced type variable data is collected into      *)
(*     tyVars.                                                            *)
(**************************************************************************)
let rec transType tyExp =
  match tyExp with 
    Absyn.TypeVarType(typeVarInfo) -> 
      (* it is assumed the typeVarInfo here must take form of BindableTypeVar*)
      (match (typeVarInfo) with 
         | Absyn.BindableTypeVar(binding) ->
             if Option.isNone (!binding) then (*actually a free var *)
               let tyVarData =
                 match gListFind tyExp tyVars with
                   | None -> (* not encountered yet *)
                       let varData = Absyn.makeNewTypeVariableData () in
                         gListAdd tyExp varData tyVars; (* add into tyVars *)
                         varData
                   | Some(varData) -> varData
               in
                 Absyn.makeNewTypeVariable tyVarData
             else 
               (* It seems that this situation never happens, possibly
                * due to a previous call to dereferenceType *)
               transType (Option.get (!binding)) (* a type reference *)
         | _ -> 
             (* Originally absyn types do not contain any FreeTypeVar *)
             Errormsg.impossible 
                  Errormsg.none "transType: invalid type expression")
  | Absyn.ArrowType(arg, target) ->
      Absyn.ArrowType(transType arg, transType target)
  | Absyn.ApplicationType(kind, args) ->
      Absyn.ApplicationType(kind, List.map transType args)
  | _ -> Errormsg.impossible Errormsg.none "transType: invalid type expression"

(** ********************************************************************** **)
(**                        PROCESS TERMS                                   **)
(** ********************************************************************** **)
(**************************************************************************)
(* transTerm:                                                             *)
(* transforming terms into a form suitable for variable annotation:       *)
(* a). term variables are separated into term variable occurrences and    *)
(*     term variable data which is based on a nameless representation;    *)
(* b). type environment is trimmed for constant occurrences based on the  *)
(*     type skeleton optimization result;                                 *)
(* c). strings are transformed into a stringData, and the text is collect *)
(*     into the module string list;                                       *)
(* d). bound variables are transformed into de Bruijn indexes and the     *)
(*     binder (name) list of abstractions are removed.                    *)
(* Note it is assumed that the application and abstraction sub structures *)
(* contained in the input term are unnested.                              *)
(**************************************************************************)
let rec transTerm bvs tm = 
  match tm with
  Absyn.IntTerm(v, _)            -> Absyn.IntTerm(v, Errormsg.none)
  | Absyn.RealTerm(v, _)           -> Absyn.RealTerm(v, Errormsg.none)
  | Absyn.StringTerm(s, _)         -> (transTermStr s)
  | Absyn.ConstantTerm(c, tyenv, _)-> (transTermConst c tyenv)
  | Absyn.FreeVarTerm(varInfo, _)  -> (transTermFreeVar varInfo)
  | Absyn.BoundVarTerm(varInfo, _) -> (transTermBoundVar varInfo bvs) 
  | Absyn.AbstractionTerm(abst, _) -> (transTermAbst bvs abst) 
  | Absyn.ApplicationTerm(app, _)  -> (transTermAppl bvs app)
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
    Absyn.StringTerm(strdata, Errormsg.none)
  | _ -> Errormsg.impossible Errormsg.none "transTermStr: invalid string rep"


(**************************************************************************)
(* transform constants:                                                   *)
(* TO ADD: rearrange type environment according to information provided   *)
(* by type reduction.                                                     *)
(**************************************************************************)
and transTermConst c tyenv =
  (*  a general list function needed to truncate type environments here *)
  let rec trunclist l n = 
  if n = 0 then []
    else 
      match l with
        (h::t) -> (h::(trunclist t (n-1)))
      | _ -> Errormsg.impossible Errormsg.none 
               "Parse.trunclist: invalid arguments."
  in
  let skeletonNeededness = 
  Option.get (Absyn.getConstantSkeletonNeededness c) 
  in
  let rec trimTypeEnvironment tyenv index newtyenv =
  match tyenv with
    [] -> List.rev newtyenv 
  | (ty :: rest) ->
      if Array.get skeletonNeededness index then
        trimTypeEnvironment rest (index + 1) ((transType ty) :: newtyenv)
      else
        trimTypeEnvironment rest (index + 1) newtyenv
  in
  if (Absyn.isPervasiveConstant c) then
    Absyn.ConstantTerm(c, List.map transType 
           (trunclist tyenv (Absyn.getConstantTypeEnvSize false c)),
           Errormsg.none)
  else
    Absyn.ConstantTerm(c, trimTypeEnvironment tyenv 0 [], Errormsg.none)
      
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
  match gListFind tysy qVars with
    | Some(qVarData) ->  qVarData (* body quantified *)
    | None -> 
      match gListFind tysy hqVars with
        | Some(hqVar) ->          (* exp head quant *)
            (match !hqVar with
               | Some(hqVarData) -> hqVarData 
               | None ->   (*exp head quant; but first encountered *)
                   let hqVarData = Absyn.makeNewVariableData () in
                     hqVar := Some(hqVarData);      (* update hqVars *)
                     hqVarData)
        | None -> 
            (* implicitly quantified at head of top-level clause or   *) 
            (* implicitly/explicitly quantified in embedding context of *)
            (* embedded clauses.                                        *)
            match (gListFind tysy tVars) with
                Some(varData) -> varData
              | None -> (* first encountered *)
                  let myVarData = Absyn.makeNewVariableData () in
                    gListAdd tysy myVarData tVars; (*add into tVars*)
                    myVarData
      
(**************************************************************************)
(* transform free variables:                                              *)
(* This variable could be implicitly quantified at the head of the top    *)
(* level clause or could be an explicitly quantified one that appears as  *)
(* the head of a flex goal.                                               *)
(**************************************************************************)
and transTermFreeVar var =
  match var with
    | Absyn.NamedFreeVar(tysy) ->
        Absyn.FreeVarTerm(Absyn.FreeVar(transTermVar tysy, ref None), 
            Errormsg.none)
    | _ -> Errormsg.impossible Errormsg.none 
             "transTermFreeVar: invalid var rep"

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
  | Absyn.NamedBoundVar(tysy) ->
    let rec ith tysys ind = match tysys with
      |  [] -> (ind, false)
      | (tysy' :: rest) -> 
        if (tysy' == tysy) then 
          (ind, true)
        else ith rest (ind + 1)
    in
    let (dbInd, found) = ith bvs 1 in
      if (found) then (* lambda-bound? *)
        Absyn.BoundVarTerm(Absyn.DBIndex(dbInd), Errormsg.none)
      else 
        Absyn.FreeVarTerm(Absyn.FreeVar(transTermVar tysy, ref None), 
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
    Absyn.AbstractionTerm(Absyn.UNestedAbstraction([], nabs, newbody), 
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
          nargs), Errormsg.none)
  | _ -> Errormsg.impossible Errormsg.none "transTermAppl: invalid app rep"

(** ********************************************************************** **)
(**                       PROCESS CLAUSES                                  **)
(** ********************************************************************** **)
(***************************************************************************)
(* INSERT A CLAUSE INTO PREDICATE DEFINITION LIST:                         *)
(***************************************************************************)
(* Add the clause into the defintition block of its head predicate; a new  *)
(* definition block is created if there exists none for the predicate, and *)
(* if the clause is at top-level, the new defintition block is associated  *)
(* with the predicate name constant itself too as its codeinfo.            *)
(* Note that it is assumed the clauses being processed by this function are*)
(* in an order reversed to their appearence in the program (their actual   *)
(* order), and are reversed again in the process of insertion.             *)
(***************************************************************************) 
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
    None -> (* has to create a new definition block *)
      let newClBlock = Absyn.makeNewClauseBlock clause closed in
      (* record the clause block into the code info field of the predicate *)
      (* name if top-level *)
      (if embedded then ()
      else Absyn.setConstantCodeInfo pred (Some (Absyn.Clauses newClBlock)));
      ((pred, newClBlock) :: clDefs) 
  | Some(cls, _, _, _) -> (* one exists: add one more clause *)
      (*let rec show_clause cls = 
  match cls with 
  [] -> ()
  | (cl :: rest) -> 
  print_string ("clauseName: " ^ 
  (Absyn.getConstantName (Absyn.getClausePred cl)) ^
  "\n");
  show_clause rest
  in*)
      cls := clause :: !cls;
      clDefs


(**************************************************************************)
(*               A PRE CLAUSE REPRESENTATION                              *)
(**************************************************************************)
(* (pred, term args, type args, number term args, number type args (, body)) *)
type preclause = 
    Fact of Absyn.aconstant * Absyn.aterm list * Absyn.atype list * int * int
  | Rule of Absyn.aconstant * Absyn.aterm list * Absyn.atype list * int * int
    * Absyn.agoal

(****************************************************************************)
(*                         PROCESS CLAUSE                                   *)
(****************************************************************************)
(*  Transform term representation of a clause into a pre clause paired      *)
(*  with the association lists of term variables and type variables         *)
(*  appearing free in the clause and the list of universal variables        *)
(*  explicitly quantified at the clause head.                               *)
(*  The main action carried out here is to recurse over the top-level for   *)
(*  all structures and records such variables into the global hqVars list.  *)
(****************************************************************************)
let rec processClause clauseTerm =
  (* collect variable data of explicitly head quantified variables that are*)
  (* used in the clause *)
  let collectHQVars hqVarAssocs =
  let rec collectHQVarsAux hqVarAssocs vars =
    match hqVarAssocs with
    [] -> vars
    | ((tysy, varInfo):: rest) ->
      match (!varInfo) with
      None -> collectHQVarsAux rest vars (* not used *)
      | Some(hqVarData) ->  collectHQVarsAux rest (hqVarData :: vars)
  in
  collectHQVarsAux hqVarAssocs []
  in
  (* function body of processClause *)
  match clauseTerm with
    Absyn.ConstantTerm(head, tyenv, _) -> (* proposition fact *)
      let (preClause, freeVars, freeTyVars) = processFact head tyenv [] 0 in
      (preClause, freeVars, freeTyVars, collectHQVars (!hqVars))
  | Absyn.ApplicationTerm(_) ->
      let func = Absyn.getTermApplicationHead clauseTerm in
      let args = Absyn.getTermApplicationArguments clauseTerm in
      let head = Absyn.getTermConstant func in
      if (Pervasive.isallConstant head) then
        let arg = List.hd args in 
        gListAdd (List.hd (Absyn.getTermAbstractionVars arg)) 
          (ref None) hqVars;
        processClause (Absyn.getTermAbstractionBody arg)
      else
        let (preClause, freeVars, freeTyVars) =
          if (Pervasive.isimplConstant head) then (* process rule *)
            processRule (List.hd (List.tl args)) (List.hd args)
          else (* process fact *)
            processFact head (Absyn.getTermMoleculeEnv func) args
              (Absyn.getTermApplicationArity clauseTerm)
        in
        (preClause, freeVars, freeTyVars, collectHQVars (!hqVars))
  | t ->
      (Errormsg.impossible (Absyn.getTermPos t) 
         "Processclauses.processClause: invalid clause term.")

(***************************************************************************)
(* process clause head:                                                    *)
(* Transform term arguments, type arguments of a clause head into a form   *)
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
  (Fact(pred, predArgs, predTyArgs, ntmargs + (Absyn.getConstantTypeEnvSize false pred),
    ntmargs), !tVars, !tyVars)

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
      Absyn.ConstantTerm(pred, tyenv, _) -> (pred, tyenv, [], 0) 
    | Absyn.ApplicationTerm(_) ->
      let head = Absyn.getTermApplicationHead clauseHead in
      (Absyn.getTermConstant head, Absyn.getTermMoleculeEnv head,
       Absyn.getTermApplicationArguments clauseHead, 
       Absyn.getTermApplicationArity clauseHead)
    | t ->
      (Errormsg.impossible
        (Absyn.getTermPos t)
        "Processclauses.processRule: invalid clause term.")
  in
  let (predArgs, predTyArgs) = processClauseHead args tyenv in
  let goal = processGoal clauseBody in
  (Rule(pred, predArgs, predTyArgs, (Absyn.getConstantTypeEnvSize false pred) + arity,
    arity, goal), !tVars, !tyVars)

(*****************************************************************************)
(* process a goal:                                                           *)
(* transform a goal from its term representation into its goal representation*)
(* Note that the goal that is introduced from closing off universal          *)
(* variable's defintions gets special treatment and representation.          *)
(*****************************************************************************)
and processGoal gltm = 
  match gltm with
    Absyn.ApplicationTerm(_) ->
      if (gltm == Pervasiveutils.cutFailTerm) then Absyn.CutFailGoal
      else 
  let head = Absyn.getTermApplicationHead gltm in
  let args = Absyn.getTermApplicationArguments gltm in
  (match head with
    Absyn.ConstantTerm(pred, _, _) ->
      if Pervasive.isandConstant pred then               (*and goal *)
        processAndGoal (List.hd args) (List.hd (List.tl args))
      else if Pervasive.issomeConstant pred then         (*some goal*)
        processSomeGoal (List.hd args) 
      else if Pervasive.isallConstant pred then          (*all goal *)
        processAllGoal (List.hd args)
      else if Pervasive.isimplConstant pred then         (*imp goal *)
        processImpGoal (List.hd args) (List.hd (List.tl args)) gltm
      else                                     (* rigid atomic goal *)
        processAtomicGoal gltm head 
    (Absyn.getTermApplicationArguments gltm)
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
    Absyn.FreeVarTerm(Absyn.NamedFreeVar(_), _) -> (* free var head *)
      Absyn.AtomicGoal(Pervasive.solveConstant, 1, 1, [(transTerm [] gltm)],[])
  | Absyn.ConstantTerm(pred, tyenv, _) ->
      Absyn.AtomicGoal(pred, 
                       arity + (Absyn.getConstantTypeEnvSize false pred),
                       arity,
                       List.map (transTerm []) args, List.map transType tyenv)
  | _ -> Errormsg.impossible Errormsg.none "processAtomicGoal: invalid pred"
  

(***************************************************************************)
(* process and goal:                                                       *)
(***************************************************************************)
and processAndGoal andl andr =
  let l = processGoal andl in
  let r = processGoal andr in
  Absyn.AndGoal(l, r);



(***************************************************************************)
(* process some goal:                                                      *)
(* The existential quantified variable is added into qVars as side effect, *)
(* and its variable data information is recorded with the goal structure   *)
(***************************************************************************)
and processSomeGoal goalBody =
  let tysy = List.hd (Absyn.getTermAbstractionVars goalBody) in
  let varData = Absyn.makeNewVariableData () in
  gListAdd tysy varData qVars; (* add into qVars *)
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

  (* recurse over for-all structures, and collect universally quantified *)
  (* variables into qVars.                                               *)  
  let rec processAllGoalAux goal hcPairs =
    match goal with
      Absyn.ApplicationTerm(_) ->
        let head = Absyn.getTermApplicationHead goal in
        let args = Absyn.getTermApplicationArguments goal in
        if (Absyn.isTermConstant head) && 
           (Pervasive.isallConstant (Absyn.getTermConstant head)) then
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
  processAllGoalAux (Absyn.getTermAbstractionBody goalBody) 
                    [collectHCpair tysy]


(****************************************************************************)
(* process implication goal:                                                *)
(* Actions carried out here are:                                            *)
(* a). transform the body goal;                                             *)
(* b). transform the clauses in the ancester and collect them into the      *)
(*     definition list for the implication goal;                            *)
(* c). collect list of term variables and type variables that should be     *)
(*     initialized upon entering the implication goal (those variables      *)
(*     having their scopes outside of the implication goal, but having      *)
(*     their first appearence inside the ancester of the goal.              *)
(****************************************************************************)
and processImpGoal clauseTerm goalTerm impGoal =
  (* recurse over the conjunctive structures *)
  let rec processImpClauses clauseTerm clauseDefs varInits tyVarInits =
    match clauseTerm with 
      Absyn.ApplicationTerm(_) -> 
        let args = Absyn.getTermApplicationArguments clauseTerm in
        let head = Absyn.getTermApplicationHead clauseTerm in
        if Absyn.isTermConstant head then
          if Pervasive.isandConstant (Absyn.getTermConstant head) then
            let (newClDefs, newVarInits, newTyVarInits) = 
            processImpClauses (List.hd args) clauseDefs varInits tyVarInits
            in
            processImpClauses (List.hd (List.tl args)) newClDefs newVarInits
                              newTyVarInits
          else
            processImpClause clauseTerm clauseDefs varInits tyVarInits impGoal
        else
          Errormsg.impossible
            (Absyn.getTermPos clauseTerm)
            "Processclauses.processImpGoal: invalid (flexible) clause head."
    | _ -> processImpClause clauseTerm clauseDefs varInits tyVarInits impGoal
  in

  let (clauseDefs, varInits, tyVarInits) = 
    processImpClauses clauseTerm [] [] [] 
  in
  Absyn.ImpGoal(Absyn.Definitions(clauseDefs), 
                Absyn.VarInits(List.rev varInits),
                Absyn.TypeVarInits(List.rev tyVarInits),
                processGoal goalTerm)

(********************************************************************)
(* processImpClause:                                                *)
(* Processing a clause embedded in an implication goal:             *)
(* a. transform into clause representation and insert it into the   *)
(*    definition list of the implication goal;                      *)
(* b. collect term and type variables with their scopes outside the *)
(*    clause but with first appearence inside;                      *)
(* c. generate mapping for type and term variables with their scopes*)
(*    outside the clause; the scopes of free type variables are     *)
(*    reflected by this mapping list: those occurring in the type   *)
(*    association of a free term variable are assumed to have the   *)
(*    same scopes the corresponding term variables and therefore    *)
(*    are collected in the type variable mapping if the term        *)
(*    variable has its scope outside; other free type variables     *)
(*    are assumed to have their scopes at the head of the embedded  *)
(*    clause.                                                       *)  
(********************************************************************)
and processImpClause clauseTerm clauseDefs varInits tyVarInits impGoal =
  (* process the embedded clause *)
  let (ltVars, ltyVars, lqVars, lhqVars, lembedded) =    (* bookkeeping *)
  (!tVars, !tyVars, !qVars, !hqVars, isEmbedded ()) 
  in
  gListsSet [] [] [] [];  (* set global (type) variable list to empty   *)
  setEmbedded true;       (* set embedded flag to true                  *)
  let (preClause, fvAssoc, tyfvAssoc, expHQVars) = processClause clauseTerm in
  
  (* collect (type) variable mapping information for this clause and    *)
  (* increment (type) variable initialization                           *) 
  gListsSet ltVars ltyVars lqVars lhqVars;   (* recover global lists    *)   
  setEmbedded lembedded;                     (* recover embedded flag   *)
  let (fvMaps, tyfvMaps, newVarInits, newTyVarInits) = 
    mapFreeVars fvAssoc tyfvAssoc varInits tyVarInits
  in
  let fvMaps'   = Absyn.TermVarMap(fvMaps)   in
  let tyfvMaps' = Absyn.TypeVarMap(tyfvMaps) in

  (* create clause representation *)
  let (pred, clause) =
  match preClause with
    Fact(pred, args, tyargs, nargs, ntargs) ->
    (pred, 
     Absyn.Fact(pred, args, tyargs, nargs, ntargs, fvMaps', tyfvMaps', 
          expHQVars, ref None, []))
  | Rule(pred, args, tyargs, nargs, ntargs, goal) ->
    (pred, 
     Absyn.Rule(pred, args, tyargs, nargs, ntargs, fvMaps', tyfvMaps', 
          expHQVars, ref None, goal, ref (Absyn.GoalEnvAssoc []), 
          ref None, ref false, []))
  in
  let closed = inClosedDefs pred impGoal in
  (* insert the new clause into the definition list of the impl goal *)
  (insertClause pred clause clauseDefs true closed, newVarInits, newTyVarInits)


(********************************************************************)
(* mapFreeVars:                                                     *)
(* Generate variable mappings for type and term variables free in   *)
(* the embedded clause. Accumulate variable and type variable       *)
(* initialization lists.                                            *)
(********************************************************************)
and mapFreeVars fvs tyfvs varInits tyVarInits =
  
  (******************************************************************)
  (* recurse over the given free variable (typesymbol,              *)
  (* variable data association) list and                            *)
  (* a). generate free variable mapping:                            *)
  (*     looking for the variable's typesymbol from the current     *)
  (*     qVar, hqVar and tyVar lists (which records the variables   *)
  (*     of the enclosing clause). If the variable is found in      *)
  (*     those lists and it does have an associated variable data   *)
  (*     then use the variable data to form a variable mapping.     *)
  (*     Otherwise, it implies the variable has a scope outside of  *)
  (*     the current (embedded) clause but yet has an occurrence,   *)
  (*     so that a new variable data is created for initialization, *)
  (*     and a variable mapping is formed with this newly created   *)
  (*     one.                                                       *)
  (* b). increment variable initialization list for those with      *)
  (*     scopes outside of the clause, but are not encountered      *)
  (*     in the enclosing clause yet. Namely, all the variable data *)
  (*     newly created in the previous step belongs to this category*)
  (*     and should be collected.                                   *)
  (* c). collect type variables appearing in the type environment   *)
  (*     of the variables that are free in the current clause: this *)
  (*     information is needed for deciding the scopes of type      *)
  (*     variables appearing free in the current clause in          *)
  (*     collectTyVarMaps.                                          *)
  (******************************************************************)
  let rec collectVarMaps fvs fvMaps varInits globalTyVars =
    match fvs with
      [] -> (fvMaps, varInits, globalTyVars)
    | ((tysy, toVarData) :: rest) ->	
        (* collect global type variables *)
        let newGlobalTyVars = 
          Types.freeTypeVars (Absyn.getTypeSymbolType tysy) globalTyVars
        in
        (* collect var map and var init *)
        let (fromVarData, newVarInits) =
          match gListFind tysy qVars with
              Some(qVarData) -> (qVarData, varInits) (* body quantified     *)
            | None ->
                match gListFind tysy hqVars with
                    Some(hqVarInfo) ->       (* exp head quantified *)
                      (match (!hqVarInfo) with  
                           Some(hqVarData) -> (hqVarData, varInits)
                         | None -> 
                             (*exp head quant with first occ in embedded cl:*)
                             (*should be initiated*)
                             let hqVarData = Absyn.makeNewVariableData () in
                               hqVarInfo := Some(hqVarData); (*update hqVars *)
                               (hqVarData, (hqVarData :: varInits)))
                  | None -> 
                      (* implicitly quantified at head of top-level clause or
                       * implicitly/explicitly quantified in embedding
                       * context of embedded clauses. *) 
                       match gListFind tysy tVars with
                          Some(varData) -> (varData, varInits)
                        | None -> 
                            let varData = Absyn.makeNewVariableData () in
                              gListAdd tysy varData tVars; (*update tyVars*)
                              (varData, 
                               if not (isEmbedded ()) then 
                                 (varData :: varInits )
                               else varInits)
        in
          collectVarMaps rest ((fromVarData, toVarData)::fvMaps) newVarInits
            newGlobalTyVars
  in
  
  (******************************************************************)
  (* recurse over the given free type variable (type - type variable*)
  (* data association) list: if the free type variable is not       *)
  (* contained in the feeding in globalTyVars list, then it has its *)
  (* scope just over the embedded clause, and needs not be          *)
  (* considered for type variable mapping and initialization;       *)
  (* otherwise, the following actions are carried out:              *)
  (* a). generate free type variable mapping:                       *)
  (*     look for the type variable through the current tyVars list *)
  (*     which is assumed to contain the encountered type variables *)
  (*     in the enclosing clause. If one is found, then use the     *)
  (*     associated type variable data to form a type variable      *)
  (*     mapping; otherwise, the type variable has outside scope    *)
  (*     but has first occurrence inside this embedded clause, and  *)
  (*     a new type variable data is created, and used to form a    *)
  (*     type variable mapping;                                     *)
  (* b). the type variable initialization list is incremented by    *)
  (*     those newly created type variable data in the previous step*)
  (******************************************************************)
  let rec collectTyVarMaps typeVars globalTyVars tyVarMaps tyVarInits =
    match typeVars with
        [] -> (tyVarMaps, tyVarInits)
      | ((tyVar, toTyVarData)::rest) ->
          if (List.mem tyVar globalTyVars) then
            let (fromTyVarData, newTyVarInits) =
              match gListFind tyVar tyVars with
                  Some(tyVarData) -> (tyVarData, tyVarInits)
                | None -> (* global, but not encountered outside yet *)
                    let tyVarData = Absyn.makeNewTypeVariableData () in
                      gListAdd tyVar tyVarData tyVars; (*update tyVars *)
                      (tyVarData, tyVarData :: tyVarInits)
            in
              collectTyVarMaps rest globalTyVars 
                ((fromTyVarData, toTyVarData) :: tyVarMaps) newTyVarInits
          else (* not really global *)
            collectTyVarMaps rest globalTyVars tyVarMaps tyVarInits
  in

  (* function body of mapFreeVars *)
  let (fvMaps, newVarInits, globalTyVars) = 
    collectVarMaps fvs [] varInits [] 
  in
  let (tyFvMaps, newTyVarInits) = 
    collectTyVarMaps tyfvs globalTyVars [] tyVarInits 
  in
    (fvMaps, tyFvMaps, newVarInits, newTyVarInits)

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
      (* transform from term representation to clause representation      *)
      gListsSet [] [] [] []; (* initialize global variable info lists *)
      setEmbedded false;     (* initialize embeddedClause flag        *)
      (* transfer a clause (varMap and tyVarMap for top-level modules are *)
      (* always empty                                                     *)
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
      (* collect this clause into the module definitions which is organized *)
      (* around predicate names.                                            *)
      let newClDefs = insertClause pred clause clauseDefs false anonymous in
        processTopLevelClauses rest impmods newClDefs anonymous	  

(** ********************************************************************** **)
(**                       INTERFACE FUNCTION                               **)
(** ********************************************************************** **)
let processClauses amod clTerms newClTerms closeddefs = 
  match amod with
    Absyn.Module(modname, modimps, modaccs, ctable, ktable, atable, _,
     gkinds, lkinds, gconsts, lconsts, hconsts, skels, hskels, _)
    ->
      let () = Errormsg.log Errormsg.none 
                 "Procesclauses.processClauses: processing clauses..." in
      setClosedDefs closeddefs;
      (* process anonymous clauses (those introduced for deorification), and*)
      (* increment them into the module definition list.                    *)
      (* Note: 1) the import module field of anonymous clauses should be    *)
      (*          empty;                                                    *)
      (*       2) the definitions of anonymous clauses are always closed    *)
      let clDefs = 
        processTopLevelClauses (List.rev newClTerms) [] [] true 
      in
      (* process clauses *)
      let newClDefs = processTopLevelClauses clTerms modimps clDefs false in
      (* Insert the clauses definitions and the string list into the module *)
      (* abstract syntax.                                                   *)
      let () = Errormsg.log Errormsg.none 
                 "Procesclauses.processClauses: processed clauses" in
        Absyn.Module(modname, modimps, modaccs, ctable, ktable, atable, !modStr, 
                     gkinds, lkinds, gconsts, lconsts, hconsts, skels, hskels, 
                     ref (Absyn.PreClauseBlocks(Absyn.Definitions(newClDefs))))
  | _ -> Errormsg.impossible Errormsg.none "processClauses: invalid module"
