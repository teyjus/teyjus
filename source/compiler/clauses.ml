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

type symbol = Symbol.symbol
type pos = Errormsg.pos

(* used for record hidden constant and implication goal pairs which *)
(* indicate the position where its definition should be closed.     *)
type closeddefinition = (Absyn.aconstant * Absyn.aterm)

let closedDefs = ref []
let initClosedDefs () = closedDefs := []
let getClosedDefs ()  = !closedDefs
let addClosedDef  defPair = closedDefs := (defPair :: !closedDefs)


type universalvardefinition = (Absyn.atypesymbol * Absyn.aterm list ref)
let getUVDefDefs = function
  (_, defs) -> defs
let getUVDefTypeSymbol = function
  (ts, _) -> ts

type newclause = NewClause of (Absyn.aconstant * Absyn.aterm list)

let getNewClauseConstant = function
  NewClause(c,_) -> c

let getNewClauseClause = function
  NewClause(_,c) -> c

type hiddenconstant =
  HiddenConstant of (Absyn.aconstant * Absyn.atypesymbol)

let getHiddenConstantConstant = function
  HiddenConstant(c,_) -> c

type deorifygoalresult =
  DeOrifyGoalResult of 
    (Absyn.aterm list *             (* The goals *)
     Absyn.atypesymbol list *       (* The free variables *)
     universalvardefinition list *  (* Universal definitions *)
     bool *                         (* Is there a cut? *)
     newclause list *               (* The new clauses *)
     hiddenconstant list)           (* The hidden constants *)

let getDeOrifyGoalResultGoals = function
  DeOrifyGoalResult(g,_,_,_,_,_) -> g
let getDeOrifyGoalResultFVS = function
  DeOrifyGoalResult(_,fvs,_,_,_,_) -> fvs
let getDeOrifyGoalResultUVDefs = function
  DeOrifyGoalResult(_,_,uvdefs,_,_,_) -> uvdefs
let getDeOrifyGoalResultHasCut = function
  DeOrifyGoalResult(_,_,_,hc,_,_) -> hc
let getDeOrifyGoalResultNewClauses = function
  DeOrifyGoalResult(_,_,_,_,nc,_) -> nc
let getDeOrifyGoalResultHiddenConstants = function
  DeOrifyGoalResult(_,_,_,_,_,hc) -> hc

type deorifyclauseresult =
  DeOrifyClauseResult of (Absyn.aterm list * Absyn.atypesymbol list *
    universalvardefinition list * newclause list * hiddenconstant list)

let getUniversalDefinitionTypeSymbol = function
  (tsym,_) -> tsym

(**********************************************************************
*illegalConstant:
* Determines whether the given constant is illegal in the arguments
* to a predicate; this is the case if the constant represents an
* implication ("=>" or ":-").  If this is the case, an error message
* is printed using the given position information.
**********************************************************************)
let illegalConstant c pos =
  if c == Pervasive.implConstant || c == Pervasive.colondashConstant then
    (Errormsg.error pos ("symbol " ^ (Absyn.getConstantPrintName c) ^
      " cannot be embedded in predicate arguments");
    true)
  else
    false

(********************************************************************
*union:
* Returns the union of two lists of free variables.  Takes care
* to ensure that no duplicates are introduced.
********************************************************************)
let union fvs1 fvs2 =
  let rec union_aux fvs2 result =
    match fvs2 with 
        [] -> List.rev result
      | (fv :: rest) ->
          if (List.memq fv fvs1) then 
            union_aux rest result
          else 
            union_aux rest (fv::result)
  in
    fvs1 @ (union_aux fvs2 [])

(********************************************************************
*makeHiddenConstant:
********************************************************************)
let makeHiddenConstant tsym =
  let ty = Absyn.getTypeSymbolType tsym in
  let mol = Types.skeletonizeType ty in
  let ty' = Types.getMoleculeType mol in
  (*let envsize = List.length (Types.getMoleculeEnvironment mol) in*)
  let skel = Absyn.makeSkeleton ty' in
  let c = Absyn.makeHiddenConstant skel 0 in
    ((Absyn.getTypeSymbolHiddenConstantRef tsym) := Some c;
     HiddenConstant(c,tsym))
  
(********************************************************************
*newHead:
* Creates a predicate name and a clause head for a list of clauses
* corresponding to a disjunctive goal. Skeletonization here is okay
* since the type skeleton will not be needed again for such predicates.
********************************************************************)
let rec newHead fvs : Absyn.aconstant * Absyn.aterm =
  let rec argumentsAndTypes fvs =
    match fvs with
      [] -> ([], [])
    | f::fs ->
        let arg = Absyn.makeBoundVarTerm f Errormsg.none in
        let ty = Absyn.getTypeSymbolType f in
        let (args, tys) = argumentsAndTypes fs in
        (arg::args, ty::tys)
  in
  
  if List.length fvs = 0 then
    let ty = Absyn.makeKindType Pervasive.kbool in
    let skel = Absyn.makeSkeleton ty in
    let c = Absyn.makeAnonymousConstant 0 skel in
    let t = Absyn.ConstantTerm(c, [], Errormsg.none) in
    (c, t)
  else
    let (args, argtys) = argumentsAndTypes fvs in
    let ty = Absyn.makeArrowType (Absyn.makeKindType Pervasive.kbool) argtys in
    let _ = Errormsg.log Errormsg.none 
              ("Clauses.newHead: type: " ^ (Absyn.string_of_type_ast ty)) in
    let tymol = Types.skeletonizeType ty in
    let env = (Types.getMoleculeEnvironment tymol) in
    let skel = Absyn.makeSkeleton ty in
    let c = Absyn.makeAnonymousConstant (List.length env) skel in
    let t = Absyn.ConstantTerm(c, env, Errormsg.none) in
    let t' = 
      Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(t, List.rev args, List.length args), 
        Errormsg.none) in
      (c, t')

(********************************************************************
*newPredicate:
* Creates a list of new clauses from the given list of goals. The
* goals represent a disjunction.
********************************************************************)
and newPredicate goals fvs newclauses : Absyn.aterm * newclause list =
  let (pred, head) = newHead fvs in
  let clauses = reverseDistributeImplication head goals [] in
  let newclause = NewClause(pred, clauses) in
    (head, newclause::newclauses)

(********************************************************************
*newPredicateFromAnd:
* Creates a list of new clauses from two lists of 'and' goals, where
* each list represents a disjunction.
********************************************************************)
and newPredicateFromAnd goals1 goals2 fvs newclauses : Absyn.aterm * newclause list =
  let (pred, head) = newHead fvs in
  let clauses = reverseDistributeImplication head goals2 [] in
  let clauses' = reverseDistributeImplication head goals1 clauses in
  let newclause = NewClause(pred, clauses') in
    (head, newclause::newclauses)

(********************************************************************
*getGoal:
* If the andgoal is not None, the head should actually be an
* application, with and ("," or "&") at the head, and the
* original head and the andgoal as arguments.
********************************************************************)
and getGoal head andgoal : Absyn.aterm =
  if Option.isSome andgoal then
    let andgoal' = Option.get andgoal in
    let head' = Absyn.ApplicationTerm(
      Absyn.FirstOrderApplication(
        Pervasive.andTerm,
        [head; andgoal'],
        2),
      Absyn.getTermPos head) in
    head'
  else
    head

(********************************************************************
*closeUniversalDefinition:
* Adding closure clauses for a universally quantified variable. Such
* clauses are needed only if uvdefs contains an entry for the variable.
* If it does, then the entry takes the form of a list of implication
* goals whose antecedents must be added to. At the end, uvdefs must be
* shorn of such an entry.
********************************************************************)
and closeUniversalDefinitions tsym uvdefs =
  let rec getAndDrop tsym uvdefs uvdefs' =
  match uvdefs with
    [] -> (None, List.rev uvdefs')
  | ((uv, defs) as uvdef :: rest) ->
    if tsym == uv then 
      (Some defs, (List.rev uvdefs') @ rest)
    else
      getAndDrop tsym rest (uvdef :: uvdefs')
  in

  let rec addClosedDefinitions hc defs =
  match defs with
    [] -> ()
  | (def :: rest) -> addClosedDef (hc, def); addClosedDefinitions hc rest
  in

  (*let addClosedDefinitions hc impgoal = addClosedDef (hc, impgoal) in*)

  (* function body of closeUniversalDefintions *)
  let (defs, newuvdefs) = getAndDrop tsym uvdefs [] in
  if Option.isNone defs then newuvdefs
  else
  let defs' = Option.get defs in
  if (List.length (!defs')) <= 0 then newuvdefs (* needed? *)
  else (* now has to record into the closedef list *)
    let hc = Absyn.getTypeSymbolHiddenConstant tsym in
   (* List.iter (addClosedDefinitions hc) (!defs'); *)
    addClosedDefinitions hc (!defs'); 
    newuvdefs
    
     
(********************************************************************
*collectFreeVars:
* Accumulates the free variables in a term onto the given fvs list.
* Transforms applications to Boehm form, and checks the legality of
* constants in the term.
********************************************************************)
and collectFreeVars term ?(toplevel=false) fvs bvs : Absyn.aterm * (Absyn.atypesymbol list) =
  match term with
    Absyn.IntTerm(_) -> (term, fvs)
  | Absyn.RealTerm(_) -> (term, fvs)
  | Absyn.StringTerm(_) -> (term, fvs)
  | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tsym),pos) ->
      if not (List.memq tsym fvs) then
        (term, tsym::fvs)
      else
        (term, fvs)
  | Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym), pos) ->
      if not ((List.memq tsym bvs) || (List.memq tsym fvs)) then
        (term, tsym::fvs)
      else
        (term, fvs)
  | Absyn.ConstantTerm(c,_,pos) ->
     (* Query (toplevel) terms may have implications *)
     if (not(toplevel) && (illegalConstant c pos)) then
        (Absyn.errorTerm, [])
      else
        (term, fvs)
  | Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym, body),pos) ->
     (* Fixed a typo: tsym should be added to bound variables
      * (This is what Parse.fixTerm does) -- NG *)
     let (body', fvs') = collectFreeVars body ~toplevel fvs (tsym::bvs) in
     let term' = Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym, body'), pos) in
     (term', fvs')
  | Absyn.ApplicationTerm(_) ->
     let pos = Absyn.getTermPos term in
     let (head, args) = Absyn.getTermApplicationHeadAndArguments term in
     let (head', fvs') = collectFreeVars head ~toplevel fvs bvs in
     let (args', fvs'') = collectArgsFreeVars args ~toplevel fvs' bvs in
     let term' = Absyn.ApplicationTerm(
                     Absyn.FirstOrderApplication(
                         head',
                         args',
                         List.length args'),
                     pos) in
     (term', fvs'')
  | Absyn.ErrorTerm -> (term, fvs)   
  | _ -> Errormsg.impossible Errormsg.none "Clauses.collectFreeVars: Invalid term type."

(**********************************************************************
*collectArgsFreeVars:
* Collect the free variables of a list of terms, and return a list
* of rectified items, and the fvs.
**********************************************************************)
and collectArgsFreeVars args ?(toplevel=false) fvs bvs =
  match args with
    [] -> ([], fvs)
  | a::aa ->
      let (args', fvs') = collectArgsFreeVars aa ~toplevel fvs bvs in
      let (arg', fvs'') = collectFreeVars a ~toplevel fvs' bvs in
      (arg'::args', fvs'')

(**********************************************************************
*distributeAndGoal:
* Distributing an AND goal over a list of goals and appending the result
* to the list given in othergoals.
**********************************************************************)
and distributeAndGoal goals othergoals andgoal =
  (********************************************************************
  *distribute:
  * Conjoining a goal on the right to each goal in a list of goals,
  * with consideration for andgoal.
  ********************************************************************)
  let rec distribute goals othergoals andgoal =
    match goals with
      [] -> othergoals
    | g::gs ->
        (getGoal g andgoal) :: (distribute gs othergoals andgoal)
  in
  
  if (Option.isNone andgoal) && (List.length othergoals = 0) then
    goals
  else
    (distribute goals othergoals andgoal)

(**********************************************************************
*distributeQuantifierTerms:
* Distributing a quantifier given by qterm over a variable given by tsym
* over a list of goals and then forming a conjunction with andgoal.
**********************************************************************)
and distributeQuantifierTerms qterm tsym goals andgoal =
  match goals with
    [] -> []
  | g::gs ->
      let goal =
        Absyn.ApplicationTerm(
          Absyn.FirstOrderApplication(
            qterm,
            [Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym, g), Errormsg.none)],
            1),
          Errormsg.none) in
      
      let goal' = getGoal goal andgoal in
      goal' :: (distributeQuantifierTerms qterm tsym gs andgoal)
 
(**********************************************************************
*distributeUniversal:
* Insert a universal quantification over vtysy over each of the terms
* in myterms.
**********************************************************************)
and distributeUniversal term tsym terms =
  let rec distribute' term terms =
    match terms with
      [] -> []
    | t::ts ->
        let newterm = Absyn.ApplicationTerm(
          Absyn.FirstOrderApplication(term,
            [Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym, t), Errormsg.none)], 1),
          Errormsg.none) in
        (newterm :: (distribute' term ts))
  in
  
  distribute' term terms

(**********************************************************************
*reverseDistributeImplication:
**********************************************************************)
and reverseDistributeImplication head goals cls =
  match goals with
    [] -> cls
  | _ ->
    let goal = List.hd goals in
    let goals' = List.tl goals in
    
    let appinfo = 
      Absyn.FirstOrderApplication(
        Absyn.makeConstantTerm (Pervasive.implConstant) [] 
          Errormsg.none, [goal;head], 2) in
    let cls' = (Absyn.ApplicationTerm(appinfo, Errormsg.none)) :: cls in
      reverseDistributeImplication head goals' cls'

(**********************************************************************
*distributeImplication:
* Produces a new list of normalized clauses by adding an implication over
* each of the normalized clauses in cls with the antecedent of each
* implication being goal.
**********************************************************************)
and distributeImplication goal cls result =
  (********************************************************************
  *distribute:
  * Distribute a single implication.
  ********************************************************************)
  let rec distribute goal clause =
    let regularImplication () =
      let imply = Pervasive.implicationTerm in
      Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(imply, [goal; clause], 2), Errormsg.none)
    in
    
    match clause with
      Absyn.ApplicationTerm(Absyn.FirstOrderApplication(f, args, _), pos) ->
        (match f with
          Absyn.ConstantTerm(c,_,_) ->
            if c = Pervasive.allConstant then
              let arg1 = List.hd args in
              let tsym = Absyn.getTermAbstractionVar arg1 in
              let body = Absyn.getTermAbstractionBody arg1 in
              let pos' = Absyn.getTermPos body in
              Absyn.ApplicationTerm(
                Absyn.FirstOrderApplication(
                  f,
                  [Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym, distribute goal body), pos')],
                  1),
                Errormsg.none)
            else if c = Pervasive.implConstant then
              let arg1 = List.hd args in
              let arg2 = List.hd (List.tl args) in
              let inner = Absyn.ApplicationTerm(
                Absyn.FirstOrderApplication(
                  Pervasive.andTerm,
                  [arg1; goal],
                  2),
                Errormsg.none) in
              Absyn.ApplicationTerm(
                Absyn.FirstOrderApplication(
                  f, 
                  [inner; arg2],
                  2),
                Errormsg.none)
            else
              regularImplication ()
        | _ -> regularImplication ())
    | _ -> regularImplication ()
  in
  
  match cls with
    [] -> (result : Absyn.aterm list)
  | c::cs ->
      (distribute goal c)  :: (distributeImplication goal cs result)


(**********************************************************************
*deOrifyGoal:
* The returned value brings back a list of disjunctions forming goal if
* wholegoal is true and a disjunction free form otherwise.
**********************************************************************)
and deOrifyGoal goal uvs uvdefs andgoal wholegoal newclauses hcs =
  let (t, args) = Absyn.getTermApplicationHeadAndArguments goal in  
  match t with
    Absyn.ConstantTerm(c,_,pos) ->
      if c = Pervasive.allConstant then
        let arg1 = List.hd args in
        (deOrifyUniversalGoal t arg1 uvs uvdefs andgoal wholegoal newclauses hcs)
      else if c = Pervasive.someConstant then
        let arg1 = List.hd args in
        (deOrifyExistentialGoal t arg1 uvs uvdefs andgoal wholegoal newclauses hcs)
      else if c = Pervasive.orConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (deOrifyOrGoal arg1 arg2 uvs uvdefs andgoal wholegoal newclauses hcs)
      else if c = Pervasive.andConstant || c = Pervasive.ampandConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (deOrifyAndGoal arg1 arg2 uvs uvdefs andgoal wholegoal newclauses hcs)
      else if c = Pervasive.implConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (deOrifyImplicationGoal arg1 arg2 uvs uvdefs andgoal false newclauses hcs)
      else if c = Pervasive.colondashConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (deOrifyImplicationGoal arg2 arg1 uvs uvdefs andgoal false newclauses hcs)
      else if c = Pervasive.cutConstant then
        (deOrifyCutGoal goal andgoal uvdefs newclauses hcs)
      else
        (deOrifyAtomicGoal t args andgoal uvs uvdefs newclauses hcs)
  | _ -> (deOrifyAtomicGoal t args andgoal uvs uvdefs newclauses hcs)

(**********************************************************************
*deOrifyCutGoal:
* Deorifies a cut goal.  Records that the goal has a cut, but is
* otherwise routine.
**********************************************************************)
and deOrifyCutGoal goal andgoal uvdefs newclauses hcs =
  DeOrifyGoalResult([getGoal goal andgoal], [], uvdefs, true, newclauses, hcs)

(**********************************************************************
*deOrifyUniversalGoal:
* The uv list must be added to, the body processed, each AND goal list
* returned should be collapsed into a single goal with a universal
* quantifier ranging over it and then conjoined with the andgoals. Finally,
* 'closure' clauses must be added for possible definitions pertaining to 
* the universally quantified variable and the free variable list should
* be pruned.
**********************************************************************)
and deOrifyUniversalGoal t arg1 uvs uvdefs andgoal wholegoal newclauses hcs =
  let (body, tsym) = etaFluffQuantifier t arg1 in
  let hc = makeHiddenConstant tsym in
  let uvs' = tsym :: uvs in
  let DeOrifyGoalResult(goals', fvs', uvdefs', hascut', newclauses', hcs') = 
    deOrifyGoal body uvs' uvdefs None wholegoal newclauses hcs in
  let hcs'' = hc :: hcs' in
  let uvdefs'' = closeUniversalDefinitions tsym uvdefs' in 
  if List.memq tsym fvs' then
    let fvs'' = (List.filter ((<>) tsym) fvs') in
    let goals'' = distributeQuantifierTerms t tsym goals' andgoal in 
    DeOrifyGoalResult(goals'', fvs'', uvdefs'', hascut', newclauses', hcs'')
  else
    let goals'' = distributeAndGoal goals' [] andgoal in
      DeOrifyGoalResult(goals'', fvs', uvdefs'', hascut', newclauses', hcs'')

(**********************************************************************
*deOrifyExistentialGoal:
**********************************************************************)
and deOrifyExistentialGoal t arg1 uvs uvdefs andgoal wholegoal newclauses hcs =
  let (body, tsym) = etaFluffQuantifier t arg1 in
  let DeOrifyGoalResult(goals', fvs', uvdefs', hascut', newclauses', hcs') = 
  deOrifyGoal body uvs uvdefs None wholegoal newclauses hcs 
  in
  
  if List.memq tsym fvs' then
    let fvs'' = (List.filter ((<>) tsym) fvs') in
    let goals'' = distributeQuantifierTerms t tsym goals' andgoal in
    DeOrifyGoalResult(goals'', fvs'', uvdefs', hascut', newclauses', hcs')
  else
    let goals'' = distributeAndGoal goals' [] andgoal in
    DeOrifyGoalResult(goals'', fvs', uvdefs', hascut', newclauses', hcs')

(**********************************************************************
*deOrifyImplicationGoal:
* Transforming an implication goal. Iterated implications at the top-level
* must be converted into conjunctions in the antecedent. Then the antecedent
* must be normalized and dedisjunctified as a clause. Next, the consequent
* must be dedisjunctified as a goal. Finally, the implication must be
* reconstructed. As an auxiliary effect, the newly constructed implication
* goal must become the sole implication goal for all the uvs that have
* definitions in the goal; this value is to be accumulated into the incoming
* value of uvdefs and returned in the uvdefs part of the return value.
**********************************************************************)
and deOrifyImplicationGoal 
      clause goal uvs uvdefs andgoal wholegoal newclauses hcs =
  let empty = function
      [] -> true
    | _::_ -> false
  in
  (********************************************************************
  *normalizeGoal:
  *	Given the antecedent and the consequent of an implication goal,
  *	replaces top-level implications in the consequent with conjunctions
  *	in the antecedent.
  ********************************************************************)
  let rec normalizeGoal clause goal =
    let rec normalize clause goal =
      let (t, args) = Absyn.getTermApplicationHeadAndArguments goal in
      if (Absyn.isTermConstant t) then
        let c = Absyn.getTermConstant t in
        if not ((c = Pervasive.implConstant) ||
          (c = Pervasive.colondashConstant)) then
          (clause, goal)
        else if (c = Pervasive.implConstant) then
          let newcl = List.hd args in
          let goal' = List.hd (List.tl args) in
          let clause' = Absyn.ApplicationTerm(
            Absyn.FirstOrderApplication(
              Pervasive.andTerm,
              [newcl; clause],
              2),
            Errormsg.none) in
          normalize clause' goal'
        else if (c = Pervasive.colondashConstant) then
          let newcl = List.hd (List.tl args) in
          let goal' = List.hd args in
          let clause' = Absyn.ApplicationTerm(
            Absyn.FirstOrderApplication(
              Pervasive.andTerm,
              [newcl; clause],
              2),
            Errormsg.none) in
          normalize clause' goal'
        else
          (Errormsg.impossible Errormsg.none 
             "Clauses.normalize: should be unreachable")
      else
        (clause, goal)
    in
    (normalize clause goal)
    
  (********************************************************************
  *addUVDefs:
  * Merge the given list of UV defs.
  ********************************************************************)
  and addUVDefs tsyms uvs =
    match uvs with
      [] -> tsyms
    | u::uvs' ->
        let tsym = getUVDefTypeSymbol u in
        if not (List.memq tsym tsyms) then
          addUVDefs (tsym::tsyms) uvs'
        else
          (* addUVDefs tsyms uvs *)
      addUVDefs tsyms uvs'

  (********************************************************************
  *addNewUVDefs:
  * Merge the given list of UV defs.
  ********************************************************************)
  and addNewUVDefs (tsyms : Absyn.atypesymbol list) goal 
            (uvdefs : universalvardefinition list) =
    match tsyms with
      [] -> uvdefs
    | t::tsyms' ->
        try
          let defs = List.assq t uvdefs in
          (defs := (goal)::(!defs);
          addNewUVDefs tsyms' goal uvdefs)
        with
      Not_found ->
      addNewUVDefs tsyms' goal ((t, ref [goal])::uvdefs)

     (* print_endline ("tysy name: " ^ Absyn.getTypeSymbolName t); *)
     (* Errormsg.impossible Errormsg.none "Clauses.addNewUVDefs: unbound uv."*)
  
  (********************************************************************
  *andifyList:
  * Form a conjunction of a list of terms.
  ********************************************************************)
  and andifyList glist =
    match glist with
      [] -> (Errormsg.impossible Errormsg.none 
               "Clauses.andifyList: empty list.")
    | g::[] -> g
    | g::gs ->
        Absyn.ApplicationTerm(
          Absyn.FirstOrderApplication(
            Pervasive.andTerm,
            [g; (andifyList gs)],
            2),
          Errormsg.none)
  in
  
  let (clause, goal) = normalizeGoal clause goal in
  let defuvs = ref [] in
  let clauses = (normalizeClause clause [] uvs defuvs true) in
  let defuvs = !defuvs in
  let DeOrifyClauseResult(clauses', fvs', uvdefs', newclauses', hcs') = 
    deOrifyClauses clauses [] uvs newclauses hcs in
  let defuvs = addUVDefs defuvs uvdefs' in 
  let DeOrifyGoalResult(goals', fvs, uvdefs', hascut, newclauses'', hcs'') = 
    deOrifyGoal goal uvs [] None false newclauses' hcs' in
  let defuvs = addUVDefs defuvs uvdefs' in
  if (not (empty clauses')) && (not (empty goals')) then
    let cl = andifyList clauses' in
    let goal = List.hd goals' in
    let goal' = Absyn.ApplicationTerm(
      Absyn.FirstOrderApplication(
        Pervasive.implicationTerm, [cl;goal], 2),
      Errormsg.none) in
    
    let goal'' = getGoal goal' andgoal in
    let uvdefs' = addNewUVDefs defuvs goal'' uvdefs in
    
    DeOrifyGoalResult([goal''], union fvs' fvs, uvdefs', 
                      hascut, newclauses'', hcs'')
  else
    DeOrifyGoalResult([], [], [], false, newclauses'', hcs')


(**********************************************************************
*deOrifyAtomicGoal:
* For atomic goals other than cut. If the head is a uv, it must be replaced
* with a constant term made from the appropriate hidden constant. Otherwise,
* if it is a bound or free var, it counts as a free variable occurrence.
* Finally, arguments have to be checked for correctness, they have to be
* rectified and their free variables collected.
**********************************************************************)
and deOrifyAtomicGoal head args andgoal uvs uvdefs newclauses hcs =
  let deorify' head hascut fvs uvdefs =
    match args with
      [] ->
        DeOrifyGoalResult([getGoal head andgoal], fvs, uvdefs, 
                          hascut, newclauses, hcs)
    | _ ->
       let (args', fvs') = collectArgsFreeVars args fvs [] in
        let head' = Absyn.ApplicationTerm(
          Absyn.FirstOrderApplication(
            head,
            args',
            List.length args'),
          Absyn.getTermPos head) in
        DeOrifyGoalResult([(getGoal head' andgoal)], fvs', 
                          uvdefs, hascut, newclauses, hcs)
  in
  
  match head with
    Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym),pos) ->
      if (List.memq tsym uvs) then
        let c = Absyn.getTypeSymbolHiddenConstant tsym in
        let head' = Absyn.ConstantTerm(c, [],  pos) in
        (deorify' head' false [] uvdefs)
      else
        let head' = Absyn.FreeVarTerm(Absyn.NamedFreeVar(tsym), pos) in
        (deorify' head' false [tsym] uvdefs)
  | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tsym),pos) ->
      deorify' head false [tsym] uvdefs
  | _ -> deorify' head false [] uvdefs

(**********************************************************************
*deOrifyOneOrGoal:
* Processing one disjunct of a disjunctive goal. If this branch has a cut,
* then a new predicate must be introduced to prevent the cut from cutting off
* choice points in the parent clause.
**********************************************************************)
and deOrifyOneOrGoal goal uvs uvdefs andgoal newclauses hcs =
  let DeOrifyGoalResult(goals', fvs', uvdefs', hascut', newclauses', hcs') =
    (deOrifyGoal goal uvs uvdefs andgoal true newclauses hcs) in
  
  if hascut' then
    let (goal', newclauses') = newPredicate goals' fvs' newclauses in
    DeOrifyGoalResult([goal'], fvs', uvdefs',hascut', newclauses', hcs')
  else
    DeOrifyGoalResult(goals', fvs', uvdefs',hascut', newclauses', hcs')

(**********************************************************************
*deOrifyOrGoal:
* Transformation of an OR goal. Process each disjunct getting a list of
* goals. Then, if the disjunction is to permeate to the whole goal, join the
* lists obtained from processing the disjuncts and distribute the incoming
* and goal over the result. Otherwise collapse the disjunction into one branch
* by defining a new predicate. Note that the order of processing subgoals must
* be reversed to get the right (reverse) ordering for clauses. Also upward
* propogation of cuts stops here
**********************************************************************)
and deOrifyOrGoal goal1 goal2 uvs uvdefs andgoal wholegoal newclauses hcs =
  let getAndGoal wholegoal andgoal =
    if wholegoal then
      andgoal
    else
      None
  in
  let andgoal' = (getAndGoal wholegoal andgoal) in
  
  let result1 = deOrifyOneOrGoal goal2 uvs uvdefs andgoal' newclauses hcs in
  
  let newclauses' = getDeOrifyGoalResultNewClauses result1 in
  let uvdefs' = getDeOrifyGoalResultUVDefs result1 in
  let hcs' = getDeOrifyGoalResultHiddenConstants result1 in
  let result2 = deOrifyOneOrGoal goal1 uvs uvdefs' andgoal' newclauses' hcs' in
  
  let newclauses'' = getDeOrifyGoalResultNewClauses result2 in
  let hcs'' = getDeOrifyGoalResultHiddenConstants result2 in
  let fvs' = union 
               (getDeOrifyGoalResultFVS result1) 
               (getDeOrifyGoalResultFVS result2) in
  let uvdefs' = getDeOrifyGoalResultUVDefs result2 in
  
  if wholegoal then
    let goals1 = getDeOrifyGoalResultGoals result1 in
    let goals2 = getDeOrifyGoalResultGoals result2 in
    DeOrifyGoalResult(goals1 @ goals2, fvs', uvdefs', false, 
                      newclauses'', hcs'')
  else
    let (pred, newclauses''') = newPredicateFromAnd
      (getDeOrifyGoalResultGoals result2)
      (getDeOrifyGoalResultGoals result1)
      fvs'
      newclauses'' in

    if Option.isSome andgoal then
      let goal' = Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(
          Pervasive.andTerm,
          [pred; Option.get andgoal],
          2),
        Errormsg.none) in
      DeOrifyGoalResult([goal'], fvs', uvdefs', false, newclauses''', hcs'')
    else
      DeOrifyGoalResult([pred], fvs', uvdefs', false, newclauses''', hcs'')

(**********************************************************************
*deOrifyAndGoal:
**********************************************************************)
and deOrifyAndGoal goal1 goal2 uvs uvdefs andgoal wholegoal newclauses hcs =
  let getAndGoal goals =
    match goals with
      [] -> None
    | g::gs -> Some g
  in
  
  let result1 = deOrifyGoal goal2 uvs uvdefs andgoal false newclauses hcs in
  let r1goals = getDeOrifyGoalResultGoals result1 in
  let hcs' = getDeOrifyGoalResultHiddenConstants result1 in
  let andgoal' = getAndGoal r1goals in
  
  let uvdefs' = getDeOrifyGoalResultUVDefs result1 in
  let newclauses' = getDeOrifyGoalResultNewClauses result1 in
  
  let result2 = deOrifyGoal goal1 uvs uvdefs' andgoal' 
                  wholegoal newclauses' hcs' in
  let fvs' = union 
               (getDeOrifyGoalResultFVS result1)
               (getDeOrifyGoalResultFVS result2) in
  let hcs'' = getDeOrifyGoalResultHiddenConstants result2 in
  let hascut' = (getDeOrifyGoalResultHasCut result1) || 
                (getDeOrifyGoalResultHasCut result2) in
  let uvdefs' = getDeOrifyGoalResultUVDefs result2 in
  let newclauses'' = getDeOrifyGoalResultNewClauses result2 in
  
  DeOrifyGoalResult(getDeOrifyGoalResultGoals result2, fvs', 
                    uvdefs', hascut', newclauses'', hcs'')

(**********************************************************************
*deOrifyClause:
* Removes disjunctions from a clause.
**********************************************************************)
and deOrifyClause term currentclauses fvs uvs uvdefs newclauses hcs =
  let checkClauseArguments args fvs =
    let rec check' args fvs =
      match args with
        [] -> ([], fvs)
      | a::aa ->
          let (args', fvs') = check' aa fvs in
          let (arg', fvs'') = collectFreeVars a fvs' [] in
          (arg'::args', fvs'')
    in
    check' args fvs
  in
  match term with
    (*  An application  *)
    Absyn.ApplicationTerm(_, pos) ->
      let (f, aa) = Absyn.getTermApplicationHeadAndArguments term in
      let arg = List.hd aa in
      let args = List.tl aa in
      
      (*  atomicHead: regular head with arguments.  *)
      let atomicHead () =  
        let (args', fvs') = checkClauseArguments (arg::args) fvs in
        let newclause = 
          Absyn.ApplicationTerm(
            Absyn.FirstOrderApplication(f, args', List.length args'),
            pos) in
          DeOrifyClauseResult(newclause::currentclauses, fvs', uvdefs, 
                              newclauses, hcs)
      in

      (match f with
        Absyn.ConstantTerm(c,_,_) ->
          if c = Pervasive.allConstant then
            let tsym = Absyn.getTermAbstractionVar arg in
            let DeOrifyClauseResult(clauses', fvs', uvdefs', newclauses', hcs') =
              deOrifyClause (Absyn.getTermAbstractionBody arg) [] fvs uvs uvdefs newclauses hcs in
            let clauses'' = distributeUniversal f tsym clauses' in
            let fvs'' = List.filter ((!=) tsym) fvs' in
              DeOrifyClauseResult(clauses'' @ currentclauses, fvs'', uvdefs', newclauses', hcs') 
              (* DeOrifyClauseResult(clauses'', fvs'', uvdefs', newclauses', hcs') *)
            
          else if c = Pervasive.implConstant then

            let DeOrifyGoalResult(goals, fvs', uvdefs', hascut', newclauses', hcs') =
              deOrifyGoal arg uvs uvdefs None false newclauses hcs in		   
            let DeOrifyClauseResult(clauses', fvs'', uvdefs'', newclauses'', hcs'') =
              deOrifyClause (List.hd args) [] (union fvs fvs') uvs uvdefs' newclauses' hcs' in
            let t = List.hd clauses' in
            let clauses'' = reverseDistributeImplication t goals currentclauses in
              DeOrifyClauseResult(clauses'', fvs'', uvdefs'', newclauses'', hcs'')
            
          else
            atomicHead ()
            
      | _ ->
          atomicHead ())
    
  (*  The term is a propositional head.  *)
  | _ -> DeOrifyClauseResult(term::currentclauses, fvs, uvdefs, newclauses, hcs)

(**********************************************************************
*deOrifyClauses:
* Removes disjunctions from a list of clauses.  The result is a list
* of absyn clauses that don't contain disjunctions.
**********************************************************************)
and deOrifyClauses clauses currentclauses uvs newclauses hcs =
  match clauses with
    [] ->
      DeOrifyClauseResult(currentclauses, [], [], newclauses, hcs)
  | c::cs ->
      let DeOrifyClauseResult(clauses', fvs', uvdefs', newclauses', hcs') = 
        deOrifyClauses cs currentclauses uvs newclauses hcs 
      in
        (deOrifyClause c clauses' fvs' uvs uvdefs' newclauses' hcs')

(**********************************************************************
*etaFluffQuantifier:
* Transforms the body of a quantified term into one that has an
* abstraction structure at the top; resulting body and bound variable
* are returned.
**********************************************************************)
and etaFluffQuantifier term arg : Absyn.aterm * Absyn.atypesymbol = 
  match arg with
    Absyn.AbstractionTerm(Absyn.NestedAbstraction(_,_),_) ->
     (Absyn.getTermAbstractionBody arg, Absyn.getTermAbstractionVar arg)
  (* We add this case for processing queries, which are toplevel terms,
   * and therefore may have UNestedAsbractions. However, we should not have
   * more than one abstraction in a quantifier term. *)
  | Absyn.AbstractionTerm(Absyn.UNestedAbstraction(_,nabs,_),_) ->
     if nabs = 1 then
       (Absyn.getTermAbstractionBody arg, List.hd @@ Absyn.getTermAbstractionVars arg)
     else
       Errormsg.impossible Errormsg.none "Clauses.etaFluffQuantifier: Expected predicate in quantifier."
  | _ ->
      let tenv = Absyn.getTermMoleculeEnv term in
      let sym = Symbol.generate () in
      let tsym = Absyn.BoundVar(sym, ref None, 
                                ref true, ref(Some(List.hd tenv))) in
      let bvterm = Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym), 
                                      Errormsg.none) in
      let term = Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(arg, [bvterm], 1), Errormsg.none) in
        (term, tsym)

(**********************************************************************
*normalizeClause:
* Normalizes the clause clauseterm and adds the result to the list
* clauses.  Accumulates the name of any universal variable that may be
* defined by this clause into the list uvdefs.
**********************************************************************)
and normalizeClause clauseterm clauses uvs uvdefs embedded =
  (********************************************************************
  *indeterminate:
  * Indicates that a predicate head's name didn't resolve.
  ********************************************************************)
  let indeterminate tsym pos =
    (Errormsg.error pos("Predicate name " ^
      (Absyn.getTypeSymbolName tsym) ^ " is indeterminate");
    clauses)
  in
  
  (********************************************************************
  *rigidAtom:
  * A "plain" atom head, with or without arguments.
  ********************************************************************)
  let rigidAtom term clauses args =
    match args with
      [] ->
        term :: clauses
    | _ ->
        let term' = Absyn.ApplicationTerm(
          Absyn.FirstOrderApplication(term, args, List.length args),
          Errormsg.none) in
        (term' :: clauses)
  in
  
  (*  Bail if the term is an error. *)
  if clauseterm = Absyn.errorTerm then
    clauseterm::clauses
  else
  
  let (t, args) = Absyn.getTermApplicationHeadAndArguments clauseterm in

  match t with
      Absyn.ConstantTerm(c, env, pos) ->
        (*  Clause is: all P  *)
        if c = Pervasive.allConstant then
          let (arg, tsym) = etaFluffQuantifier t (List.hd args) in
          (distributeUniversal t tsym (normalizeClause arg [] uvs uvdefs embedded)) @ clauses
                                            
          (*  Clause is: a, b or a & b  *)
        else if c = Pervasive.andConstant || c = Pervasive.ampandConstant then
          let arg1 = List.hd args in
          let arg2 = List.hd (List.tl args) in
          (normalizeClause arg2 (normalizeClause arg1 clauses uvs uvdefs embedded) uvs uvdefs embedded)
          
          (*  Clause is an implication. *)
        else if c = Pervasive.implConstant then
          let arg1 = List.hd args in
          let arg2 = List.hd (List.tl args) in
          (distributeImplication arg1 (normalizeClause arg2 [] uvs uvdefs embedded) clauses)
          
          (*  Clause is: a :- b.  *)
        else if c = Pervasive.colondashConstant then
          let arg1 = List.hd args in
          let arg2 = List.hd (List.tl args) in
          (distributeImplication arg2 (normalizeClause arg1 [] uvs uvdefs embedded) clauses)
          
          (*  Clause is an exists clause. *)
        else if c = Pervasive.someConstant then
          (Errormsg.error (Absyn.getTermPos t) "Existential quantifer illegal at top-level in clause";
           clauses)
      
          (*  Clause is: a; b *)
        else if c = Pervasive.orConstant then
          (Errormsg.error (Absyn.getTermPos t) "Disjunction illegal at top-level in clause";
           clauses)
        
        else if (Absyn.getConstantType c) = (Absyn.PervasiveConstant(false)) then
          (Errormsg.error (Absyn.getTermPos t) ("attempting to redefine predicate" ^
                          (Errormsg.info (Absyn.getConstantPrintName c)));
           clauses)

        else if (Absyn.getConstantNoDefs c) then
          (Errormsg.error (Absyn.getTermPos t) ("clauses for constant " ^
                          (Absyn.getConstantPrintName c) ^ " prohibited in this module");
           clauses)
          
        else 
          if embedded then
            ((Absyn.getConstantClosedRef c) := false;
             rigidAtom t clauses args)
          else
            (rigidAtom t clauses args)
        
  | Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym), pos) ->
      if List.memq tsym uvs then
        let t' = Absyn.ConstantTerm(Absyn.getTypeSymbolHiddenConstant tsym,
                  [], Errormsg.none) in
        if not (List.memq tsym !uvdefs) then
          (uvdefs := tsym :: !uvdefs;
           rigidAtom t' clauses args)
        else
          (rigidAtom t' clauses args)
      else
        (indeterminate tsym pos)
  | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tsym), pos) ->
      (indeterminate tsym pos)
  | _ ->
    (Errormsg.impossible (Absyn.getTermPos t) 
       ("Clauses.normalizeClause: Invalid term type: " ^
      (Absyn.string_of_term_ast t) ^ "; " ^ 
        (Absyn.string_of_term_ast clauseterm)))

(**********************************************************************
*translateClauses:
* This is the main exported function for this module.  It takes both
* a preabsyn and absyn module, and returns an updated absyn module.
**********************************************************************)
and translateClauses pmod amod =
  let getNewClauses ncs =
    let ncs' = List.map getNewClauseClause ncs in
    ncs'
  in
  
  let setHiddenConstants amod hcs =
    let hiddenconstants = (List.map getHiddenConstantConstant hcs) in
    let skels = (List.map (Absyn.getConstantSkeletonValue) hiddenconstants) in
    let _ = (Absyn.getModuleHiddenConstantsRef amod) := hiddenconstants in
    let _ = (Absyn.getModuleHiddenConstantSkeletonsRef amod) := skels in
    amod
  in
  
  (********************************************************************
  *translateClause:
  * Translates a given preabsyn clause.  It first parses it using the
  * Parse module, which generates an absyn term.  Then, it normalizes
  * the term, and finally deorifies it.  The result is a list of
  * absyn clauses.
  ********************************************************************)
  let translateClause preclause amod clauses newclauses hcs =
    let preterm = Preabsyn.getClauseTerm preclause in
    let clause = Parse.translateClause preterm amod in
    if Option.isSome clause then
      let clause = Option.get clause in
      let uvdefs = ref [] in
      let clauses' = normalizeClause clause [] [] uvdefs false in
      let DeOrifyClauseResult(clauses', _, _, newclauses', hcs') = 
        (deOrifyClauses clauses' clauses [] newclauses hcs) in
      (clauses', newclauses', hcs')
    else
      (clauses, newclauses, hcs)
  in
  
  (********************************************************************
  *parse':
  * Auxiliary function that iterates over a list of preabsyn clauses,
  * translates them, and flattens the resulting lists of clauses.
  ********************************************************************)
  let rec parse' preclauses amod clauses newclauses hcs =
    match preclauses with
      [] -> (clauses, newclauses, hcs)
    | c::cs ->
        let (clauses', newclauses', hcs') = 
          translateClause c amod clauses newclauses hcs 
        in
          parse' cs amod clauses' newclauses' hcs'

  in

  initClosedDefs (); 
  let preclauses = Preabsyn.getModuleClauses pmod in
  let (clauses', newclauses', hcs') = parse' preclauses amod [] [] [] in
  
  (*  Enter the hidden constants into the module  *)
  let amod' = setHiddenConstants amod hcs' in
  let newclauses'' = getNewClauses newclauses' in  
    (amod', List.map Parse.removeNestedAbstractions clauses', 
     List.map Parse.removeNestedAbstractions (List.concat newclauses''),
     getClosedDefs ())

(**********************************************************************
*linearizeClause:
* Linearizes a term representing a clause.  The resulting are probably
* broken.
**********************************************************************)
let linearizeClause clause =
  let counter = ref 0 in
  let fresh tsym =
    let _ = incr counter in
    let name = "Linearized-" ^ (string_of_int !counter) in
    let sym = Symbol.symbol name in
    let tsym' = Absyn.copyTypeSymbol sym tsym in
    Absyn.NamedFreeVar(tsym')
  in

  let imp head body pos =
    Absyn.ApplicationTerm(
      Absyn.FirstOrderApplication(
        Absyn.ConstantTerm(Pervasive.implConstant, [],  pos),
        [body; head],
        2),
      pos)
  in

  let conj pos l r =
    Absyn.ApplicationTerm(
      Absyn.FirstOrderApplication(
        Absyn.ConstantTerm(Pervasive.andConstant, [], pos),
        [l; r],
        2),
      pos)
  in
  
  let getHeadAndBody clause : Absyn.aterm * (Absyn.aterm option) =
    match clause with
        Absyn.ApplicationTerm(
          Absyn.FirstOrderApplication(
            Absyn.ConstantTerm(impc, _, _),
            [body; head],
            _),
          _) when impc == Pervasive.implConstant ->
          (head, Some body)
      | Absyn.ApplicationTerm(
          Absyn.FirstOrderApplication(
            Absyn.ConstantTerm(impc, _, _),
            [head; body],
            _),
          _) when impc == Pervasive.colondashConstant ->
          (head, Some body)
      | Absyn.ApplicationTerm(_) -> (clause, None)
      | Absyn.ConstantTerm(_) -> (clause, None)
      | _ -> Errormsg.impossible (Absyn.getTermPos clause) ("Clauses.linearizeClause: invalid clause " ^ (Absyn.string_of_term_ast clause))
  in
  
  let linearizeHead head pos =
    let rec fold f s ts =
      match ts with
        t::ts' ->
          let (h, b) = f t s in
          let (hs, b') = fold f b ts' in
          (h::hs, b')
      | [] -> ([], s)
    in
    
    let rec linearize term uvs =
      match term with
        | Absyn.IntTerm(_) | Absyn.RealTerm(_)
        | Absyn.StringTerm(_) | Absyn.ConstantTerm(_)
        | Absyn.BoundVarTerm(_) | Absyn.ErrorTerm -> (term, uvs)
        
        | Absyn.ApplicationTerm(Absyn.FirstOrderApplication(c, args, i), p) ->
            let (args', uvs') = fold linearize uvs args in
            (Absyn.ApplicationTerm(Absyn.FirstOrderApplication(c, args', i), p), uvs')
        | Absyn.AbstractionTerm(Absyn.UNestedAbstraction(tsyms, i, body), p) ->
            let (body', uvs') = linearize body uvs in
            (Absyn.AbstractionTerm(Absyn.UNestedAbstraction(tsyms, i, body'), p), uvs')
        
        | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tsym) as info, p) ->
            let sym = Absyn.getTypeSymbolSymbol tsym in
            (try
              let (_, others) = List.assoc sym uvs in
              let _ = Errormsg.log p ("Clauses.linearseClause: found " ^ (Absyn.string_of_term_ast term)) in
              let fv = fresh tsym in
              let uvs' = List.remove_assq sym uvs in
              (Absyn.FreeVarTerm(fv, p), (sym, (info, fv :: others)) :: uvs')
            with Not_found ->
                let _ = Errormsg.log p ("Clauses.linearseClause: not found " ^ (Absyn.string_of_term_ast term)) in
               (term, (sym, (info, [])) :: uvs))
        | _ -> Errormsg.impossible pos ("Clauses.linearize: invalid term " ^ (Absyn.string_of_term_ast term))
    in
    match head with
        Absyn.ApplicationTerm(Absyn.FirstOrderApplication(c, args, i), _) ->
          let (args', uvs) = fold linearize [] args in
          (Absyn.ApplicationTerm(Absyn.FirstOrderApplication(c, args', i), pos), uvs)
      | Absyn.ConstantTerm(_) -> (head, [])
      | _ -> Errormsg.impossible pos ("Clauses.linearize: invalid head " ^ (Absyn.string_of_term_ast head))
  in
  
  let equalities pos uvs body =
    let equality tinfo tinfo' =
      let types = [] in
      let fv = Absyn.FreeVarTerm(tinfo, pos) in
      let fv' = Absyn.FreeVarTerm(tinfo', pos) in
      Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(
          Absyn.ConstantTerm(Pervasive.eqConstant, types, pos),
          [fv;fv'],
          2),
        pos)
    in
    let uvs' = List.rev uvs in
    let eqs = List.flatten 
                (List.map (fun (_, (info, fvs)) -> 
                             List.map (equality info) (List.rev fvs)) uvs') in
    match eqs with
      eq::eqs' ->
        let l = List.fold_right (conj pos) eqs' eq in
        (match body with
            Some body' -> Some (conj pos l body')
          | None -> Some l)
    | [] -> body
  in
  
  let (head, body) = getHeadAndBody clause in
  let pos = Absyn.getTermPos head in
  let _ = Errormsg.log pos 
            ("Clauses.linearizeClause: head " ^ 
             (Absyn.string_of_term_ast head)) in
  let _ = if Option.isSome body then
    Errormsg.log pos 
      ("Clauses.linearizeClause: body " ^ 
       (Absyn.string_of_term_ast (Option.get body)))
  in
  let (head', uvs) = linearizeHead head pos in
    match equalities pos uvs body with
        Some body' ->
          imp head' body' pos
      | None -> head'

(* Construct a new clause for a query
 * main X0 ... Xn :- {query X0 ... Xn}. *)
let makeQueryClause query : Absyn.aconstant * Absyn.aterm * (Absyn.atypesymbol list) * (Absyn.atype list) =
  let query, fvars = collectFreeVars query ~toplevel:true [] [] in
  let tyfvars : Absyn.atype list = List.map Absyn.getTypeSymbolType fvars in
  let (pred, head) = newHead (List.rev fvars) in
  let appinfo = Absyn.FirstOrderApplication(
                    Absyn.makeConstantTerm (Pervasive.implConstant) [] 
                      Errormsg.none, [query;head], 2) in
  (pred, Absyn.ApplicationTerm(appinfo, Errormsg.none), fvars, tyfvars)


let translateQuery query amod : Absyn.amodule * (Absyn.aterm list) * (Absyn.aterm list) * (closeddefinition list) =
  let setHiddenConstants amod hcs =
    let hiddenconstants = (List.map getHiddenConstantConstant hcs) in
    let skels = (List.map (Absyn.getConstantSkeletonValue) hiddenconstants) in
    let _ = (Absyn.getModuleHiddenConstantsRef amod) := hiddenconstants in
    let _ = (Absyn.getModuleHiddenConstantSkeletonsRef amod) := skels in
    amod
  in
  let translateClause query amod =
    let uvdefs = ref []  in
    let clauses = normalizeClause query [] [] uvdefs false in
    let DeOrifyClauseResult(clauses', _, _, newclauses, hcs) = 
      (deOrifyClauses clauses [] [] [] []) in
    (clauses', newclauses, hcs)   
  in
  initClosedDefs ();
  (* The main clause is at the head of clauses *)
  let (clauses, newclauses, hcs) = translateClause query amod in
  let main_clause = List.hd clauses in
  let clauses = List.tl clauses in
  (* Enter the hidden constants into the module *)
  let amod' = setHiddenConstants amod hcs in
  (* main_clause needs to be processed as a new clause
   * because it is anonymous (and therefore closed) *)
  let newclauses' = main_clause :: (List.concat (List.map getNewClauseClause newclauses)) in
  (amod',
   List.map Parse.removeNestedAbstractions clauses,
   List.map Parse.removeNestedAbstractions newclauses',
   getClosedDefs ())
