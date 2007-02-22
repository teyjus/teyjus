type symbol = Symbol.symbol
type pos = Errormsg.pos

type universalvardefinition = (Absyn.atypesymbol * Absyn.aterm list)
type newclause = NewClause of (Absyn.aconstant * Absyn.aterm list)

type deorifygoalresult =
  DeOrifyGoalResult of (Absyn.aterm list * Absyn.atypesymbol list *
  universalvardefinition list * bool * newclause list)

let getDeOrifyGoalResultGoal = function
  DeOrifyGoalResult(g,_,_,_) -> g
let getDeOrifyGoalResultGoal = function
  DeOrifyGoalResult(_,fvs,_,_) -> fvs
let getDeOrifyGoalResultGoal = function
  DeOrifyGoalResult(_,_,uvdefs,_) -> uvdefs
let getDeOrifyGoalResultGoal = function
  DeOrifyGoalResult(_,_,_,hc,_) -> hc
let getDeOrifyGoalResultGoal = function
  DeOrifyGoalResult(_,_,_,_,nc) -> nc


(********************************************************************
*newPredicate:
*
********************************************************************)
let newPredicate goals fvs newclauses =
  let (pred, head) = newHead fvs in
  let clauses = reverseDistributeImplication head goals [] in
  let newclause = NewClause(pred, clauses) in
  newclause::newclauses

(********************************************************************
*newPredicateFromAnd:
********************************************************************)
let newPredicateFromAnd goals1 goals2 fvs newclauses =
  let (pred, head) = newHead fvs in
  let clauses = reverseDistributeImplication head goals2 [] in
  let clauses' = reverseDistributeImplication head goals1 clauses in
  let newclause = NewClause(pred, clauses') in
  newclause::newclauses

(********************************************************************
*getClauseHeadAndArguments:
* Gets the clause head and arguments, flattening an applications at
* the head.
********************************************************************)
let rec getClauseHeadAndArguments term =  
  match t with
    Absyn.ApplicationTerm(Absyn.FirstOrderTerm(t', args, _), _) ->
      let (head, args') = getClauseHeadAndArguments t' in
      (t', args' @ args)
  | _ -> (term, [])

(********************************************************************
*collectFreeVars:
* Accumulates the free variables in a term onto the given fvs list.
* Transforms applications to Boehm form, and checks the legality of
* constants in the term.
********************************************************************)
and collectFreeVars term fvs bvs =
  match term with
    Absyn.IntTerm(_) -> (term, fvs)
  | Absyn.RealTerm(_) -> (term, fvs)
  | Absyn.StringTerm(_) -> (term, fvs)
  | Absyn.FreeVarTerm(tsym,_,pos) ->
      if not List.mem tsym fvs then
        (term, tsym::fvs)
      else
        (term, fvs)
  | Absyn.BoundVarTerm(tsym, _, pos) ->
      if not ((List.mem tsym bvs) || (List.mem tsym fvs)) then
        (term, tsym::fvs)
      else
        (term, fvs)
  | Absyn.ConstantTerm(c,_,_,pos) ->
      if Parse.illegalConstant c pos then
        (Absyn.errorTerm, [])
      else
        (term, fvs)
  | Absyn.AbstractionTerm(tsym,body,pos) ->
      let (body', fvs') = collectFreeVars body fvs bvs in
      let term' = Absyn.AbstractionTerm(tsym, body', pos) in
      (term', fvs')
  | Absyn.ApplicationTerm(_) ->
      let (head, args) = getClauseHeadAndArguments term in
      let (head', fvs') = collectFreeVars head fvs bvs in
      let (args', fvs'') = collectArgsFreeVars args fvs' in
      let term' = Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(
          head',
          args',
          List.size args'),
        false,
        pos) in
      (term', fvs'')
  | _ -> Errormsg.impossible Errormsg.none "Clauses.collectFreeVars: Invalid term type."

(**********************************************************************
*collectArgsFreeVars:
* Collect the free variables of a list of terms, and return a list
* of rectified items, and the fvs.
**********************************************************************)
and collectArgsFreeVars args fvs =
  match args with
    [] -> ([], fvs)
  | a::aa ->
      let (args', fvs') = collectArgsFreeVars aa fvs in
      let (arg', fvs'') = collectFreeVars a fvs bvs in
      (arg'::args', fvs'')

(**********************************************************************
*distributeUniversal:
* Insert a universal quantification over vtysy over each of the terms
* in myterms.
**********************************************************************)
let rec distributeUniversal term tsym terms =
  let rec distribute' term terms =
    match terms with
      [] -> []
    | t::ts ->
        let newterm = Absyn.ApplicationTerm(
          Absyn.FirstOrderApplication(term,
            [Absyn.AbstractionTerm(tsym, t, Errormsg.none)], 1), 
          Errormsg.none) in
        (newterm :: (distribute' term ts))
  in
  
  distribute' term terms

(**********************************************************************
*distributeImplication:
* Produces a new list of normalized clauses by adding an implication over
* each of the normalized clauses in cls with the antecedent of each
* implication being goal.
**********************************************************************)
and distributeImplication goal cls =
  (********************************************************************
  *distribute:
  * Distribute a single implication.
  ********************************************************************)
  let rec distribute goal clause ctable =
    let regularImplication () =
      let imply = Parse.implicationTerm in
      Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(imply, [goal; clause], 2),false, Errormsg.none)
    in
    
    match clause with
      Absyn.ApplicationTerm(Absyn.FirstOrderApplication(f, args, _), _, pos) ->
        (match f with
          Absyn.ConstantTerm(c,_) ->
            if c = Pervasive.allConstant then
              let arg1 = List.hd args in
              let tsym = Absyn.getAbstractionTermVar arg1 in
              let body = Absyn.getAbstractionTermBody arg1 in
              Absyn.ApplicationTerm(
                Absyn.FirstOrderApplication(
                  f,
                  [Absyn.AbstractionTerm(tsym, distribute goal arg1, Errormsg.none)],
                  1),
                false,
                Errormsg.none)
            else if c = Pervasive.implicationConstant then
              let arg1 = List.hd args in
              let arg2 = List.hd (List.tl args) in
              let inner = Absyn.ApplicationTerm(
                Absyn.FirstOrderApplication(
                  Parse.andTerm,
                  [arg1; goal],
                  2),
                false,
                Errormsg.none) in
              Absyn.ApplicationTerm(
                Absyn.FirstOrderApplication(
                  f, 
                  [inner; arg2],
                  2),
                false,
                Errormsg.none)
            else
              regularImplication ()
        | _ -> regularImplication ())
    | _ -> regularImplicaiton ()
  in
  
  match cls with
    [] -> []
  | c::cs ->
      (distribute goal c)  :: (distributeImplication goal cs)


(**********************************************************************
*deOrifyGoal:
* The returned value brings back a list of disjunctions forming goal if
* wholegoal is true and a disjunction free form otherwise.
**********************************************************************)
let rec deOrifyGoal goal uvs uvdefs andgoal wholegoal =
  let (t, args) = getClauseHeadAndArguments goal in
  
  match t with
    Absyn.ConstantTerm(c,_,pos) ->
      if c = Pervasive.allConstant then
        let arg1 = List.hd args in
        (deOrifyUniversalGoal t arg1 uvs uvdefs andgoal wholegoal)
      else if c = Pervasive.someConstant then
        let arg1 = List.hd args in
        (deOrifyExistentialGoal t arg1 uvs uvdefs andgoal wholegoal)
      else if c = Pervasive.orConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (deOrifyOrGoal arg1 arg2 uvs uvdefs andgoal wholegoal)
      else if c = Pervasive.andConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (deOrifyAndGoal arg1 arg2 uvs uvdefs andgoal wholegoal)
      else if c = Pervasive.implicationConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (deOrifyImplicationGoal arg1 arg2 uvs uvdefs andgoal false)
      else if c = Pervasive.colonDashConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (deOrifyImplicationGoal arg2 arg1 uvs uvdefs andgoal false)
      else if c = Pervasive.cutConstant then
        deOrifyCutGoal goal andgoal uvdefs
      else
        deOrifyAtomicGoal t args andgoal uvs uvdefs
  | _ -> (deOrifyAtomicGoal t args andgoal uvs uvdefs)

(**********************************************************************
*deOrifyAtomicGoal:
* For atomic goals other than cut. If the head is a uv, it must be replaced
* with a constant term made from the appropriate hidden constant. Otherwise,
* if it is a bound or free var, it counts as a free variable occurrence.
* Finally, arguments have to be checked for correctness, they have to be
* rectified and their free variables collected.
**********************************************************************)
and deOrifyAtomicGoal head args andgoal uvs uvdefs =
  let deorify' head hascut fvs uvdefs =
  
    (*  If the andgoal is not None, the head should actually be an
        application, with and ("," or "&") at the head, and the
        original head and the andgoal as arguments. *)
    let getGoal head andgoal =
      if Option.isSome andgoal then
          let andgoal' = Option.get andgoal in
          let head' = Absyn.ApplicationTerm(
            Absyn.FirstOrderApplication(
              Parse.andTerm,
              [head; andgoal'],
              2),
            false,
            Absyn.getTermPos head) in
          DeOrifyGoalResult([head'], fvs, uvdefs, hascut)
        else
          DeOrifyGoalResult([head], fvs, uvdefs, hascut)
    in
    
    match args with
      [] ->
          DeOrifyGoalResult([getGoal head], fvs, uvdefs, hascut)
    | _ ->
        let (args', fvs') = collectArgsFreeVars args fvs in
        let head' = Absyn.ApplicationTerm(
          Absyn.FirstOrderApplication(
            head,
            args',
            List.size args'),
          false,
          Absyn.getTermPos head) in
        DeOrifyGoalResult([getGoal head'], fvs', uvdefs, hascut)
  in
  
  match head with
    Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym),_,pos) ->
      if (List.mem tsym uvs) then
        let c = Absyn.getTypeSymbolConstant tsym in
        let head' = Absyn.ConstantTerm(c, [], false, pos) in
        deorify' head' false [] uvdefs
      else
        let head' = Absyn.FreeVarTerm(tsym, pos) in
        deorify' head' false [tsym] uvdefs
  | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tsym),_,pos) ->
      deorify' head false [tsym] uvdefs
  | _ -> deorify' head false [] uvdefs

(**********************************************************************
*deOrifyOneOrGoal:
* Processing one disjunct of a disjunctive goal. If this branch has a cut,
* then a new predicate must be introduced to prevent the cut from cutting off
* choice points in the parent clause.
**********************************************************************)
let rec deOrifyOneOrGoal goal uvs uvdefs andgoal =
  let DeOrifyGoalResult(goals', uvs', uvdefs', hascut') =
    (deOrifyGoal goal uvs uvdefs andgoal true) in
  
  if hascut' then
    let _ = _ in
    DeOrifyGoalResult(,uvs',uvdefs',hascut')
  else
    DeOrifyGoalResult(goals',uvs',uvdefs',hascut')

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
and deOrifyOrGoal goal1 goal2 uvs uvdefs andgoal wholegoal =
  let getAndGoal wholegoal andgoal =
    if wholegoal then
      andgoal
    else
      None
  in
  let andgoal' = (getAndGoal wholegoal andgoal) in
  
  let result1 = deOrifyOneOrGoal goal2 uvs uvdefs andgoal' in
  
  let uvdefs' = getDeOrifyGoalResultUVDefs result1 in
  let result2 = deOrifyOneOrGoal goal1 uvs uvdefs' andgoal' in
  
  let fvs' = union (getDeOrifyGoalResultFVS result1) (getDeOrifyGoalResultFVS result2) in
  let uvdefs' = getDeOrifyGoalResultsUVDefs result1 in

  if wholegoal then
    let goals1 = getDeOrifyGoalResultGoals result1 in
    let goals2 = getDeOrifyGoalResultGoals result2 in
    DeOrifyGoalResults(goals1 @ goals2, fvs', uvdefs', false)
  else
    let pred = newPredicate
      (getDeOrifyGoalResultGoals result2)
      (getDeOrifyGoalResultGoals result1)
      (getDeOrifyGoalResultFVS result1) in
    if Option.isSome andgoal' then
      let goal' = Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(
          Parse.andTerm,
          [pred; Option.get andgoal'],
          2),
        false,
        Errormsg.none) in
      DeOrifyGoalResults([goal'], fvs', uvdefs', false)
    else
      DeOrifyGoalResults([pred], fvs', uvdefs', false)

(**********************************************************************
*deOrifyAndGoal:
**********************************************************************)
and deOrifyAndGoal goal1 goal2 uvs uvdefs andgoal wholegoal =
  let getAndGoal goals =
    match goals with
      [] -> None
    | g::gs -> Some g
  in
  
  let result1 = deOrifyGoal goal2 uvs uvdefs andgoal false in
  let r1goals = getDeOrifyGoalResultGoals result1 in
  let andgoal' = getAndGoal r1goals in
  
  let uvdefs' = getDeOrifyGoalResultsUVDefs result1 in
  
  let result2 = deOrifyGoal goal1 uvs uvdefs' andgoal' wholegoal in
  let fvs' = union (getDeOrifyGoalResultsFVS result1) (getDeOrifyGoalResultsFVS result2) in
  let hascut' = (getDeOrifyGoalResultsHasCut result1) || (getDeOrifyGoalResultsHasCut result2) in
  let uvdefs' = getDeOrifyGoalResultsUVDefs result2 in
  DeOrifyGoalResults([getDeOrifyGoalResultsGoals result2], fvs', uvdefs', hascut')

(**********************************************************************
*deOrifyClause:
* Removes disjunctions from a clause.
**********************************************************************)
let rec deOrifyClause term currentclauses fvs uvs uvdefs =
  match term with
    (*  An application  *)
    Absyn.ApplicationTerm(Absyn.FirstOrderApplication(f, arg::args, num), _, _) ->
      (*  atomicHead: regular head with arguments.  *)
      let atomicHead () =  
        let (args', fvs') = checkClauseArguments args fvs in
        let newclause = Absyn.ApplicationTerm(Absyn.FirstOrderApplication(f, args', num)) in
        (newclause::currentclauses, fvs', uvdefs)
      in
      
      (match f with
        Absyn.ConstantTerm(c,_,_,_) ->
          if c = Pervasive.allConstant then
            let tsym = Absyn.getAbstractionTermVar(arg) in
            let (clauses', fvs', uvdefs') = deOrifyClause arg [] fvs uvs uvdefs in
            let clauses'' = distributeUniversal f tsym clauses' in
            let fvs'' = List.filter ((=) tsym) fvs' in
            (clauses'' @ currentclauses, fvs'', uvdefs)
            
          else if c = Pervasive.implicationConstant then
            let (goals, fvs', uvdefs') = deOrifyGoal arg uvs uvdefs in
            let (clauses', fvs'', uvdefs'') = deOrifyClause (List.hd args) [] (union fvs' fvs'') uvs uvdefs' in
            let t = List.head clauses' in
            let clauses'' = reverseDistributeImplication t goals currentclauses in
            (clauses'', fvs'', uvdefs'')
            
          else
            atomicHead ()
            
      | _ ->
          atomicHead ())
    
    (*  The term is a propositional head.  *)
  | _ -> (term::currentclauses, fvs, uvdefs)

(**********************************************************************
*deOrifyClauses:
* Removes disjunctions from a list of clauses.  The result is a list
* of absyn clauses that don't contain disjunctions.
**********************************************************************)
and deOrifyClauses clauses currentclauses uvs =
  match clauses with
    [] ->
      currentclauses
  | c::cs ->
      let (clauses', fvs', uvs', uvdefs') = deOrifyClauses cs currentclauses uvs in
      (deOrifyClause c clauses' fvs' uvs' uvdefs')

(**********************************************************************
*etaFluffQuantifier:
* Transforms the body of a quantified term into one that has an
* abstraction structure at the top; resulting body and bound variable
* are returned.
**********************************************************************)
and etaFluffQuantifier term arg =
  match term with
    Absyn.LambdaTerm(_) ->
      (Absyn.getAbstractionTermBody arg, Absyn.getAbstractionTermVar arg)
  | _ ->
      let c = Absyn.getConstantTermConstant term in
      let tenv = Absyn.getConstantTypeEnv c in
      let tsym = Absyn.BoundVar(Symbol.generate (), ref None, ref true, List.hd tenv) in
      let bvterm = Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym), false, Errormsg.none) in
      let term = Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(arg, [bvterm], 1), false, Errormsg.none) in
      (term, tsym)
        

(**********************************************************************
*normalizeClause:
*
**********************************************************************)
and normalizeClause clauseterm clauses uvs uvdefs embedded =
  (********************************************************************
  *indeterminate:
  * Indicates that a predicate head's name didn't resolve.
  ********************************************************************)
  let indeterminate term clauses =
    (Errormsg.error (Absyn.getTermPos t) ("Predicate name " ^
      (Absyn.getTypeSymbolName tsym) ^ " is indeterminate");
    clauses)
  in
  
  (********************************************************************
  *rigidAtom:
  * An "plain" atom head, with or without arguments.
  ********************************************************************)
  let rigidAtom term clauses args =
    match args with
      [] ->
        term :: clauses
    | _ ->
        let term' = Absyn.ApplicationTerm(
          Absyn.FirstOrderApplication(term, args, List.size args),
          false, Errormsg.none) in
        (term' :: clauses)
  in
  
  (*  Bail if the term is an error. *)
  if clauseterm = Absyn.errorTerm then
    clauseterm::clauses
  else
  
  let (t, args) = getClauseHeadAndArguments clauseterm in
  
  match t with
    Absyn.ConstantTerm(c, _, _) ->
      (*  Clause is: all P  *)
      if c = Pervasive.allConstant then
        let (arg, tsym) = etaFluffQuantifier t (List.hd args) in
        (distributeUniversal t tsym (normalizeClause arg [] uvs uvdefs embedded) uvs embedded)
      
      (*  Clause is: a, b or a & b  *)
      else if c = Pervasive.andConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (normalizeClause arg2 (normalizeClause arg1 clauses uvs uvdefs embedded) uvs uvdefs embedded)
      
      (*  Clause is an implication. *)
      else if c = Pervasive.implicationConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (distributeImplication arg1 (normalizeClause arg2 [] uvs uvdefs embedded) clauses)
      
      (*  Clause is: a :- b.  *)
      else if c = Pervasive.colonDashConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (distributeImplication arg2 (normalizeClause arg1 [] uvs embedded) clauses)

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
          (Errormsg.info (Absyn.getConstantName c)));
        clauses)
      
      else if (Absyn.getConstantNoDefs c) then
        (Errormsg.error (Absyn.getTermPos t) ("clauses for constant " ^
          (Absyn.getConstantName c) ^ " prohibited in this module");
        clauses)
      
      else if embedded then
        ((Absyn.getConstantClosedRef c) := false;
        rigidAtom t clauses args)
  | Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym), _, _) ->
      if List.mem tsym uvs then
        let t' = Absyn.ConstantTerm(Absyn.getTypeSymbolConstant tsym,
          [], false, Errormsg.none) in
        if not List.mem tsym !uvdefs then
          (uvdefs := tsym :: !uvdefs;
          rigidAtom t clauses args)
        else
          (rigidAtom t clauses args)
      else
        (indeterminate t clauses)
  | _ ->
      (indeterminate t clauses)
      
(**********************************************************************
*newKind:
* Callback used when the term parser encounters an undefined kind.
* Any undefined kind is flagged as an error.
**********************************************************************)
and newKind sym arity pos =
  let k = Absyn.Kind() in
  let ktable' = () in
  (if arity > 0 then
    Errormsg.error pos ("undefined type constructor " ^
      (Symbol.name sym) ^ " of arity " ^ (string_of_int arity))
  else
    Errormsg.error pos ("undefined sort " ^ (Symbol.name sym)));
  k

(**********************************************************************
*newConstant:
* Callback used whn the term parser encounters an undefined constant.
*************=********************************************************)
and newConstant =
  ()

(**********************************************************************
*translateClauses:
* This is the main exported function for this module.  It takes both
* a preabsyn and absyn module, and returns an updated absyn module.
**********************************************************************)
and translateClauses pmod amod =
  (********************************************************************
  *translateClause:
  * Translates a given preabsyn clause.  It first parses it using the
  * Parse module, which generates an absyn term.  Then, then normalizes
  * the term, and finally deorifies it.  The result is a list of
  * absyn clauses.
  ********************************************************************)
  let translateClause preclause ktable ctable =
    let (clause, newconstants) =
      Parse.translateClause preclause ktable ctable newkind newconst in
    
    let normclause = normalizeClause clause in
    let cls = deOrifyClauses normclause in
    cls
  in
  
  (********************************************************************
  *parse':
  * Auxiliary function that iterates over a list of preabsyn clauses,
  * translates them, and flattens the resulting lists of clauses.
  ********************************************************************)
  let parse' preclauses ktable ctable =
    match preclauses with
      [] -> []
    | c::cs ->
        let clauses = translateClause c ktable ctable in
        clauses @ (parse' cs ktable ctable)

  in
  let ktable = Absyn.getModuleKindTable amod in
  let ctable = Absyn.getModuleConstantTable amod in
  
  let preclauses = Preabsyn.getModuleClauses pmod in
  let clauses = parse' preclauses [] ktable ctable in
  
  (*  Enter them into the module  *)
  (Absyn.getModuleClausesRef amod) := clauses
