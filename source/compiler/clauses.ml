type symbol = Symbol.symbol
type pos = Errormsg.pos

type universalvardefinition = (Absyn.atypesymbol * Absyn.aterm list)
type newclause = NewClause of (Absyn.aconstant * Absyn.aterm list)

let getNewClauseConstant = function
  NewClause(c,_) -> c

let getNewClauseClause = function
  NewClause(_,c) -> c

type hiddenconstant =
  HiddenConstant of (Absyn.aconstant * Absyn.atypesymbol)

type deorifygoalresult =
  DeOrifyGoalResult of (Absyn.aterm list * Absyn.atypesymbol list *
    universalvardefinition list * bool * newclause list * hiddenconstant list)

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
let getDeOrifyGoalResultHiddenConsts = function
  DeOrifyGoalResult(_,_,_,_,_,hc) -> hc

type deorifyclauseresult =
  DeOrifyClauseResult of (Absyn.aterm list * Absyn.atypesymbol list *
    universalvardefinition list * newclause list)

(********************************************************************
*union:
* Returns the union of two lists of free variables.
********************************************************************)
let rec union fvs1 fvs2 =
  fvs1 @ fvs2

(********************************************************************
*makeHiddenConstant:
********************************************************************)
let makeHiddenConstant tsym =
  let ty = Absyn.getTypeSymbolType tsym in
  let skel = Absyn.makeSkeleton ty in
  let c = Absyn.makeHiddenConstant skel in
  ((Absyn.getTypeSymbolHiddenConstantRef tsym) := Some c;
  HiddenConstant(c,tsym))
  
(********************************************************************
*newHead:
* Creats a predicate name and a clause head for a list of clauses
* corresponding to a disjunctive goal. Skeletonization here is okay
* since the type skeleton will not be needed again for such predicates.
********************************************************************)
let rec newHead fvs =
  let rec argumentsAndTypes fvs =
    match fvs with
      [] -> ([], [])
    | f::fs ->
        let arg = Absyn.makeBoundVarTerm f Errormsg.none in
        let ty = Absyn.getTypeSymbolType f in
        let (args, tys) = argumentsAndTypes fs in
        (arg::args, ty::tys)
  in
    
  if List.length fvs > 0 then
    let c = Absyn.makeAnonymousConstant 0 in
    let t = Absyn.ConstantTerm(c, [], false, Errormsg.none) in
    (c, t)
  else
    let (args, argtys) = argumentsAndTypes fvs in
    let ty = Absyn.makeArrowType (Absyn.makeKindType Pervasive.kbool) argtys in
    let tymol = Types.skeletonizeType(ty) in
    let env = (Types.getMoleculeEnvironment tymol) in
    let c = Absyn.makeAnonymousConstant (List.length env) in
    let t = Absyn.ConstantTerm(c, env, false, Errormsg.none) in
    let t' = Absyn.ApplicationTerm(Absyn.FirstOrderApplication(t, args, List.length args), false, Errormsg.none) in
    (c, t')

(********************************************************************
*newPredicate:
*
********************************************************************)
and newPredicate goals fvs newclauses =
  let (pred, head) = newHead fvs in
  let clauses = reverseDistributeImplication head goals [] in
  let newclause = NewClause(pred, clauses) in
  (head, newclause::newclauses)

(********************************************************************
*newPredicateFromAnd:
********************************************************************)
and newPredicateFromAnd goals1 goals2 fvs newclauses =
  let (pred, head) = newHead fvs in
  let clauses = reverseDistributeImplication head goals2 [] in
  let clauses' = reverseDistributeImplication head goals1 clauses in
  let newclause = NewClause(pred, clauses') in
  (head, newclause::newclauses)

(********************************************************************
*getClauseHeadAndArguments:
* Gets the clause head and arguments, flattening an applications at
* the head.
********************************************************************)
and getClauseHeadAndArguments term =  
  match term with
    Absyn.ApplicationTerm(Absyn.FirstOrderApplication(t', args, _), _, pos) ->
      let (head, args') = getClauseHeadAndArguments t' in
      (t', args' @ args)
  | _ -> (term, [])

(********************************************************************
*closeUniversalDefinitions:
********************************************************************)
and closeUniversalDefinitions tsym uvdefs =
  ()

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
  | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tsym),_,pos) ->
      if not (List.mem tsym fvs) then
        (term, tsym::fvs)
      else
        (term, fvs)
  | Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym), _, pos) ->
      if not ((List.mem tsym bvs) || (List.mem tsym fvs)) then
        (term, tsym::fvs)
      else
        (term, fvs)
  | Absyn.ConstantTerm(c,_,_,pos) ->
      if Parse.illegalConstant c pos then
        (Absyn.errorTerm, [])
      else
        (term, fvs)
  | Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym,body),b,pos) ->
      let (body', fvs') = collectFreeVars body fvs bvs in
      let term' = Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym, body'), b, pos) in
      (term', fvs')
  | Absyn.ApplicationTerm(_) ->
      let pos = Absyn.getTermPos term in
      let (head, args) = getClauseHeadAndArguments term in
      let (head', fvs') = collectFreeVars head fvs bvs in
      let (args', fvs'') = collectArgsFreeVars args fvs' bvs in
      let term' = Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(
          head',
          args',
          List.length args'),
        false,
        pos) in
      (term', fvs'')
  | _ -> Errormsg.impossible Errormsg.none "Clauses.collectFreeVars: Invalid term type."

(**********************************************************************
*collectArgsFreeVars:
* Collect the free variables of a list of terms, and return a list
* of rectified items, and the fvs.
**********************************************************************)
and collectArgsFreeVars args fvs bvs =
  match args with
    [] -> ([], fvs)
  | a::aa ->
      let (args', fvs') = collectArgsFreeVars aa fvs bvs in
      let (arg', fvs'') = collectFreeVars a fvs bvs in
      (arg'::args', fvs'')

(**********************************************************************
*distributeQuantifierTerms:
**********************************************************************)
and distributeQuantifierTerms 
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
            [Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym, t), false, Errormsg.none)], 1),
          false,
          Errormsg.none) in
        (newterm :: (distribute' term ts))
  in
  
  distribute' term terms

(**********************************************************************
*reverseDistributeImplication:
**********************************************************************)
and reverseDistributeImplication head goals cls =
  if List.length goals > 0 then
    let goal = List.hd goals in
    let goals' = List.tl goals in
    
    let appinfo = Absyn.FirstOrderApplication(Absyn.makeConstantTerm (Pervasive.implConstant) (Errormsg.none), [goal;head], 2) in
    let cls' = (Absyn.ApplicationTerm(appinfo, false, Errormsg.none)) :: cls in
    reverseDistributeImplication head goals' cls'
  else
    cls

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
        Absyn.FirstOrderApplication(imply, [goal; clause], 2),false, Errormsg.none)
    in
    
    match clause with
      Absyn.ApplicationTerm(Absyn.FirstOrderApplication(f, args, _), _, pos) ->
        (match f with
          Absyn.ConstantTerm(c,_,_,_) ->
            if c = Pervasive.allConstant then
              let arg1 = List.hd args in
              let tsym = Absyn.getTermAbstractionVar arg1 in
              let body = Absyn.getTermAbstractionBody arg1 in
              let pos' = Absyn.getTermPos body in
              Absyn.ApplicationTerm(
                Absyn.FirstOrderApplication(
                  f,
                  [Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym, distribute goal body), false, pos')],
                  1),
                false,
                Errormsg.none)
            else if c = Pervasive.implConstant then
              let arg1 = List.hd args in
              let arg2 = List.hd (List.tl args) in
              let inner = Absyn.ApplicationTerm(
                Absyn.FirstOrderApplication(
                  Pervasive.andTerm,
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
and deOrifyGoal goal uvs uvdefs andgoal wholegoal newclauses =
  let (t, args) = getClauseHeadAndArguments goal in
  
  match t with
    Absyn.ConstantTerm(c,_,_,pos) ->
      if c = Pervasive.allConstant then
        let arg1 = List.hd args in
        (deOrifyUniversalGoal t arg1 uvs uvdefs andgoal wholegoal newclauses)
      else if c = Pervasive.someConstant then
        let arg1 = List.hd args in
        (deOrifyExistentialGoal t arg1 uvs uvdefs andgoal wholegoal newclauses)
      else if c = Pervasive.orConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (deOrifyOrGoal arg1 arg2 uvs uvdefs andgoal wholegoal newclauses)
      else if c = Pervasive.andConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (deOrifyAndGoal arg1 arg2 uvs uvdefs andgoal wholegoal newclauses)
      else if c = Pervasive.implConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (deOrifyImplicationGoal arg1 arg2 uvs uvdefs andgoal false newclauses)
      else if c = Pervasive.colondashConstant then
        let arg1 = List.hd args in
        let arg2 = List.hd (List.tl args) in
        (deOrifyImplicationGoal arg2 arg1 uvs uvdefs andgoal false newclauses)
      else if c = Pervasive.cutConstant then
        (deOrifyCutGoal goal andgoal uvdefs newclauses)
      else
        (deOrifyAtomicGoal t args andgoal uvs uvdefs newclauses)
  | _ -> (deOrifyAtomicGoal t args andgoal uvs uvdefs newclauses)

(**********************************************************************
*deOrifyCutGoal:
* Deorifies a cut goal.  Records that the goal has a cut, but is
* otherwise routine.
**********************************************************************)
and deOrifyCutGoal goal andgoal uvdefs newclauses =
  let getGoal g andgoal =
    if Option.isSome andgoal then
      let and' = Option.get andgoal in
      Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(
          Pervasive.andTerm,
          [g; and'],
          2),
        false,
      Errormsg.none)
    else
      g
  in
  DeOrifyGoalResult([getGoal goal andgoal], [], uvdefs, true, newclauses, [])

(**********************************************************************
*deOrifyUniversalGoal:
**********************************************************************)
and deOrifyUniversalGoal t arg1 uvs uvdefs andgoal wholegoal newclauses =
  let (body, tsym) = etaFluffQuantifier t arg1 in
  let hc = makeHiddenConstant tsym in
  let uvs' = tsym :: uvs in
  
  let DeOrifyGoalResult(goals', fvs', uvdefs', hascut', newclauses', hcs') = deOrifyGoal body uvs' uvdefs None wholegoal newclauses in
  let hcs'' = hc :: hcs' in
  let uvdefs'' = closeUniversalDefinitions tsym uvdefs' in
  
  if List.mem tsym fvs' then
    let fvs'' = (List.filter ((<>) tsym) fvs') in
    let goals'' = distributeQuantifierTerms t tsym goals' andgoal in
    DeOrifyGoalResult(goals'', fvs'', uvdefs'', hascut', newclauses', hcs'')
  else
    let goals'' = distributeAndGoal goals' andgoal [] in
    DeOrifyGoalResult(goals'', fvs', uvdefs'', hascut', newclauses', hcs'')

(**********************************************************************
*deOrifyExistentialGoal:
**********************************************************************)
and deOrifyExistentialGoal t arg1 uvs uvdefs andgoal wholegoal newclauses =
  let (body, tsym) = etaFluffQuatifier t arg1 in
  let DeOrifyGoalResult(goals', fvs', uvdefs', hascut', newclauses', hcs') = deOrifyGoal body uvs uvdefs None wholegoal newclauses in
  
  if List.mem tsym fvs' then
    let fvs'' = (List.filter ((<>) tsym) fvs') in
    let goals'' = distributeQuantifierTerms t tsym goals' andgoal in
    DeOrifyGoalResult(goals'', fvs'', uvdefs'', hascut', newclauses', hcs'')
  else
    let goals'' = distributeAndGoal goals' andgoal [] in
    DeOrifyGoalResult(goals'', fvs', uvdefs'', hascut', newclauses', hcs'')

(**********************************************************************
*deOrifyImplicationGoal:
**********************************************************************)
and deOrifyImplicationGoal t arg1 uvs uvdefs andgoal wholegoal newclauses =
  Errormsg.impossible Errormsg.none "Clauses.deOrifyImplicationGoal: not implemented"

(**********************************************************************
*deOrifyAtomicGoal:
* For atomic goals other than cut. If the head is a uv, it must be replaced
* with a constant term made from the appropriate hidden constant. Otherwise,
* if it is a bound or free var, it counts as a free variable occurrence.
* Finally, arguments have to be checked for correctness, they have to be
* rectified and their free variables collected.
**********************************************************************)
and deOrifyAtomicGoal head args andgoal uvs uvdefs newclauses =
  let deorify' head hascut fvs uvdefs =
    (*  If the andgoal is not None, the head should actually be an
        application, with and ("," or "&") at the head, and the
        original head and the andgoal as arguments. *)
    let getGoal head andgoal =
      if Option.isSome andgoal then
          let andgoal' = Option.get andgoal in
          let head' = Absyn.ApplicationTerm(
            Absyn.FirstOrderApplication(
              Pervasive.andTerm,
              [head; andgoal'],
              2),
            false,
            Absyn.getTermPos head) in
          DeOrifyGoalResult([head'], fvs, uvdefs, hascut, newclauses)
        else
          DeOrifyGoalResult([head], fvs, uvdefs, hascut, newclauses)
    in
    
    match args with
      [] ->
          getGoal head andgoal
    | _ ->
        let (args', fvs') = collectArgsFreeVars args fvs [] in
        let head' = Absyn.ApplicationTerm(
          Absyn.FirstOrderApplication(
            head,
            args',
            List.length args'),
          false,
          Absyn.getTermPos head) in
        getGoal head' andgoal
  in
  
  match head with
    Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym),b,pos) ->
      if (List.mem tsym uvs) then
        let c = Absyn.getTypeSymbolHiddenConstant tsym in
        let head' = Absyn.ConstantTerm(c, [], false, pos) in
        (deorify' head' false [] uvdefs)
      else
        let head' = Absyn.FreeVarTerm(Absyn.NamedFreeVar(tsym), b, pos) in
        (deorify' head' false [tsym] uvdefs)
  | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tsym),_,pos) ->
      deorify' head false [tsym] uvdefs
  | _ -> deorify' head false [] uvdefs

(**********************************************************************
*deOrifyOneOrGoal:
* Processing one disjunct of a disjunctive goal. If this branch has a cut,
* then a new predicate must be introduced to prevent the cut from cutting off
* choice points in the parent clause.
**********************************************************************)
and deOrifyOneOrGoal goal uvs uvdefs andgoal newclauses=
  let DeOrifyGoalResult(goals', fvs', uvdefs', hascut', newclauses') =
    (deOrifyGoal goal uvs uvdefs andgoal true newclauses) in
  
  if hascut' then
    let (goal', newclauses') = newPredicate goals' fvs' newclauses in
    DeOrifyGoalResult([goal'], fvs', uvdefs',hascut', newclauses')
  else
    DeOrifyGoalResult(goals', fvs', uvdefs',hascut', newclauses')

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
and deOrifyOrGoal goal1 goal2 uvs uvdefs andgoal wholegoal newclauses =
  let getAndGoal wholegoal andgoal =
    if wholegoal then
      andgoal
    else
      None
  in
  let andgoal' = (getAndGoal wholegoal andgoal) in
  
  let result1 = deOrifyOneOrGoal goal2 uvs uvdefs andgoal' newclauses in
  
  let newclauses' = getDeOrifyGoalResultNewClauses result1 in
  let uvdefs' = getDeOrifyGoalResultUVDefs result1 in
  let result2 = deOrifyOneOrGoal goal1 uvs uvdefs' andgoal' newclauses' in
  
  let newclauses'' = getDeOrifyGoalResultNewClauses result2 in
  let fvs' = union (getDeOrifyGoalResultFVS result1) (getDeOrifyGoalResultFVS result2) in
  let uvdefs' = getDeOrifyGoalResultUVDefs result1 in
  
  if wholegoal then
    let goals1 = getDeOrifyGoalResultGoals result1 in
    let goals2 = getDeOrifyGoalResultGoals result2 in
    DeOrifyGoalResult(goals1 @ goals2, fvs', uvdefs', false, newclauses'')
  else
    let (pred, newclauses''') = newPredicateFromAnd
      (getDeOrifyGoalResultGoals result2)
      (getDeOrifyGoalResultGoals result1)
      (getDeOrifyGoalResultFVS result1)
      newclauses'' in

    if Option.isSome andgoal' then
      let goal' = Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(
          Pervasive.andTerm,
          [pred; Option.get andgoal'],
          2),
        false,
        Errormsg.none) in
      DeOrifyGoalResult([goal'], fvs', uvdefs', false, newclauses''')
    else
      DeOrifyGoalResult([pred], fvs', uvdefs', false, newclauses''')

(**********************************************************************
*deOrifyAndGoal:
**********************************************************************)
and deOrifyAndGoal goal1 goal2 uvs uvdefs andgoal wholegoal newclauses =
  let getAndGoal goals =
    match goals with
      [] -> None
    | g::gs -> Some g
  in
  
  let result1 = deOrifyGoal goal2 uvs uvdefs andgoal false newclauses in
  let r1goals = getDeOrifyGoalResultGoals result1 in
  let andgoal' = getAndGoal r1goals in
  
  let uvdefs' = getDeOrifyGoalResultUVDefs result1 in
  let newclauses' = getDeOrifyGoalResultNewClauses result1 in
  
  let result2 = deOrifyGoal goal1 uvs uvdefs' andgoal' wholegoal newclauses' in
  let fvs' = union (getDeOrifyGoalResultFVS result1) (getDeOrifyGoalResultFVS result2) in
  let hascut' = (getDeOrifyGoalResultHasCut result1) || (getDeOrifyGoalResultHasCut result2) in
  let uvdefs' = getDeOrifyGoalResultUVDefs result2 in
  let newclauses'' = getDeOrifyGoalResultNewClauses result2 in
  
  DeOrifyGoalResult(getDeOrifyGoalResultGoals result2, fvs', uvdefs', hascut', newclauses'')

(**********************************************************************
*deOrifyClause:
* Removes disjunctions from a clause.
**********************************************************************)
and deOrifyClause term currentclauses fvs uvs uvdefs newclauses =
  let checkClauseArguments args fvs =
    Errormsg.impossible Errormsg.none "Clauses.checkClauseArguments: not implemented"
  in
  
  match term with
    (*  An application  *)
    Absyn.ApplicationTerm(Absyn.FirstOrderApplication(f, arg::args, num), b, pos) ->
      (*  atomicHead: regular head with arguments.  *)
      let atomicHead () =  
        let (args', fvs') = checkClauseArguments args fvs in
        let newclause = Absyn.ApplicationTerm(Absyn.FirstOrderApplication(f, args', num),b,pos) in
        DeOrifyClauseResult(newclause::currentclauses, fvs', uvdefs, newclauses)
      in
      
      (match f with
        Absyn.ConstantTerm(c,_,_,_) ->
          if c = Pervasive.allConstant then
            let tsym = Absyn.getTermAbstractionVar arg in
            let DeOrifyClauseResult(clauses', fvs', uvdefs', newclauses') = deOrifyClause arg [] fvs uvs uvdefs newclauses in
            let clauses'' = distributeUniversal f tsym clauses' in
            let fvs'' = List.filter ((=) tsym) fvs' in
            DeOrifyClauseResult(clauses'' @ currentclauses, fvs'', uvdefs, newclauses')
            
          else if c = Pervasive.implConstant then
            let DeOrifyGoalResult(goals, fvs', uvdefs', hascut', newclauses') = deOrifyGoal arg uvs uvdefs None false newclauses in
            let DeOrifyClauseResult(clauses', fvs'', uvdefs'', newclauses'') = deOrifyClause (List.hd args) [] (union fvs fvs') uvs uvdefs' newclauses' in
            let t = List.hd clauses' in
            let clauses'' = reverseDistributeImplication t goals currentclauses in
            DeOrifyClauseResult(clauses'', fvs'', uvdefs'', newclauses'')
            
          else
            atomicHead ()
            
      | _ ->
          atomicHead ())
    
    (*  The term is a propositional head.  *)
  | _ -> DeOrifyClauseResult(term::currentclauses, fvs, uvdefs, newclauses)

(**********************************************************************
*deOrifyClauses:
* Removes disjunctions from a list of clauses.  The result is a list
* of absyn clauses that don't contain disjunctions.
**********************************************************************)
and deOrifyClauses clauses currentclauses uvs newclauses =
  match clauses with
    [] ->
      DeOrifyClauseResult(currentclauses, [], [], newclauses)
  | c::cs ->
      let DeOrifyClauseResult(clauses', fvs', uvdefs', newclauses') = deOrifyClauses cs currentclauses uvs newclauses in
      (deOrifyClause c clauses' fvs' uvs uvdefs' newclauses')

(**********************************************************************
*etaFluffQuantifier:
* Transforms the body of a quantified term into one that has an
* abstraction structure at the top; resulting body and bound variable
* are returned.
**********************************************************************)
and etaFluffQuantifier term arg =
  match term with
    Absyn.AbstractionTerm(_) ->
      (Absyn.getTermAbstractionBody arg, Absyn.getTermAbstractionVar arg)
  | _ ->
      let tenv = Absyn.getTermTypeEnv term in
      let tsym = Absyn.BoundVar(Symbol.generate (), ref None, ref true, ref(Some(List.hd tenv))) in
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
  let indeterminate tsym pos =
    (Errormsg.error pos("Predicate name " ^
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
          Absyn.FirstOrderApplication(term, args, List.length args),
          false, Errormsg.none) in
        (term' :: clauses)
  in
  
  (*  Bail if the term is an error. *)
  if clauseterm = Absyn.errorTerm then
    clauseterm::clauses
  else
  
  let (t, args) = getClauseHeadAndArguments clauseterm in
  
  match t with
    Absyn.ConstantTerm(c, env, b, pos) ->
      (*  Clause is: all P  *)
      if c = Pervasive.allConstant then
        let (arg, tsym) = etaFluffQuantifier t (List.hd args) in
        (distributeUniversal t tsym (normalizeClause arg [] uvs uvdefs embedded))
      
      (*  Clause is: a, b or a & b  *)
      else if c = Pervasive.andConstant then
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
          (Errormsg.info (Absyn.getConstantName c)));
        clauses)
      
      else if (Absyn.getConstantNoDefs c) then
        (Errormsg.error (Absyn.getTermPos t) ("clauses for constant " ^
          (Absyn.getConstantName c) ^ " prohibited in this module");
        clauses)
      
      else if embedded then
        ((Absyn.getConstantClosedRef c) := false;
        rigidAtom t clauses args)
      else
        (rigidAtom t clauses args)
  | Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym), _, pos) ->
      if List.mem tsym uvs then
        let t' = Absyn.ConstantTerm(Absyn.getTypeSymbolHiddenConstant tsym,
          [], false, Errormsg.none) in
        if not (List.mem tsym !uvdefs) then
          (uvdefs := tsym :: !uvdefs;
          rigidAtom t' clauses args)
        else
          (rigidAtom t' clauses args)
      else
        (indeterminate tsym pos)
  | _ ->
      (Errormsg.impossible (Absyn.getTermPos t) "Clauses.normalizeClause: invalid term type")
      
(**********************************************************************
*newKind:
* Callback used when the term parser encounters an undefined kind.
* Any undefined kind is flagged as an error.
**********************************************************************)
and newKind sym arity pos ktable=
  let k = Absyn.LocalKind(sym, Some arity, ref 0, pos) in
  let ktable' = Table.add sym k ktable in
  (if arity > 0 then
    Errormsg.error pos ("undefined type constructor " ^
      (Symbol.name sym) ^ " of arity " ^ (string_of_int arity))
  else
    Errormsg.error pos ("undefined sort " ^ (Symbol.name sym));
  ktable')

(**********************************************************************
*newConstant:
* Callback used whn the term parser encounters an undefined constant.
*************=********************************************************)
and newConstant sym ctable =
  ctable

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
  let translateClause preclause amod clauses newclauses =
    let preterm = Preabsyn.getClauseTerm preclause in
    let clause =
      Parse.translateClause preterm amod newConstant newKind in
    
    let uvdefs = ref [] in
    let clauses' = normalizeClause clause [] [] uvdefs false in
    let DeOrifyClauseResult(cls, _, _, newcls) = (deOrifyClauses clauses' clauses [] newclauses) in
    (cls, newcls)
  in
  
  (********************************************************************
  *parse':
  * Auxiliary function that iterates over a list of preabsyn clauses,
  * translates them, and flattens the resulting lists of clauses.
  ********************************************************************)
  let rec parse' preclauses amod clauses newclauses =
    match preclauses with
      [] -> (clauses, newclauses)
    | c::cs ->
        let (clauses', newclauses') = translateClause c amod clauses newclauses in
        parse' cs amod clauses' newclauses'

  in  
  let preclauses = Preabsyn.getModuleClauses pmod in
  let (clauses', newclauses') = parse' preclauses amod [] [] in
  
  
  (*  Enter them into the module  *)
  (*  let _ = (Absyn.getModuleClausesRef amod) := clauses' @ (List.map getNewClauseClause) newclauses') in  *)
  amod
