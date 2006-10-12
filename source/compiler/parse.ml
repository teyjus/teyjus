type pos = Errormsg.pos
type symbol = Symbol.symbol

(*  Terms and Terms with Free Variables *)
type ptvarlist = Absyn.atypesymbol Table.SymbolTable.t
type ptterm = Term of (Absyn.aterm * Types.typemolecule)
type ptfixedterm = FixedTerm of (Absyn.afixedterm * Types.typemolecule)
type pttermandvariables = TermAndVariables of (ptterm * ptvarlist)
type ptfixedtermandvariables = FixedTermAndVariables of (ptfixedterm * ptvarlist)

(*  Functions indicating what to do when a new constant or kind is encountered. *)
type ptnewconstant = Absyn.aconstant -> (Absyn.aconstant Table.SymbolTable.t) -> (Absyn.aconstant Table.SymbolTable.t)
type ptnewkind = Absyn.akind -> Absyn.akind Table.SymbolTable.t -> Absyn.akind Table.SymbolTable.t

(*  Term Accessors  *)
let getTermTerm = function Term(t, _) -> t
let getTermMolecule = function Term(_, mol) -> mol
let getFixedTermTerm = function Term(t, _) -> t
let getFixedTermTypeMolecule = function Term(_, mol) -> mol
let getTermAndVariablesTerm = function TermAndVariables(t, _) -> t
let getTermAndVariablesVariables = function TermAndVariables(_, vars) -> vars

let errorTerm = Term(Absyn.errorTerm, Types.errorMolecule)
let errorFixedTerm = FixedTerm(Absyn.errorFixedTerm, Types.errorMolecule)
let errorTermAndVariables = TermAndVariables(errorTerm, Table.SymbolTable.empty)
let errorFixedTermAndVariables = FixedTermAndVariables(errorFixedTerm, Table.SymbolTable.empty)
let getTermType = function
  Term(_,ty) -> ty

let getTermTerm = function
  Term(t,_) -> t

let getTermAndVariablesTerm = function
  TermAndVariables(term,_) -> term

(**********************************************************************
*Term-parsing types.
**********************************************************************)

(*  Stack and stack items.  *)
type ptstackdata =
  StackTerm of (ptterm)
| StackOp of (Absyn.aconstant * Absyn.atype list * pos)
| StackError

(*  Parser State  *)
type ptparsestate =
  PrefixState
| PrefixrState
| PrefixWithArgState
| PrefixrWithArgState
| InfixState
| InfixrState
| InfixWithArgState
| InfixrWithArgState
| PostfixState
| TermState
| NoneState
| ErrorState

let string_of_parserstate = function
  PrefixState -> "PrefixState"
| PrefixrState -> "PrefixrState"
| PrefixWithArgState -> "PrefixWithArgState"
| PrefixrWithArgState -> "PrefixrWithArgState"
| InfixState -> "InfixState"
| InfixrState -> "InfixrState"
| InfixWithArgState -> "InfixWithArgState"
| InfixrWithArgState -> "InfixrWithArgState"
| PostfixState -> "PostfixState"
| TermState -> "TermState"
| NoneState -> "NoneState"
| ErrorState -> "ErrorState"

type ptstack = Stack of (ptstackdata list * ptparsestate * int * Absyn.afixity)

let getStackItemPos = function
    StackTerm(t) -> (Absyn.getTermPos (getTermTerm t))
  | StackOp(_,_,p) -> p
  | StackError -> (Errormsg.impossible Errormsg.none "Parse.getStackItemPos: stack error.")

let getStackTermTerm = function
    StackTerm(t) -> t
  | s -> Errormsg.impossible (getStackItemPos s) "getStackTermTerm: invalid stack term."

let getStackOpConstant = function
    StackOp(c,_,_) -> c
  | s -> Errormsg.impossible (getStackItemPos s) "getStackOpConstant: invalid stack op."

let getStackOpEnv = function
    StackOp(_,e,_) -> e
  | s -> Errormsg.impossible (getStackItemPos s) "getStackOpEnv: invalid stack op."

let getStackOpFixity = function
    StackOp(c,_,_) -> (Absyn.getConstantFixity c)
  | _ -> Errormsg.impossible Errormsg.none "getStackOpFixity: invalid stack op."

let getStackOpPrec = function
    StackOp(c,_,_) -> (Absyn.getConstantPrec c)
  | _ -> Errormsg.impossible Errormsg.none "getStackOpPrec: invalid stack op."

(*  Stack Accessors *)
let getStackStack = function Stack(data,_,_,_) -> data
let getStackState = function Stack(_,state,_,_) -> state
let getStackPrec = function Stack(_,_,prec,_) -> prec
let getStackFixity = function Stack(_,_,_,fix) -> fix

let printStack = fun stack ->
  let rec print' = fun data ->
    match data with
      (StackOp(c,_,_))::ds -> "{" ^ (Absyn.getConstantName c) ^ "}" ^ (print' ds)
    | (StackTerm(t))::ds -> "{" ^ (Absyn.string_of_term (getTermTerm t)) ^ "}" ^ (print' ds)
    | (StackError)::ds -> "{StackError}" ^ (print' ds)
    | [] -> ""
  in
  
  let data = getStackStack stack in
  let str = (print' data) in
  print_endline ("Stack: " ^ str)

let contains = fun env v ->
  match (Table.find v env) with
    Some(_) -> true
  | None -> false

let get = fun env v ->
  match (Table.find v env) with
    Some(v) -> v
  | None -> Errormsg.impossible Errormsg.none "Parse.get: entry not found"

let add = fun env sym tsym ->
  (Table.add sym tsym env)

let newStack = Stack([], NoneState, 0, Absyn.NoFixity)
let errorStack = Stack([], ErrorState, 0, Absyn.NoFixity)

exception TermException

(**********************************************************************
* Beta-normalization types.
**********************************************************************)
type environmententry =
    TermEntry of (Absyn.aterm)
  | SuspensionEntry of (Absyn.aterm * environmentcell list)

and environmentcell =
    EnvironmentCell of (Absyn.atypesymbol * environmententry ref)

(**********************************************************************
*Error functions.
**********************************************************************)
let idTypeError = fun tmol result pos ->
  let info =
    Errormsg.info ("Type previously determined for variable: " ^ (Types.string_of_typemolecule tmol))
  in
  
  match result with
    Types.Success -> Errormsg.impossible Errormsg.none "Parse.idTypeError: unexpected unification result"
  | Types.OccursCheckFailure -> Errormsg.error pos ("circularity discovered during type matching" ^ info)
  | Types.ClashFailure -> Errormsg.error pos ("incompatibility discovered during type matching" ^ info)

let constantTypeError = fun tmol result pos ->
  let info =
    Errormsg.info ("Type previously defined or determined for constant: " ^ (Types.string_of_typemolecule tmol))
  in
  
  match result with
    Types.Success -> Errormsg.impossible Errormsg.none "Parse.constantTypeError: unexpected unification result"
  | Types.OccursCheckFailure -> Errormsg.error pos ("circularity discovered during type matching" ^ info)
  | Types.ClashFailure -> Errormsg.error pos ("incompatibility discovered during type matching" ^ info)

(**********************************************************************
*makeType:
* Build a term and type representation of a term with a given (named
* pervasive) type.
**********************************************************************)
let makeType = fun term s ktable pos ->
  match (Table.find (Symbol.symbol s) ktable) with
    Some(k) ->
      let ty = Types.makeKindType k in
      Term(term, ty)
  | None -> (Errormsg.impossible (Absyn.getTermPos term) ("Parse.makeType: invalid kind " ^ s))

let makeConstant = fun c pos ->
  let ty = Types.makeConstantType c in
  let env = Types.getMoleculeEnvironment ty in
  Term(Absyn.ConstantTerm(c,env,pos), ty)

(**********************************************************************
*stackTop:
* Returns the top of the given stack.
**********************************************************************)
let stackTop = function
  Stack(s::_,_,_,_) -> s
| Stack([],_,_,_) -> (Errormsg.impossible Errormsg.none "Parse.stackTop: stack is empty.")

(**********************************************************************
*popStack:
* Removes the top of the given stack and returns the new stack.
**********************************************************************)
let popStack = function
  Stack(_::ss,a,b,c) -> Stack(ss, a, b, c)
| Stack([],_,_,_) -> (Errormsg.impossible Errormsg.none "Parse.popStack: stack is empty.")

(**********************************************************************
*translateClauses:
* Given an abstract syntax representation of a module and a list of
* preabsyn terms, generates a list of absyn clauses.
**********************************************************************)
let rec translateClauses = fun terms amodule ->
  Errormsg.impossible Errormsg.none "Parse.translateClauses: unimplemented."
 
(**********************************************************************
*translateTerm:
* Given an abstract syntax representation of a module and a preabsyn
* term, generates a normalized absyn term.
**********************************************************************)
let rec translateTerm = fun term amodule ->
  let _ = Errormsg.log (Preabsyn.getTermPos term) ("unparsed preabsyn: " ^ (Preabsyn.string_of_term term)) in
  let (tv,_,_,_) = parseTerm term Table.SymbolTable.empty Table.SymbolTable.empty amodule newStack in
  let term' = getTermTerm (getTermAndVariablesTerm tv) in
  let mol' = getTermMolecule (getTermAndVariablesTerm tv) in
  let _ = Errormsg.log (Absyn.getTermPos term') ("parsed term: " ^ (Absyn.string_of_term term') ^ " : " ^ (Types.string_of_typemolecule mol')) in
  let term'' = normalizeTerm term' in
  let _ = Errormsg.log (Absyn.getTermPos term'') ("parsed, normalized term: " ^ (Absyn.string_of_term term'')) in
  let fixedterm = fixTerm term' in
  (Errormsg.log (Absyn.getTermPos term'') ("parsed, normalized, fixed term: " ^ (Absyn.string_of_fixedterm fixedterm));
  fixedterm)

(**********************************************************************
*parseTerm:
* Translate a single term from preabsyn to absyn.  Requires a constant
* table containing, at least, the pervasives; a kind table also containing
* at least the prevasives; and an abbreviation table containing at least
* the pervasives.  These are given in an absyn representation of a
* module.
**********************************************************************)
and parseTerm = fun term fvs bvs amodule stack ->
  match term with
    Preabsyn.SeqTerm([term], pos) -> (parseTerm term fvs bvs amodule stack)
  | Preabsyn.SeqTerm(terms, pos) -> (parseTerms terms fvs bvs amodule (reduceToTerm) newStack)
  | Preabsyn.ListTerm(terms, pos) -> (parseTerms terms fvs bvs amodule (reduceToListTerm (Some(makeConstant Pervasive.nilConstant pos))) newStack)
  | Preabsyn.ConsTerm(headterms, tailterm, pos) ->
      (*  Translate the tail term first, then translate the head with respect to the new
          set of free variables.  Reduce using the tail term. *)
      let (tv, _, bvs', stack') = (parseTerm tailterm fvs bvs amodule stack) in
      let TermAndVariables(term', fvs') = tv in
      (parseTerms headterms fvs' bvs' amodule (reduceToListTerm (Some term')) newStack)
  | Preabsyn.LambdaTerm(b, t, pos) ->
      let bvs' = parseTypeSymbols b in
      Errormsg.impossible pos "Parse.parseTerm: lambda term unimplemented."
  | Preabsyn.IntTerm(i, pos) -> (TermAndVariables((makeType (Absyn.IntTerm(i, pos)) "int" (Absyn.getModuleKindTable amodule) pos), fvs), fvs, bvs, stack)
  | Preabsyn.RealTerm(r, pos) -> (TermAndVariables((makeType (Absyn.RealTerm(r, pos)) "real" (Absyn.getModuleKindTable amodule) pos), fvs), fvs, bvs, stack)
  | Preabsyn.StringTerm(s, pos) -> (TermAndVariables((makeType (Absyn.StringTerm(s, pos)) "string" (Absyn.getModuleKindTable amodule) pos), fvs), fvs, bvs, stack)
  | Preabsyn.IdTerm(sym, ty, idkind, pos) ->
      let (op', fvs', bvs') = (translateId term fvs bvs amodule) in
      (match op' with
        StackOp(_) -> 
          (Errormsg.error pos ("operator used without necessary arguments");
          (makeError fvs, fvs', bvs', errorStack))
      | StackTerm(t) -> (TermAndVariables(t, fvs'), fvs', bvs', stack))
  | Preabsyn.ErrorTerm -> (Errormsg.impossible Errormsg.none "Parse.parseTerm: error term encountered.")

(**********************************************************************
*parseTerms:
* Translate a list of preabsyn terms into abstract syntax.  Executes
* the given operation upon successful translation of the list.
**********************************************************************)
and parseTerms = fun terms fvs bvs amodule oper stack ->
  (********************************************************************
  *translate':
  * Translate an individual term in the list.
  ********************************************************************)
  let translate' = fun t ->
    (******************************************************************
    *simple:
    * Translate the current term as usual and attempt to stack it.
    ******************************************************************)
    let simple = fun () ->
      let (term', fvs', bvs', stack') = (parseTerm t fvs bvs amodule stack) in

      try
        ((stackTerm (getTermAndVariablesTerm term') None amodule stack'), fvs', bvs')
      with
        TermException -> (errorStack, bvs, fvs)
    in
    
    (match t with
      Preabsyn.SeqTerm(_) -> simple ()
    | Preabsyn.ListTerm(_) -> simple ()
    | Preabsyn.ConsTerm(_) -> simple ()
    | Preabsyn.IntTerm(_) -> simple ()
    | Preabsyn.RealTerm(_) -> simple ()
    | Preabsyn.StringTerm(_) -> simple ()
    | Preabsyn.IdTerm(_) ->
        let (ot, fvs', bvs') = (translateId t fvs bvs amodule) in
        (try
          (match ot with
            StackOp(_) -> ((stackOperation ot None amodule stack), fvs', bvs')
          | StackTerm(t) -> ((stackTerm t None amodule stack), fvs', bvs')
          | StackError -> (errorStack, fvs', bvs'))
        with
          TermException -> (errorStack, fvs', bvs'))
    | Preabsyn.ErrorTerm -> (errorStack, fvs, bvs))
  in

  (*  Translate each term in turn.  Once the end of the list has been
      reached, execute the given termination operation. *)
  match terms with
    (t::ts) ->
      let (stack', fvs', bvs') = translate' t in
      (parseTerms ts fvs' bvs' amodule oper stack')
  | [] -> (oper fvs bvs amodule stack)

(**********************************************************************
*parseTypeSymbols:
* Parses a list of type symbols.
**********************************************************************)
and parseTypeSymbols =
  Errormsg.impossible Errormsg.none "Parse.parseTypeSymbols: unimplemented."

(**********************************************************************
*translateId:
* Converts a preabsyn ID to an absyn representation.  How it does so
* depends on what sort of ID it is.  A better description is in order.
**********************************************************************)
and translateId = fun term fvs bvs amodule ->
  let Preabsyn.IdTerm(sym, ty, k, pos) = term in
  
  (*  Anonymous Variable  *) 
  if k = Preabsyn.AVID then
    (makeVarToOpTerm term fvs bvs amodule makeAnonymousTypeSymbol)
  else

  (*  Variable  *)
  if k = Preabsyn.VarID then
    if (contains fvs sym) then
      (varToOpTerm term (get fvs sym) fvs bvs amodule (Absyn.makeFreeVarTerm))
    else
      (makeVarToOpTerm term fvs bvs amodule makeAnonymousTypeSymbol)
  else
  
  if (contains bvs sym) then
    (varToOpTerm term (get bvs sym) fvs bvs amodule (Absyn.makeBoundVarTerm))
  else
  
  match (Table.find sym (Absyn.getModuleConstantTable amodule)) with
    Some(c) -> (constantToOpTerm term c fvs bvs amodule)
  | None ->
  
    if k = Preabsyn.CVID then
      if (contains fvs sym) then
        (varToOpTerm term (get fvs sym) fvs bvs amodule Absyn.makeFreeVarTerm)
      else
        (makeVarToOpTerm term fvs bvs amodule makeImplicitTypeSymbol)
    else
    
    (*  At this point the constant is assumed to be unknown.
        Simply raise an error.  *)
    (Errormsg.error pos ("undeclared constant " ^ (Symbol.name sym));
    (StackError, fvs, bvs))

and constantToOpTerm = fun term constant fvs bvs amodule ->
  let make' = fun tmol pos ->
    let fixity = Absyn.getConstantFixity constant in
    if fixity = Absyn.NoFixity then
      (StackTerm(Term(Absyn.ConstantTerm(constant, (Types.getMoleculeEnvironment tmol), pos), tmol)), fvs, bvs)
    else
      (StackOp(constant, (Types.getMoleculeEnvironment tmol), pos), fvs, bvs)
  in
  match term with
    Preabsyn.IdTerm(sym, Some(ty), k, pos) ->
      let tm1 = Types.makeConstantType constant in
      let tm2 = Types.Molecule((Translate.translateType ty amodule), [], false) in
      
      let result = (Types.unify tm1 tm2) in
      if result = Types.Success then
        (make' tm1 pos)
      else
        (constantTypeError tm1 result pos;
        (StackError, fvs, bvs))
  | Preabsyn.IdTerm(sym, ty, k, pos) ->
      let tm1 = Types.makeConstantType constant in
      (make' tm1 pos)
  | _ -> Errormsg.impossible (Preabsyn.getTermPos term) "Parse.constantToOpTerm: invalid term."

and makeImplicitTypeSymbol = fun c sym tysy ->
  Absyn.ImplicitTypeSymbol(false, c, sym, tysy)
and makeBoundTypeSymbol = fun c sym tysy ->
  Absyn.BoundTypeSymbol(false, c, sym, tysy)
and makeAnonymousTypeSymbol = fun c sym tysy ->
  Absyn.AnonymousTypeSymbol(false, c, sym, tysy)

and makeVarToOpTerm = fun term fvs bvs amodule makeSymFunc ->
  match term with
    Preabsyn.IdTerm(sym, Some(ty), k, pos) ->
      let skel = Translate.translateType ty amodule in
      let tmol = Types.Molecule(skel, [], false) in
      let typesym = makeSymFunc None sym (Absyn.RawType(skel)) in
      let fvs' = (add fvs sym typesym) in
      (StackTerm(Term(Absyn.makeFreeVarTerm typesym pos, tmol)), fvs', bvs)
  | Preabsyn.IdTerm(sym, None, k, pos) ->
      let skel = Absyn.TypeVarType(ref None, false) in
      let tmol = Types.Molecule(skel, [], false) in
      let typesym = makeSymFunc None sym (Absyn.RawType(skel)) in
      let fvs' = (add fvs sym typesym) in
      (StackTerm(Term(Absyn.makeFreeVarTerm typesym pos, tmol)), fvs', bvs)
  | _ -> Errormsg.impossible Errormsg.none "Parse.makeVarToOpTerm: invalid id term"

and varToOpTerm = fun term typesym fvs bvs amodule makeVarFunc ->
  match term with
    Preabsyn.IdTerm(sym, Some(ty), k, pos) ->
      (*  If the term has a given type, attempt to unify it with the
          type associated with the given symbol.  *)  
      let tm1 = Types.Molecule(Absyn.getTypeSymbolRawType typesym, [], false) in
      let tm2 = Types.Molecule((Translate.translateType ty amodule), [], false) in
      let result = (Types.unify tm1 tm2) in
      if result = Types.Success then
        (StackTerm(Term(makeVarFunc typesym pos, tm2)), fvs, bvs)
      else
        (idTypeError tm1 result pos;
        (StackError, fvs, bvs))

  | Preabsyn.IdTerm(sym, None, k, pos) ->
     (*  If the term has no given type, simply create a new type variable and
         bind it.  *)
     let tm1 = Types.Molecule(Absyn.getTypeSymbolRawType typesym, [], false) in
     (StackTerm(Term(makeVarFunc typesym pos, tm1)), fvs, bvs)
  | _ -> Errormsg.impossible (Preabsyn.getTermPos term) "Parse.varToOpTerm: invalid term"

(**********************************************************************
*reduceToTerm:
* Reduces the given stack to a term.  Commas encountered in this mode
* are interpereted as conjunctions.
**********************************************************************)
and reduceToTerm = fun fvs bvs amodule stack ->
  (********************************************************************
  *reduce':
  * Reduces the term.
  ********************************************************************)
  let rec reduce' = fun stack ->
      match (getStackState stack) with
        TermState ->
          let term = (getStackTermTerm (stackTop stack)) in
          (TermAndVariables(term, fvs), fvs, bvs, newStack)
      | ErrorState ->
          ((makeError fvs), fvs, bvs, errorStack)
      | _ ->
          try
            let stack' = reduceOperation None amodule stack in
            (reduce' stack')
          with 
            TermException -> ((makeError fvs), fvs, bvs, errorStack)
  in
  
  (*  Called when the top of the stack indicates an error.  *)
  let err = fun () ->
    let term = (getTermTerm (getStackTermTerm (stackTop stack))) in
    let pos = Absyn.getTermPos term in
    let fixity = (Absyn.string_of_fixity (Absyn.getConstantFixity (getStackOpConstant (stackTop stack)))) in
    
    (Errormsg.error pos ("missing right argument for " ^ fixity ^ " operator");
    (makeError fvs, fvs, bvs, errorStack))
  in

  match (getStackState stack) with
    PrefixState -> (err ())
  | PrefixrState -> (err ())
  | InfixState -> (err ())
  | InfixrState -> (err ())
  | _ -> (reduce' stack)

(**********************************************************************
*reduceToListTerm:
* Reduces the stack to a list term.  Commas encountered in this mode
* are interpereted as list separators.  Appends the given term to the
* parsed list.
**********************************************************************)
and reduceToListTerm = fun list fvs bvs amodule stack ->
  let rec reduce' = fun stack ->
    let default = fun () ->
      try
        let stack' = (reduceOperation list amodule stack) in
        (reduce' stack')
      with
        TermException -> (makeError fvs, fvs, bvs, errorStack)
    in
    match (getStackState stack) with
      TermState ->
        let term' = (getStackTermTerm (stackTop stack)) in
        (TermAndVariables(term', fvs), fvs, bvs, newStack)
    | InfixrWithArgState ->
        let top::ot::rest = getStackStack stack in
        let Some(item) = list in
        if (getStackOpConstant ot) = (Pervasive.consConstant) then
          let pos = getStackItemPos top in
          let cons = (makeConstant Pervasive.consConstant pos) in
          let term' = makeBinaryApply cons (getStackTermTerm top) item in
          let stack' = Stack((StackTerm(term'))::ot::rest, getStackState stack, getStackPrec stack, getStackFixity stack) in
          (reduceToTerm fvs bvs amodule stack')
        else
          default ()
    | _ -> default ()
  in
  
  (*  Called when the top of the stack indicates an error.  *)
  let err = fun () ->
    let top = (stackTop stack) in
    let pos = getStackItemPos top in
    let c = getStackOpConstant top in
    let fix = (Absyn.string_of_fixity (Absyn.getConstantFixity c)) in
    (Errormsg.error pos ("missing right argument for " ^ fix ^ " operator");
    (makeError fvs, fvs, bvs, errorStack))
  in
  
  match (getStackState stack) with
    PrefixState -> (err ())
  | PrefixrState -> (err ())
  | InfixState -> (err ())
  | InfixrState -> (err ())
  | _ ->
    let Some(item) = list in
    let (term, fvs', bvs', stack') = (reduce' stack) in
    let pos = (Absyn.getTermPos (getTermTerm (getTermAndVariablesTerm term))) in
    let cons = (makeConstant Pervasive.consConstant pos) in
    let term' = makeBinaryApply cons (getTermAndVariablesTerm term) item in
    (TermAndVariables(term', fvs'), fvs', bvs', stack')

(**********************************************************************
*reduceOperation:
**********************************************************************)
and reduceOperation = fun list amodule stack ->
  (********************************************************************
  *reduce':
  ********************************************************************)
  let reduce' = fun () ->
    let state = getStackState stack in
    let prec = getStackPrec stack in
    let fix = getStackFixity stack in
    let top::prev::rest = getStackStack stack in

    let reducePrefix = fun () ->
      let c = (makeConstant (getStackOpConstant prev) (getStackItemPos prev)) in
      let newTop = StackTerm(makeApply c (getStackTermTerm top)) in
      let stack' = Stack(newTop::rest, state, prec, fix) in
      newParseState stack'
    in
    
    let reduceInfix = fun () ->
      let r::rest' = rest in
      let c = (makeConstant (getStackOpConstant prev) (getStackItemPos prev)) in
      let newTop = StackTerm(makeBinaryApply c (getStackTermTerm r) (getStackTermTerm top)) in
      let stack' = Stack(newTop::rest', state, prec, fix) in
      newParseState stack'
    in
    
    match state with
      PostfixState ->
        let c = (makeConstant (getStackOpConstant top) (getStackItemPos top)) in
        let newTop = StackTerm(makeApply c (getStackTermTerm prev)) in
        let stack' = Stack(newTop::rest, state, prec, fix) in
        newParseState stack'
    | PrefixWithArgState -> reducePrefix ()
    | PrefixrWithArgState -> reducePrefix ()
    | InfixWithArgState -> reduceInfix ()
    | InfixrWithArgState -> reduceInfix ()
    | _ -> (Errormsg.impossible Errormsg.none "reduceOperation: invalid stack state")
  in
  
  let reduceApply = fun stack ->
    let top::_::pp::rest = (getStackStack stack) in
    let apply = makeApply (getStackTermTerm pp) (getStackTermTerm top) in
    let stack' = Stack((StackTerm(apply))::rest, (getStackState stack), (getStackPrec stack), (getStackFixity stack)) in
    newParseState stack'
  in
  
  let reduceComma = fun stack ->
    let top::comma::pp::rest = (getStackStack stack) in
    match list with
      Some(term) ->
        let pos = (getStackItemPos comma) in
        let cons = makeConstant Pervasive.consConstant pos in
        let newTop = makeBinaryApply cons (getStackTermTerm pp) (getStackTermTerm top) in
        let stack' = Stack((StackTerm(newTop))::rest, getStackState stack, getStackPrec stack, getStackFixity stack) in
        newParseState stack'
    | None ->
        let pos = getStackItemPos comma in
        let conj = makeConstant Pervasive.commaConstant pos in
        let newTop = makeBinaryApply conj (getStackTermTerm pp) (getStackTermTerm top) in
        let stack' = Stack((StackTerm(newTop))::rest, getStackState stack, getStackPrec stack, getStackFixity stack) in
        newParseState stack'
  in
  
  (*  First, check for the special case of a "pseudo-application".  In this case
      the reduction is slightly different than regular: the two arguments of the
      application are applied to eachother.
      
      Next, check for a comma.  If the list argument indicates that the parser
      is parsing a list (in bracket notation) the comma will be interpereted as
      a cons (::), otherwise it will be interpreted as a conjunction (,).
      
      Next, check for a sigma, pi, or lambda term.
  *)
  let _::ot::_ = (getStackStack stack) in
  match ot with
    StackOp(c,_,_) -> if (Absyn.getConstantSymbol c) = (Absyn.getConstantSymbol Pervasive.genericApplyConstant) then
                        (reduceApply stack)
                      else if (Absyn.getConstantSymbol c) = (Absyn.getConstantSymbol Pervasive.commaConstant) then
                        (reduceComma stack)
                      else
                        reduce' ()
  | _ -> (reduce' ())


(**********************************************************************
*stackTerm:
**********************************************************************)
and stackTerm = fun term list amodule stack ->
  (********************************************************************
  *stackApply:
  * Stacks a 'pseudo' apply operator.
  ********************************************************************)
  let stackApply = fun () ->
    let apply = (makeApplyOp (Absyn.getTermPos (getTermTerm term))) in
    let stack' = stackOperation apply list amodule stack in
    (pushTerm term stack')
  in

  (*  Check whether to add a pseudo application operator. *)
  let state = getStackState stack in
  match state with
    PrefixState -> (pushTerm term stack)
  | PrefixrState -> (pushTerm term stack)
  | InfixState -> (pushTerm term stack)
  | InfixrState -> (pushTerm term stack)
  | NoneState -> (pushTerm term stack)
  
  | PrefixWithArgState -> stackApply ()
  | PrefixrWithArgState -> stackApply ()
  | InfixWithArgState -> stackApply ()
  | InfixrWithArgState -> stackApply ()
  | PostfixState -> stackApply ()
  | TermState -> stackApply ()
  | _ -> Errormsg.impossible (Absyn.getTermPos (getTermTerm term)) ("Parse.stackTerm: invalid stack state " ^ (string_of_parserstate state))

(**********************************************************************
*stackOperation:
* Handles stacking an operator on the parse stack; this is where the
* main decisions of shifting/reducing get made.  Note that an apply
* "pseudo" operator may have to be inserted between two adjacent terms
* in the input to ensure the appropriate reduction at a later stage.
**********************************************************************)
and stackOperation = fun o list amodule stack ->
  let rec stackOperation' = fun stack ->
    let state = (getStackState stack) in
    let fixity = (getStackOpFixity o) in
    let prec = (getStackOpPrec o) in
    let pos = (getStackItemPos o) in
    
    (******************************************************************
    *preOrInf:
    * Handles the case where stack state is Prefix or Infix.
    ******************************************************************)
    let preOrInf = fun stack ->
      if (Absyn.isFixityPrefix fixity) && (prec > (getStackPrec stack)) then
        (pushOperation o stack)
      else if (Absyn.isFixityPrefix fixity) then
        (Errormsg.error pos "conflict in operator precedences.";
        raise TermException)
      else
        (Errormsg.error pos 
          ("missing left argument for " ^
          (Absyn.string_of_fixity fixity) ^
          " operator");
        raise TermException)
    in

    (******************************************************************
    *preOrInfr:
    * Handles the case where state is PrefixrState or InfixrState.
    ******************************************************************)
    let preOrInfr = fun stack ->
      if (Absyn.isFixityPrefix fixity) && (prec >= (getStackPrec stack)) then
        pushOperation o stack
      else if (Absyn.isFixityPrefix fixity) then
        (Errormsg.error pos "conflict in operator precedences.";
        raise TermException)
      else
        (Errormsg.error pos 
          ("missing left argument for " ^
          (Absyn.string_of_fixity fixity) ^
          " operator");
        raise TermException)
    in
    
    (******************************************************************
    *preOrInfWithArg:
    * Handles case where stack state is PrefixWithArgState or 
    * InfixWithArgState.
    ******************************************************************)
    let preOrInfWithArg = fun stack ->
      let pre = fun () ->
        let stack' = stackOperation (makeApplyOp pos) list amodule stack in
        (pushOperation o stack')
      in
      
      let infOrPost = fun () ->
        if prec > (getStackPrec stack) then
          pushOperation o stack
        else if prec < (getStackPrec stack) then
          let stack' = (reduceOperation list amodule stack) in
          stackOperation' stack'
       else
          (Errormsg.error pos "conflict in operator precedences(3)";
          raise TermException)
      in
      
      let inflOrPostl = fun () ->
        if prec > (getStackPrec stack) then
          pushOperation o stack
        else
          let stack' = reduceOperation list amodule stack in
          stackOperation' stack'
      in
      
      match fixity with
        Absyn.Prefix -> pre ()
      | Absyn.Prefixr -> pre ()
      | Absyn.Infix -> infOrPost ()
      | Absyn.Infixr -> infOrPost ()
      | Absyn.Postfix -> infOrPost ()
      | Absyn.Infixl -> inflOrPostl ()
      | Absyn.Postfixl -> inflOrPostl ()
      | Absyn.NoFixity -> stackOperation' stack
    in
    
    (******************************************************************
    *preOrInfrWithArg:
    * Handles case where state is PrefixrWithArgState or
    * InfixrWithArgState.
    ******************************************************************)
    let preOrInfrWithArg = fun stack ->
      let pre = fun () ->
        let stack' = stackOperation (makeApplyOp pos) list amodule stack in
        (pushOperation o stack')
      in
      
      let infOrPost = fun () ->
        if prec >= (getStackPrec stack) then
          (pushOperation o stack)
        else
          let stack' = reduceOperation list amodule stack in
          stackOperation' stack'
      in
      
      let inflOrPostl = fun () ->
        if prec > (getStackPrec stack) then
          pushOperation o stack
        else if prec < (getStackPrec stack) then
          let stack' = reduceOperation list amodule stack in
          stackOperation' stack'
        else
          let _ = printStack stack in
          (Errormsg.error pos "conflict in operator precedences(4)";
          raise TermException)
      in
      
      match fixity with
        Absyn.Prefix -> pre ()
      | Absyn.Prefixr -> pre ()
      | Absyn.Infix -> infOrPost ()
      | Absyn.Infixr -> infOrPost ()
      | Absyn.Postfix -> infOrPost ()
      | Absyn.Infixl -> inflOrPostl ()
      | Absyn.Postfixl -> inflOrPostl ()
      | Absyn.NoFixity -> stackOperation' stack
    in
    
    (******************************************************************
    *post:
    * Handles the case where state is PostfixState.
    ******************************************************************)
    let post = fun stack ->
      let pre = fun () ->
        let stack' = stackOperation (makeApplyOp pos) list amodule stack in
        pushOperation o stack'
      in
      
      let infOrPost = fun () ->
        if prec < (getStackPrec stack) then
          let stack' = reduceOperation list amodule stack in
          stackOperation' stack'
        else
          (Errormsg.error Errormsg.none "conflict in operator precedences";
          raise TermException)
      in
      
      let inflOrPostl = fun () ->
        if prec <= (getStackPrec stack) then
          let stack' = reduceOperation list amodule stack in
          stackOperation' stack'
        else
          (Errormsg.error Errormsg.none "conflict in operator precedences";
          raise TermException)
      in

      match fixity with
        Absyn.Prefix -> pre ()
      | Absyn.Prefixr -> pre ()
      | Absyn.Infix -> infOrPost ()
      | Absyn.Infixr -> infOrPost ()
      | Absyn.Postfix -> infOrPost ()
      | Absyn.Infixl -> inflOrPostl ()
      | Absyn.Postfixl -> inflOrPostl ()
      | Absyn.NoFixity -> stackOperation' stack
    in
    
    (******************************************************************
    *termState:
    * Handles the case where state is TermState.
    ******************************************************************)
    let termState = fun stack ->
      let pre = fun () ->
        let stack' = stackOperation (makeApplyOp pos) list amodule stack in
        pushOperation o stack'
      in
      
      match fixity with
        Absyn.Prefix -> pre ()
      | Absyn.Prefixr -> pre ()
      | _ -> pushOperation o stack
    in
    
    match state with
      NoneState ->
        if (Absyn.isFixityPrefix fixity) then
          (Errormsg.error pos
            ("missing left argument for " ^
            (Absyn.string_of_fixity fixity) ^
            " operator");
          raise TermException)
        else
          (pushOperation o stack)
    | PrefixState -> preOrInf stack
    | InfixState -> preOrInf stack
    
    | PrefixrState -> preOrInfr stack
    | InfixrState -> preOrInfr stack
    
    | PrefixWithArgState -> preOrInfWithArg stack
    | InfixWithArgState -> preOrInfWithArg stack
    
    | PrefixrWithArgState -> preOrInfrWithArg stack
    | InfixrWithArgState -> preOrInfrWithArg stack
    
    | PostfixState -> post stack
    
    | TermState -> termState stack
    
    | _ -> Errormsg.impossible Errormsg.none ("Parse.stackOperation: invalid stack state " ^ (string_of_parserstate state))
  in
  (stackOperation' stack)
(**********************************************************************
*pushTerm:
* Pushes a term onto the stack.  Returns the new stack.
**********************************************************************)
and pushTerm = fun term stack ->
  let Stack(ss,state,f,p) = stack in
  let stackdata = (StackTerm(term))::ss in
  match (getStackState stack) with
    PrefixState -> Stack(stackdata,PrefixrWithArgState,f,p)
  | PrefixrState -> Stack(stackdata, PrefixrWithArgState,f,p)
  | InfixState -> Stack(stackdata,InfixWithArgState,f,p)
  | InfixrState -> Stack(stackdata,InfixrWithArgState,f,p)
  | NoneState -> Stack(stackdata,TermState,f,p)
  | _ -> Errormsg.impossible Errormsg.none ("Parse.pushTerm: invalid stack state " ^ (string_of_parserstate state))

and pushOperation = fun o stack ->
  let stackdata = o :: (getStackStack stack) in
  let prec = getStackOpPrec o in
  let fix = (getStackOpFixity o) in
  let pos = getStackItemPos o in
  match fix with
    Absyn.Prefix -> Stack(stackdata, PrefixState, prec, fix)
  | Absyn.Prefixr -> Stack(stackdata, PrefixrState, prec, fix)
  | Absyn.Infix -> Stack(stackdata, InfixState, prec, fix)
  | Absyn.Infixl -> Stack(stackdata, InfixState, prec, fix)
  | Absyn.Infixr -> Stack(stackdata, InfixrState, prec, fix)
  | Absyn.Postfix -> Stack(stackdata, PostfixState, prec, fix)
  | Absyn.Postfixl -> Stack(stackdata, PostfixState, prec, fix)
  | Absyn.NoFixity -> (Errormsg.impossible pos "Parse.pushOperation: invalid fixity.")

(**********************************************************************
*newParseState:
*Producing a new state after a reduction on the parse stack. Note that
*in such a case the top of stack item must be a term and, if the stack has
*at least two items, the one below this must be a prefix or infix operator.
**********************************************************************)
and newParseState = fun stack ->
  let Stack(list,state, prec, fix) = stack in
  match list with
    (_::ot::_) ->
      let prec' = getStackOpPrec ot in
      let fix' = getStackOpFixity ot in
      (match fix' with
        Absyn.Prefix -> Stack(list, PrefixWithArgState, prec', fix')
      | Absyn.Prefixr -> Stack(list, PrefixrWithArgState, prec', fix')
      | Absyn.Infix -> Stack(list, InfixWithArgState, prec', fix')
      | Absyn.Infixl -> Stack(list, InfixWithArgState, prec', fix')
      | Absyn.Infixr -> Stack(list, InfixrWithArgState, prec', fix')
      | _ -> Errormsg.impossible Errormsg.none "Parse.newParseState: invalid fixity")
  | _ ->
    Stack(list, TermState, prec, fix)

(**********************************************************************
*makeError:
* Builds a term and type representation of an error.
**********************************************************************)
and makeError = fun fvs ->
  (TermAndVariables(errorTerm, fvs))

(**********************************************************************
*makeApplyOp:
* Create a generic application operator that should go between terms
* when two terms are encountered directly next to each other.
**********************************************************************)
and makeApplyOp = fun pos ->
  StackOp(Pervasive.genericApplyConstant, [], pos)
  
(**********************************************************************
*makeApply:
* Makes an application term, performing type checking.
**********************************************************************)
and makeApply = fun f arg ->
  let term = Absyn.ApplyTerm(getTermTerm f, getTermTerm arg, Absyn.getTermPos (getTermTerm f)) in
  let ty = Types.checkApply (getTermType f) (getTermType arg) term in
  
  if ty = Types.errorMolecule then
    errorTerm
  else
    Term(term, ty)

(**********************************************************************
*makeBinaryApply:
* Make an application term, performing type checking.  Special case
* of makeApply, used to display better error information.
**********************************************************************)
and makeBinaryApply = fun f (arg1 : ptterm) (arg2 : ptterm) ->
  let term = Absyn.ApplyTerm(
    Absyn.ApplyTerm(getTermTerm f, getTermTerm arg1, Absyn.getTermPos (getTermTerm f)),
    getTermTerm arg2, Absyn.getTermPos (getTermTerm f)) in
  
  let ty = Types.checkApply (getTermType f) (getTermType arg1) term in
  let ty' = Types.checkApply ty (getTermType arg2) term in
  
  if ty' = Types.errorMolecule then
    errorTerm
  else
    Term(term, ty')

(**********************************************************************
*Beta normalization:
**********************************************************************)
and getEntryTerm = function
    TermEntry(t) -> t
  | _ -> Errormsg.impossible Errormsg.none "Parse.getEntryTerm: invalid entry"

and getEntrySuspensionTerm = function
    SuspensionEntry(t,_) -> t
  | _ -> Errormsg.impossible Errormsg.none "Parse.getEntrySuspensionTerm: invalid entry"
  
and getEntrySuspensionEnv = function
    SuspensionEntry(_,env) -> env
  | _ -> Errormsg.impossible Errormsg.none "Parse.getEntrySuspensionEnv: invalid entry"

and isEntryTerm = function
    TermEntry(_) -> true
  | _ -> false

and isEntrySuspension = function
    SuspensionEntry(_) -> true
  | _ -> false

and getCellEntry = function
  EnvironmentCell(_, entry) -> entry

(**********************************************************************
*normalizeTerm:
* This is the main routine for beta-normalizing an abstract syntax
* term.
**********************************************************************)
and normalizeTerm = fun term ->
  let makeSuspension = fun tsym term env tail ->
    EnvironmentCell(tsym, ref (SuspensionEntry(term, env))) :: tail
  in
  
  let makeTerm = fun tsym term tail ->
    EnvironmentCell(tsym, ref (TermEntry(term))) :: tail
  in
  
  let getEnvironmentSize = List.length in
  
  let emptyEnvironment = [] in
  
  (*  Find an entry in an environment, if it exists.  *)
  let findEntry = fun env tsym ->
    let rec find' = fun env ->
      match env with
        EnvironmentCell(sym,entry)::es ->
          if sym = tsym then
            Some(entry)
          else
            find' es
      | [] -> None
    in
    (find' env)
  in
  
  let rec normalize = fun term env whnf ->
    match term with
      Absyn.IntTerm(_) -> (TermEntry(term))
    | Absyn.RealTerm(_) -> (TermEntry(term))
    | Absyn.StringTerm(_) -> (TermEntry(term))
    | Absyn.FreeVarTerm(_) -> (TermEntry(term))
    | Absyn.ConstantTerm(_) -> (TermEntry(term))
    
    (*  Bound Variables: First check to see if the variable has been
        encountered.  If so, check for an environment entry, and use
        the corresponding term or suspension.  Otherwise, simply use
        the term. *)
    | Absyn.BoundVarTerm(tsym, _) ->
        (match (findEntry env tsym) with
          Some(entry) ->
            if (isEntrySuspension (!entry)) then
              let susp = (!entry) in
              let entry' = normalize (getEntrySuspensionTerm susp) 
                            (getEntrySuspensionEnv susp) whnf in
              (entry := entry';
              entry')
            else
              (!entry)
        | None -> (TermEntry(term)))
    
    (*  Application *)
    | Absyn.ApplyTerm(l, r, p) ->
        let l' = normalize l env true in
        if (isEntrySuspension l') then
          let t' = (getEntrySuspensionTerm l') in
          let sym = (Absyn.getTermAbstractionVar t') in
          let env' = makeSuspension sym r env (getEntrySuspensionEnv l') in
          (normalize t' env' whnf)
        else
          let l'' = (getEntryTerm l') in
          let r' = normalize r env true in
          let r'' = (getEntryTerm r') in
          TermEntry(Absyn.ApplyTerm(l'', r'', p))
    
    | Absyn.AbstractionTerm(tsym, aterm, pos) ->
        if whnf then
          SuspensionEntry(term, env)
        else if (getEnvironmentSize env) > 0 then
          let tsym' = Absyn.BoundTypeSymbol(true,
            None,
            (Absyn.getTypeSymbolSymbol tsym),
            (Absyn.getTypeSymbolType tsym)) in
          let bv = Absyn.BoundVarTerm(tsym, pos) in
          let env' = makeTerm tsym bv env in
          let aterm' = (getEntryTerm (normalize aterm env' false)) in
          if aterm <> aterm' then
            TermEntry(Absyn.AbstractionTerm(tsym', aterm', pos))
          else
            TermEntry(term)
        else
          let aterm' = (getEntryTerm (normalize aterm emptyEnvironment false)) in
          if aterm <> aterm' then
            TermEntry(Absyn.AbstractionTerm(tsym, aterm', pos))
          else
            TermEntry(term)
    | Absyn.ErrorTerm -> TermEntry(Absyn.ErrorTerm)
  in

  let result = normalize term [] false in
  (getEntryTerm result)

and fixTerm = fun term ->
  Absyn.errorFixedTerm