type pos = Errormsg.pos
type symbol = Symbol.symbol

(*  Terms and Terms with Free Variables *)
type ptvarlist = Absyn.atypesymbol Table.SymbolTable.t
type ptterm = Term of (Absyn.aterm * Types.typemolecule)
type ptfixedterm = FixedTerm of (Absyn.afixedterm * Types.typemolecule)
type pttermandvariables = TermAndVariables of (ptterm * ptvarlist)
type ptfixedtermandvariables = FixedTermAndVariables of (ptfixedterm * ptvarlist)

(*  Stack and stack items.  *)
type stackdata =
  StackTerm of (ptterm)
| StackOp of (Absyn.aconstant * Absyn.atype list)

type stack = stackdata list

(*  Term Accessors  *)
let getTermTerm = function Term(t, _) -> t
let getTermMolecule = function Term(_, mol) -> mol
let getFixedTermTerm = function Term(t, _) -> t
let getFixedTermTypeMolecule = function Term(_, mol) -> mol
let getTermAndVariablesTerm = function TermAndVariables(t, _) -> t
let getTermAndVariablesVariables = function TermAndVariables(_, vars) -> vars

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
| NoneState
| TermState

let errorTerm = Term(Absyn.errorTerm, Types.errorMolecule)

(**********************************************************************
*stackTop:
* Returns the top of the given stack.
**********************************************************************)
let stackTop = function
  (s::ss) -> s
| [] -> (Errormsg.impossible Errormsg.none "Parse.stackTop: stack is empty.")

(**********************************************************************
*popStack:
* Removes the top of the given stack and returns the new stack.
**********************************************************************)
let popStack = function
  (s::ss) -> ss
| [] -> (Errormsg.impossible Errormsg.none "Parse.popStack: stack is empty.")

(**********************************************************************
*translateTerm:
* Translate a single term from preabsyn to absyn.
**********************************************************************)
let rec translateTerm = fun term freevars boundvars state stack ->
  match term with
    Preabsyn.SeqTerm(terms, pos) -> (translateTerms terms freevars boundvars reduceToTerm NoneState [])
  | Preabsyn.ListTerm(terms, pos) -> (translateTerms terms freevars (reduceToListTerm (makeConstant "nil")) NoneState [])
  | Preabsyn.ConsTerm(headterms, tailterm, pos) ->
      (*  Translate the tail term first, then translate the head with respect to the new
          set of free variables.  Reduce using the tail term. *)
      let (term', freevars') = (translateTerms tailterm freevars boundvars state stack) in
      (translateTerms headterms freevars' boundvars (reduceToListTerm term') NoneState [])
  | Preabsyn.IntTerm(i, pos) -> (makePervasiveKind Absyn.IntTerm(i, pos) "int")
  | Preabsyn.RealTerm(r, pos) -> (makePervasiveKind Absyn.RealTerm(r, pos) "real")
  | Preabsyn.StringTerm(s, pos) -> (makePervasiveKind Absyn.StringTerm(s, pos) "string")
  | Preabsyn.IdTerm(sym, ty, idkind, pos) ->
      let op' = (translateId term freevars boundvars state stack) in
      (match op' with
        StackOp(_) -> 
          (Errormsg.error pos ("operator used without necessary arguments");
          makeError freevars)
      | StackTerm(t) -> TermAndVariables(t, freevars))
  | Preabsyn.ErrorTerm -> (Errormsg.impossible Errormsg.none "Parse.translateTerm: error term encountered.")

(**********************************************************************
*translateTerms:
* Translate a list of preabsyn terms into abstract syntax.
**********************************************************************)
and translateTerms = fun terms freevars boundvars oper state stack ->

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
      let result = (translateTerm t freevars boundvars state stack) in

      try
        (stackTerm (getTermType result) oper)
      with
        TermException -> (makeError freevars)
    in
    
    (match t with
      Preabsyn.SeqTerm(_) -> simple ()
    | Preabsyn.ListTerm(_) -> simple ()
    | Preabsyn.ConsTerm(_) -> simple ()
    | Preabsyn.IntTerm(_) -> simple ()
    | Preabsyn.RealTerm(_) -> simple ()
    | Preabsyn.StringTerm(_) -> simple ()
    | Preabsyn.IdTerm(_) ->
        let ot = (translateOpTerm t freevars boundvars)
        in try
          (match ot with
            StackOp(o) -> (stackOperation o oper state stack)
          | StackTerm(t) -> (stackTerm t oper state stack))
        with
          TermException -> (makeError freevars))
  in

  (*  Translate each term in turn.  Once the end of the list has been
      reached, execute the given termination operation. *)
  match terms with
    (t::ts) ->
      (translate' t;
      translateTerms ts freevars boundvars oper state stack)
  | [] -> (oper freevars state stack)

(**********************************************************************
*reduceToTerm:
* Reduces the given stack to a term.
**********************************************************************)
and reduceToTerm = fun freevars state stack ->
  (********************************************************************
  *reduce':
  * Reduces the term.
  ********************************************************************)
  let reduce' = fun term freevars ->
    try
      (reduceOperation [])
    with 
      TermException -> ((makeError freevars), ErrorState, [])
  in
  
  (*  Called when the top of the stack indicates an error.  *)
  let err = fun () ->
    let term' = (stackTop stack) in
    let pos = Absyn.getTermPos term' in
    Errormsg.error 
    ((makeError freevars), ErrorState, [])
  in
  
  match state with
    PrefixState -> (err ())
  | PrefixrState -> (err ())
  | InfixState -> (err ())
  | InfixrState -> (err ())
  | TermState ->
      let term' = (stackTop stack) in
      (TermAndVariables(term', freevars), state, stack)
  | _ -> (red term freevars state stack)
  

(**********************************************************************
*reduceToListTerm:
**********************************************************************)
and reduceToListTerm = fun term freevars state stack ->
  let reduce' = fun term freevars ->
    try
      (reduceOperation [])
    with
      TermException -> (makeError freevars, ErrorState, [])
  in
  
  (*  Called when the top of the stack indicates an error.  *)
  let err () =
    let StackTerm(Term(term', _)) = (top stack) in
    let pos = Absyn.getTermPos term' in
    let fix = (Absyn.afixity_of_string (Absyn.getTermFixity term')) in
    (Errormsg.error pos ("missing right argument for " ^ fix ^ " operator");
    (makeError freevars ErrorState []))
  in
  
  match state with
    PrefixState -> (err ())
  | PrefixrState -> (err ())
  | InfixState -> (err ())
  | InfixrState -> (err ())
  | TermState ->
      let term' = (top stack) in
      (TermAndVariables(), state, stack)
  | _ -> (red term freevars state stack)
(**********************************************************************
*makeError:
* Builds a term and type representation of an error.
**********************************************************************)
let makeError freevars =
  TermAndVariables(errorTerm, freevars)

(**********************************************************************
*makeApply:
* Make an application term, performing type checking.
**********************************************************************)
let makeApply = fun f arg ->
  let term = Absyn.ApplyTerm(getTerm f, [getTerm arg], 1, Absyn.getTermPos f) in
  let ty = Types.checkApply (getType f) (getType arg) in
  
  if ty = Types.errorTypeMolecule then
    errorTerm
  else
    Term(term, ty, (getTermPos f))

(**********************************************************************
*makeBinaryApply:
* Make an application term, performing type checking.  Special case
* of makeApply, used to display better error information.
**********************************************************************)
let makeBinaryApply = fun f arg1 arg2 ->
  let term = Absyn.ApplyTerm(Absyn.ApplyTerm(getTerm f, [getTerm arg1], 1, Absyn.getTermPos f), [getTerm arg2], 1, Absyn.getTermPos f) in
  let ty = Types.checkApply (getType f) (getType arg1) in
  let ty' = Types.checkApply ty (getType arg2) in
  
  if ty' = Types.errorTypeMolecule then
    errorTerm
  else
    Term(term, ty', (getTermPos f))

(**********************************************************************
*makePervasiveKind:
* Build a term and type representation of a term with a pervasive type.
**********************************************************************)
let makePervasiveKind = fun term skelname ->
  let pos = Absyn.getTermPos term in
  let skel = (Pervasive.getPervasiveKindType skelname) in
  Term(term, Types.TypeMolecule(skel, []), pos)
