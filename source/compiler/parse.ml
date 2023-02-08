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
type pos = Errormsg.pos
type symbol = Symbol.symbol

(*  Terms and Terms with Free Variables *)
type ptvarlist = Absyn.atypesymbol list
type ptterm = Term of (Absyn.aterm * Types.typemolecule)

(* Functions indicating what to do when a new constant 
* or kind is encountered. *)
type ptnewconstant = symbol -> Absyn.aconstant Table.SymbolTable.t 
                            -> Absyn.aconstant Table.SymbolTable.t
type ptnewkind = symbol -> int -> pos -> Absyn.akind Table.SymbolTable.t 
                                      -> Absyn.akind Table.SymbolTable.t

(*  Term Accessors  *)
let getTermTerm = function Term(t, _) -> t
let getTermMolecule = function Term(_, mol) -> mol

let errorTerm = Term(Absyn.errorTerm, Types.errorMolecule)

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
  | StackError -> (Errormsg.impossible Errormsg.none 
                                       "Parse.getStackItemPos: stack error.")

let getStackTermTerm = function
    StackTerm(t) -> t
  | s -> Errormsg.impossible (getStackItemPos s) 
                             "getStackTermTerm: invalid stack term."

let getStackOpConstant = function
    StackOp(c,_,_) -> c
  | s -> Errormsg.impossible (getStackItemPos s) 
                             "getStackOpConstant: invalid stack op."

let getStackOpEnv = function
    StackOp(_,e,_) -> e
  | s -> Errormsg.impossible (getStackItemPos s) 
                             "getStackOpEnv: invalid stack op."

let getStackOpFixity = function
    StackOp(c,_,_) -> (Absyn.getConstantFixity c)
  | _ -> Errormsg.impossible Errormsg.none 
                             "getStackOpFixity: invalid stack op."

let getStackOpPrec = function
    StackOp(c,_,_) -> (Absyn.getConstantPrec c)
  | _ -> Errormsg.impossible Errormsg.none "getStackOpPrec: invalid stack op."

(*  Stack Accessors *)
let getStackStack = function Stack(data,_,_,_) -> data
let getStackState = function Stack(_,state,_,_) -> state
let getStackPrec = function Stack(_,_,prec,_) -> prec
let getStackFixity = function Stack(_,_,_,fix) -> fix

let printStack stack =
  ()

let contains env v =
  let find tsym =
    (Symbol.equal (Absyn.getTypeSymbolSymbol tsym) v)
  in
  (List.exists find env)

let findVar env v =
  let find tsym =
    (Symbol.equal (Absyn.getTypeSymbolSymbol tsym) v)  in
  try
    Some (List.find find env)
  with Not_found -> None

let add env sym tsym =
  if (contains env sym) then
    env
  else
    tsym::env

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

let rec environmententry_to_string entry = match entry with
  | TermEntry(term) -> Absyn.string_of_term term
  | SuspensionEntry(term, cell_list) -> 
    "Susp(" ^ (Absyn.string_of_term term) ^ ", [" ^
      (String.concat ", " (List.map environmentcell_to_string cell_list)) ^ "])"
and environmentcell_to_string cell = match cell with 
  | EnvironmentCell(typ_sym, entry_ref) ->
      (Absyn.getTypeSymbolName typ_sym) ^ " -> " ^ 
        (environmententry_to_string !entry_ref) 


(**********************************************************************
*Error functions.
**********************************************************************)
let idTypeError tmol result pos =
  let info =
    Errormsg.info ("Type previously determined for variable: " 
                           ^ (Types.string_of_typemolecule tmol))
  in
  match result with
    Types.Success -> 
      Errormsg.impossible Errormsg.none 
        "Parse.idTypeError: unexpected unification result"
  | Types.OccursCheckFailure -> 
      Errormsg.error pos ("circularity discovered during type matching" ^ info)
  | Types.ClashFailure -> 
      Errormsg.error 
        pos ("incompatibility discovered during type matching" ^ info)

let constantTypeError tmol result pos =
  let info =
    Errormsg.info ("Type previously defined or determined for constant: " 
                   ^ (Types.string_of_typemolecule tmol))
  in
  
  match result with
    Types.Success -> Errormsg.impossible Errormsg.none 
                       "Parse.constantTypeError: unexpected unification result"
  | Types.OccursCheckFailure -> 
                       Errormsg.error pos 
                         ("circularity discovered during type matching" ^ info)
  | Types.ClashFailure -> 
                    Errormsg.error pos 
                      ("incompatibility discovered during type matching" ^ info)

(**********************************************************************
*makeType:
* Build a term and type representation of a term with a given (named
* pervasive) type.
**********************************************************************)
let makeType term s ktable pos =
  match (Table.find (Symbol.symbol s) ktable) with
    Some(k) ->
      let ty = Types.makeKindMolecule k in
        Term(term, ty)
  | None -> (Errormsg.impossible (Absyn.getTermPos term) 
               ("Parse.makeType: invalid pervasive kind " ^ s))

let makeConstantTerm parsingtoplevel c pos =
  let ty = Types.makeConstantMolecule parsingtoplevel c in
  let env = Types.getMoleculeEnvironment ty in
  Term(Absyn.ConstantTerm(c,env,pos), ty)


let listSeparatorIdTerm = 
  Preabsyn.IdTerm(Symbol.symbol ",", None, Preabsyn.ConstID, Errormsg.none)

let nilIdTerm =
  Preabsyn.IdTerm(Symbol.symbol "nil", None, Preabsyn.ConstID, Errormsg.none)

(**********************************************************************
*stackTop:
* Returns the top of the given stack.
**********************************************************************)
let stackTop = function
  Stack(s::_,_,_,_) -> s
| Stack([],_,_,_) -> (Errormsg.impossible Errormsg.none 
                                          "Parse.stackTop: stack is empty.")


let rec translateClause ?(parsingtoplevel=false) term amodule =
  let previous = !Errormsg.anyErrors in
  let _ = Errormsg.anyErrors := false in
  let (tty, _, _) = parseTerm parsingtoplevel false term [] [] Table.empty amodule in
  let term' = getTermTerm tty in
  let type' = getTermMolecule tty in
  (*  Remove all overloaded operators.  *)
  let _ = Errormsg.log Errormsg.none 
            "Parse.translateClause: Removing overloads..." in
  let term'' = removeOverloads term' in
  let _ = Errormsg.log Errormsg.none
    ("Parse.translateClause: Removed overloads: " ^ 
     (Absyn.string_of_term_ast term'')) in
  (*  Ensure that the term is valid and is of the correct type. *)                        
  let _ = if term'' <> Absyn.errorTerm then
    (*  Make sure it is of type o.  *)
    let boolkind = Pervasive.kbool in
    let booltype = Types.makeKindMolecule boolkind in
    if (Types.unify type' booltype) <> (Types.Success) then
      (Errormsg.error (Absyn.getTermPos term') ("expecting term of type: o" ^ 
        (Errormsg.info ("encountered term: " ^ (Absyn.string_of_term term''))) ^
        (Errormsg.info ("of type: " ^ (Types.string_of_typemolecule type')))))
    else
      ()
  in
  let term''' = (normalizeTerm term'') in
  let result =
    if !Errormsg.anyErrors then
      None
    else
      Some term'''
  in
  (Errormsg.anyErrors := previous || (!Errormsg.anyErrors);
  result)

  
(**********************************************************************
*translateTermTopLevel:
* Given an abstract syntax representation of a module and a preabsyn
* term, generates a normalized absyn term.
**********************************************************************)
and translateTermTopLevel term amodule =
  let previous = !Errormsg.anyErrors in
  let () = Errormsg.anyErrors := false in
  
  let () = Errormsg.log (Preabsyn.getTermPos term) 
                       ("unparsed preabsyn: " ^ 
                                 (Preabsyn.string_of_term term)) in
  let (tty, _, _) = parseTerm true false term [] [] Table.empty amodule in
  let term' = getTermTerm tty in
  let mol' = getTermMolecule tty in
  let () = Errormsg.log 
            (Absyn.getTermPos term') 
            ("parsed term: " ^ (Absyn.string_of_term term') 
                             ^ " : " 
                             ^ (Types.string_of_typemolecule mol')) in
  let term'' = removeOverloads term' in

  let term''' = removeNestedAbstractions (normalizeTerm term'') in
  let _ = Errormsg.log (Absyn.getTermPos term''') 
    ("parsed, normalized term: " ^ (Absyn.string_of_term_ast term''')) in

  let (fixedterm, fvars, ftyvars) = fixTerm term''' in
  let _ = Errormsg.log (Absyn.getTermPos term''')
      ("parsed, normalized, fixed term: " ^ 
       (Absyn.string_of_term_ast fixedterm))
  in 

  let mol' = Types.skeletonizeMolecule mol' in
  let Types.Molecule(ty, tys) = mol' in
  let mol'' = Types.Molecule(ty, List.map Types.replaceTypeSetType tys) in

  let newftyvars = Types.getNewVarsInTypeMol mol'' ftyvars in
  let result =
    if !Errormsg.anyErrors then
      (None)
    else
      (Some (fixedterm, mol'', fvars, newftyvars))
  in
  (Errormsg.anyErrors := previous || (!Errormsg.anyErrors);
  result)

(**********************************************************************
*parseTerm:
* Translate a preabstract syntax structure that is known to correspond to
* an abstract syntax term in its entirety. The parameters are as follows:
*
*   parsingtoplevel : boolean indicating whether this term was read at the
*     top level.
*   inlist : indicator of how to read commas, i.e. a flag signalling 
*            whether or not in list ctxt
*   term : the preabstract syntax term to be transformed
*   fvs : the set of free variables in the term seen so far (a list)
*   bvs : a list of variables with binders in enclosing context
*   bdgs : a table containing the bindings for the annotated types
*   amodule : a (partial) module structure in abstract syntax form that 
*             provides the kind and type signatures for parsing
*
* The returned value is a pair consisting of a term-type pair and a list
* of free variables appearing in the term.
**********************************************************************)
and parseTerm parsingtoplevel inlist term fvs bvs bdgs amodule =
  match term with
    Preabsyn.SeqTerm(terms, pos) ->
      (*  Corresponds to a parenthetized expression. Parsing into actual
          abstract syntax should proceed assuming a non-list context. *)
      (parseTerms parsingtoplevel false terms fvs bvs bdgs amodule newStack)

  | Preabsyn.ListTerm(terms, pos) ->
      (*  Corresponds to an expression of the form [e1,...,en]. 
       *  Parsing context becomes that of a list. 
       *  A cons and nil to be inserted at end *)
      let terms' = terms @ [listSeparatorIdTerm; nilIdTerm] in
        parseTerms parsingtoplevel true terms' fvs bvs bdgs amodule newStack 

  | Preabsyn.ConsTerm(headterms, tailterm, pos) ->
      (*  Corresponds to [e1,...,en | e0]. A comma at the top level in e0 
          should be treated as an and instead of a list sep, but this will
          happen because in that case e0 will be a SeqTerm that switches 
          context *)
      let terms' = headterms @ [listSeparatorIdTerm; tailterm] in
        parseTerms parsingtoplevel true terms' fvs bvs bdgs amodule newStack

  | Preabsyn.LambdaTerm(b, t, pos) ->
      (* Corresponds to x\ t. Mode should switch to non-list in parsing t *)
      let bbv, bdgs = parseAbstractedSymbol b bdgs amodule in
      let (tty, fvs', bdgs') = 
        parseTerms 
          parsingtoplevel false t fvs (bbv :: bvs) bdgs amodule newStack in
        (makeAbstraction tty bbv pos, fvs', bdgs')
  
  | Preabsyn.IntTerm(i, pos) -> 
           (makeType (Absyn.IntTerm(i,  pos)) 
                     "int" (Absyn.getModuleKindTable amodule) pos, 
            fvs, bdgs)
  | Preabsyn.RealTerm(r, pos) -> 
           (makeType (Absyn.RealTerm(r, pos)) 
                      "real" (Absyn.getModuleKindTable amodule) pos,
            fvs, bdgs)
  | Preabsyn.StringTerm(s, pos) -> 
           (makeType (Absyn.StringTerm(Absyn.StringLiteral(s), pos))
                     "string" (Absyn.getModuleKindTable amodule) pos,
            fvs, bdgs)

  | Preabsyn.IdTerm(_, _, _, pos) ->
      let (op', fvs', bdgs') = 
        translateId parsingtoplevel inlist term fvs bvs bdgs amodule
      in
      (match op' with
        StackOp(_) -> 
          (Errormsg.error pos ("operator used without necessary arguments");
          (errorTerm, fvs', bdgs'))
      | StackTerm(t) -> (t, fvs', bdgs')
      | StackError -> (errorTerm, [], bdgs'))

  | Preabsyn.ErrorTerm -> (errorTerm, [], bdgs)

(**********************************************************************
*parseTerms:
* Translates a list of preabsyn terms into abstract syntax.  It does so
* by parsing each individual term in the list using parseTerm.
*
* Arguments:
*   parsingtoplevel: a boolean flag indicating whether the function is
*     parsing a term at the toplevel (in "interactive" mode) or a
*     clause in a file (see translateTermTopLevel, translateClause).
*   inlist: a boolean flag indicating whether the function is parsing
*     within a list.  This is used to decided whether to parse a comma
*     as a list separator or a conjunction operator.
*   terms: the list of terms in preabstract syntax to parse.
*   fvs: the set of free variables in the term so far.
*   bvs: the set of variables with binders in the enclosing context.
*   bdgs : a table containing the bindings for the annotated types.
*   amodule: the abstract syntax module against whose definitions this
*     term list is being parsed.
*   stack: the current parse stack.
* Returns:
*   The updated stack.
*   The given free variable set updated with any new free variables.
**********************************************************************)
and parseTerms parsingtoplevel inlist terms fvs bvs bdgs amodule stack =
  (********************************************************************
  *translate':
  * Translate an individual term in the list.  The general case is one
  * in which the term is parsed using parseTerm, the returned term is
  * stacked as a term (not an operation), and the returned free variables
  * are themselves returned.
  ********************************************************************)
  let translate' t =    
    (match t with
      Preabsyn.SeqTerm(_)
    | Preabsyn.ListTerm(_)
    | Preabsyn.ConsTerm(_)
    | Preabsyn.IntTerm(_)
    | Preabsyn.RealTerm(_)
    | Preabsyn.StringTerm(_)
    | Preabsyn.LambdaTerm(_) ->
        let (term', fvs', bdgs') =
          (parseTerm parsingtoplevel inlist t fvs bvs bdgs amodule) in
        (try
          let st = stackTerm parsingtoplevel term' amodule stack in
          (st, fvs', bdgs')
        with
          TermException -> (errorStack, fvs, bdgs))
    | Preabsyn.IdTerm(_) ->
        let (ot, fvs', bdgs') =
          (translateId parsingtoplevel inlist t fvs bvs bdgs amodule) 
        in
        (try
          (match ot with
            StackOp(_) ->
              ((stackOperation parsingtoplevel ot amodule stack), fvs', bdgs')
          | StackTerm(t) ->
              ((stackTerm parsingtoplevel t amodule stack), fvs', bdgs')
          | StackError -> (errorStack, fvs', bdgs'))
        with
          TermException -> (errorStack, fvs', bdgs'))
    | Preabsyn.ErrorTerm -> (errorStack, fvs, bdgs))
  in

  (*  Translate each term in turn.  Once the end of the list has been
      reached, execute the given termination operation. *)
  match terms with
    (t::ts) ->
      let (stack', fvs', bdgs') = translate' t in
      (parseTerms parsingtoplevel inlist ts fvs' bvs bdgs' amodule stack')
  | [] -> (reduceToTerm parsingtoplevel fvs bdgs amodule stack)

(**********************************************************************
*parseTypeSymbol:
* Parses a symbol appearing in a lambda expression (the x of x\ t).
**********************************************************************)
and parseAbstractedSymbol tsym bdgs amodule =
  match tsym with
    | Preabsyn.AbstractedSymbol(sym, Some(t), _) -> 
        let Translate.TypeAndBindings(t', bdgs') = 
          Translate.translateTypeAnnot t bdgs amodule in
          (Absyn.BoundVar(sym, ref None, ref true, ref(Some t')), bdgs')
    | Preabsyn.AbstractedSymbol(sym, None, _) -> 
        (Absyn.BoundVar(sym, ref None, ref true,
                       ref(Some (Absyn.makeTypeVariable ()))), bdgs)


(**********************************************************************
*translateId:
* Converts a preabsyn id to an absyn representation.  How it does so
* depends on what sort of id it is:
*   Anonymous variables ("_"): a fresh variable is created.
*   Named variables ("_foo", "_bar"): the variable name is looked up in
*     the list of free variables.  If it exists the already existing
*     variable is returned, otherwise a new variable is generated and
*     added to the list of free varaibles.
*   Ids: the name is looked up first in the bound variable list.  If it
*     exists the associated variable is returned.  If it doesn't, the
*     name is looked up the absyn module's constant table.  If it exists
*     a constant term is created, with special care being taken to ensure
*     that a list separator is returned when a comma is encountered while
*     parsing within a list term ("[foo, bar, baz]").  If it doesn't
*     exist and the name begins with a capital letter it is treated
*     exactly like a named variable (see above); if the name doesn't begin
*     with a capital it is treated as an unknown constant.
**********************************************************************)
and translateId parsingtoplevel inlist term fvs bvs bdgs amodule =
  let translateFreeVar sym =  
    let var = findVar fvs sym in
       match var with
           Some var' -> 
             varToOpTerm 
               term var' fvs bvs bdgs amodule (Absyn.makeFreeVarTerm)
         | None ->
             makeVarToOpTerm term fvs bdgs amodule makeImplicitTypeSymbol
  in
  (* Check that in a given type, there are neither bound type variables 
  * nor type variables used as well as term variables *)
  let checkTypeVariables optTyp =
    let rec aux typ = match typ with
      | Preabsyn.Atom(sym, _, pos) -> 
          (match (findVar bvs sym) with
             | Some _ -> 
                 Errormsg.error pos 
                   ("The type variable '" ^ (Symbol.name sym) ^ 
                    "' cannot be bound")
             | None -> ()) ;
          (match (findVar fvs sym) with
             | Some _ -> 
                 Errormsg.error pos 
                   ("The term variable '" ^ (Symbol.name sym) ^ 
                    "' cannot also be used as a type variable")
             | None -> ()) 
      | Preabsyn.App(t1, t2, _) 
      | Preabsyn.Arrow(t1, t2, _) ->
          aux t1; aux t2
      | Preabsyn.ErrorType -> 
          (* Errors will be handle somewhere else *)
          ()
    in
      match optTyp with 
        | Some(typ) ->
            aux typ
        | None -> ()
  in
  (* Check that the current term variable is not already used as a type 
   * variable *)
  let checkClashTypeTermVariables sym pos = 
    match (Table.find sym bdgs) with
      | Some(_) -> 
          Errormsg.error pos 
            ("The type variable '" ^ (Symbol.name sym) ^ 
             "' cannot also be used as a term variable")
      | None -> ()
  in

  match term with
      Preabsyn.IdTerm(sym, optTyp, k, pos) ->
        checkTypeVariables optTyp; 
        checkClashTypeTermVariables sym pos; 
      (match k with
         Preabsyn.AVID ->       (*  _, i.e. an anonymous variable  *) 
           (makeVarToOpTerm term fvs bdgs amodule makeAnonymousTypeSymbol)

       | Preabsyn.VarID ->      (* _ followed by name, i.e. a free variable *)
           translateFreeVar sym

       | Preabsyn.ConstID ->
          (match (findVar bvs sym) with
               Some bvar' ->     (* here we have a bound variable *)
                 (varToOpTerm term bvar' fvs bvs bdgs amodule 
                    (Absyn.makeBoundVarTerm))

             | None ->      (* Check next if declared constant *)
                 (match 
                   Table.find sym (Absyn.getModuleConstantTable amodule) with
                     Some c ->  
                       (* Is a constant; commas may be list separators *)
                       if c == Pervasive.andConstant && inlist then
                         (constantToOpTerm parsingtoplevel term
                            Pervasiveutils.listSeparatorConstant 
                            fvs bdgs amodule)
                       else 
                         (constantToOpTerm 
                            parsingtoplevel term c fvs bdgs amodule)
                     | None -> 
                         (Errormsg.error pos
                            ("undeclared constant '" ^ (Symbol.name sym) ^ 
                             "'");
                          (StackError, fvs, bdgs))))
         | Preabsyn.CVID -> 
            match (findVar bvs sym) with
               Some bvar' ->     (* here we have a bound variable *)
                 (varToOpTerm term bvar' fvs bvs bdgs amodule 
                    (Absyn.makeBoundVarTerm))
             | None ->          
                 translateFreeVar sym)

  | _ -> (Errormsg.impossible (Preabsyn.getTermPos term) 
                              "Parse.translateId: invalid term")

and constantToOpTerm parsingtoplevel term constant fvs bdgs amodule =
  let make' tmol bdgs pos =
    let fixity = Absyn.getConstantFixity constant in
    if fixity = Absyn.NoFixity then
      (StackTerm(Term(Absyn.ConstantTerm(constant, 
                                         (Types.getMoleculeEnvironment tmol), 
                                         pos), tmol)), 
       fvs, bdgs)
    else
      (StackOp(constant, (Types.getMoleculeEnvironment tmol), pos), 
       fvs, bdgs)
  in
  match term with
    Preabsyn.IdTerm(_, Some(ty), _, pos) ->
      let Translate.TypeAndBindings(ty', bdgs') = 
        Translate.translateTypeAnnot ty bdgs amodule in
      let tm1 = Types.makeConstantMolecule parsingtoplevel constant in
      let tm2 = 
        Types.Molecule(ty', []) in
      let result = (Types.unify tm1 tm2) in
      if result = Types.Success then
        (make' tm1 bdgs pos)
      else
        (constantTypeError tm1 result pos;
        (StackError, fvs, bdgs))
  | Preabsyn.IdTerm(_, None, _, pos) ->
      let tm1 = Types.makeConstantMolecule parsingtoplevel constant in
      (make' tm1 bdgs pos)
  | _ -> Errormsg.impossible (Preabsyn.getTermPos term) 
                             "Parse.constantToOpTerm: invalid term."

and makeImplicitTypeSymbol c sym ty =
  Absyn.ImplicitVar(sym, ref c, ref false, ref (Some ty))
and makeAnonymousTypeSymbol c sym ty =
  Absyn.AnonymousImplicitVar(sym, ref c, ref false, ref (Some ty))

(* The first time that this free variable (with or without type annotation) 
* is met during the translation of the current clause *)
and makeVarToOpTerm term fvs bdgs amodule makeSymFunc =
  match term with
    Preabsyn.IdTerm(sym, Some(pty), _, pos) ->
      (* There is a type annotation. We use it to generate the skeleton *)
      let Translate.TypeAndBindings(aty, bdgs') = 
        Translate.translateTypeAnnot pty bdgs amodule in
      let tmol = Types.Molecule(aty, []) in
      let typesym = makeSymFunc None sym aty in
      let fvs' = (add fvs sym typesym) in
        (StackTerm(Term(Absyn.makeFreeVarTerm typesym pos, tmol)), 
         fvs', bdgs')
  | Preabsyn.IdTerm(sym, None, _, pos) ->
      (* There is no type annotation. This variable will thus be bindable 
       * during unification *)
      let skel = Absyn.makeTypeVariable () in
      let tmol = Types.Molecule(skel, []) in
      let typesym = makeSymFunc None sym skel in
      let fvs' = (add fvs sym typesym) in
        (StackTerm(Term(Absyn.makeFreeVarTerm typesym pos, tmol)), 
         fvs', bdgs)
  | _ -> Errormsg.impossible Errormsg.none 
                             "Parse.makeVarToOpTerm: invalid id term"

(* We already met the symbol of the variable term. 
*   - typesym : the atypesymbol that we previously associated 
*     to the term's symbol  
*   - fvs : the current free variables, just passed to the StackTerm structure
*   - amodule : the current module. Will be useful if there is a type 
*     annotation. In this case we will need the module's kinds and type
*     abbreviations to translate the annotation.
*)
and varToOpTerm term typesym fvs _ bdgs amodule makeVarFunc =
  let tm1 = Types.Molecule(Absyn.getTypeSymbolRawType typesym, []) in
  match term with
    Preabsyn.IdTerm(_, Some(ty), _, pos) ->
      (*  If the term has a type annotation, attempt to unify it with the
          previously associated type  *)  
      let Translate.TypeAndBindings(ty', bindings) = 
        Translate.translateTypeAnnot ty bdgs amodule in
      let tm2 = 
        Types.Molecule(ty', []) in
      let result = (Types.unify tm1 tm2) in
      if result = Types.Success then
        (StackTerm(Term(makeVarFunc typesym pos, tm2)), fvs, bdgs)
      else
        (idTypeError tm1 result pos; (StackError, fvs, bdgs))

  | Preabsyn.IdTerm(_, None, _, pos) ->
     (*  If the term has no type annotation, simply create a 
      *  new type variable and bind it.  *)
       (StackTerm(Term(makeVarFunc typesym pos, tm1)), fvs, bdgs)
  | _ -> Errormsg.impossible (Preabsyn.getTermPos term) 
                             "Parse.varToOpTerm: invalid term"

(**********************************************************************
*reduceToTerm:
* Reduces the given stack to a term.
*   Arguments:
*     parsingtoplevel: boolean indicating that we are parsing a term from
*       the top-level interface and not a source file.
*     fvs: free variables
*     amodule: the associated absyn module
*     stack: the current parse stack.
*   Returns:
*
**********************************************************************)
and reduceToTerm parsingtoplevel fvs bdgs amodule stack =
  (********************************************************************
  *reduce':
  * Reduces the term.
  ********************************************************************)
  let rec reduce' stack =
      match (getStackState stack) with
        TermState ->
          let term = (getStackTermTerm (stackTop stack)) in 
            (term, fvs, bdgs)

      | ErrorState -> (errorTerm, fvs, bdgs)
      | _ ->
          try
            let stack' = reduceOperation parsingtoplevel amodule stack in
            (reduce' stack')
          with 
            TermException -> (errorTerm, fvs, bdgs)
  in
  
  (*  Called when the top of the stack indicates an error.  *)
  match (getStackState stack) with
    PrefixState
  | PrefixrState
  | InfixState
  | InfixrState ->
      let pos = getStackItemPos (stackTop stack) in
      let fixity = 
        (Absyn.string_of_fixity
          (Absyn.getConstantFixity 
          (getStackOpConstant (stackTop stack)))) in

      (Errormsg.error pos ("missing right argument for " ^
        (StringLabels.lowercase_ascii fixity) ^ " operator");
      (errorTerm, fvs, bdgs))
  | _ -> (reduce' stack)

(**********************************************************************
*reduceOperation:
* Reduces an operation at the top of the stack.  There is a special
* case for the list separator: it is changed to a cons operator.
*
* The main complexity in this function is that of dealing with special cases
* for the stack operator. There are two, both pertaining to infix operators.
* 
* First, it could be a "pseudo-application".  In this case, the two terms 
* "around" it are applied to each other and the app disappears.
*
* Then there is the case of a comma or list separator. This needs to be 
* transformed into a cons; the list separator operator was put into the 
* stack instead of the cons during the earlier phase because this has 
* lower precedence.
*
* Arguments:
*   parsingtoplevel:  boolean indicating that we are parsing a term from
*       the top-level interface and not a source file.
*   amodule: the absyn module
*   stack: the current parse stack
* Returns:
**********************************************************************)
and reduceOperation parsingtoplevel amodule stack =
  (********************************************************************
  *reduce':
  ********************************************************************)
  let reduce' () = 
    let state = getStackState stack in
    let prec = getStackPrec stack in
    let fix = getStackFixity stack in
    
    let top = List.hd (getStackStack stack) in
    let prev = List.hd (List.tl (getStackStack stack)) in
    let rest = List.tl (List.tl (getStackStack stack)) in

    let reducePrefix () =
      let c = (makeConstantTerm parsingtoplevel (getStackOpConstant prev) 
                                (getStackItemPos prev)) in
      let newTop = StackTerm(makeApply c (getStackTermTerm top)) in
      let stack' = Stack(newTop::rest, state, prec, fix) in
      newParseState stack'
    in
    
    let reduceInfix () =
      let r = List.hd rest in
      let rest' = List.tl rest in
      
      let c = (makeConstantTerm parsingtoplevel (getStackOpConstant prev) 
                                (getStackItemPos prev)) in
      let newTop = StackTerm(makeBinaryApply c (getStackTermTerm r) 
                                             (getStackTermTerm top)) in
      let stack' = Stack(newTop::rest', state, prec, fix) in
      newParseState stack'
    in
    
    match state with
      PostfixState ->
        let c = (makeConstantTerm parsingtoplevel (getStackOpConstant top) 
                                  (getStackItemPos top)) in
        let newTop = StackTerm(makeApply c (getStackTermTerm prev)) in
        let stack' = Stack(newTop::rest, state, prec, fix) in
        newParseState stack'

    | PrefixWithArgState
    | PrefixrWithArgState -> reducePrefix ()

    | InfixWithArgState
    | InfixrWithArgState -> reduceInfix ()
    | _ -> errorStack
  in
  
  (********************************************************************
  *reduceApply:
  * Reduces an "apply operator" on the stack.  An "apply operator" is
  * the dummy inserted between adjacent terms.  The apply operator is
  * removed from the stack, and the top two terms (sitting around the 
  * dummy apply operator) are used to form an application.
  ********************************************************************)
  let reduceApply stack =
    let top = List.hd (getStackStack stack) in
    let pp = List.hd (List.tl (List.tl (getStackStack stack))) in
    let rest = List.tl (List.tl (List.tl (getStackStack stack))) in

    let apply = makeApply (getStackTermTerm pp) (getStackTermTerm top) in
    let stack' = Stack((StackTerm(apply))::rest, 
                       (getStackState stack), 
                       (getStackPrec stack), (getStackFixity stack)) in
    newParseState stack'
  in
  
  (********************************************************************
  *reduceComma:
  * A comma operator in the stack must translate to a cons 
  ********************************************************************)
  let reduceComma stack =
    let top = List.hd (getStackStack stack) in
    let comma = List.hd (List.tl (getStackStack stack)) in
    let pp = List.hd (List.tl (List.tl (getStackStack stack))) in
    let rest = List.tl (List.tl (List.tl (getStackStack stack))) in

    let pos = (getStackItemPos comma) in
    let cons = makeConstantTerm parsingtoplevel Pervasive.consConstant pos in
    let newTop = makeBinaryApply cons 
                                 (getStackTermTerm pp) (getStackTermTerm top) in
    let stack' = Stack((StackTerm(newTop))::rest, 
      getStackState stack, getStackPrec stack, 
      getStackFixity stack) in
    newParseState stack'
  in
  
  (*  The actual code for reduceOperation *)
  let ot = List.hd (List.tl (getStackStack stack)) in
  match ot with
    StackOp(c,_,_) -> 
      let csym = (Absyn.getConstantSymbol c) in
      if csym = (Absyn.getConstantSymbol Pervasive.genericApplyConstant) then
        (reduceApply stack)
      else if csym == 
               (Absyn.getConstantSymbol Pervasiveutils.listSeparatorConstant) 
           then (reduceComma stack)
           else reduce' ()
  | _ -> (reduce' ())


(**********************************************************************
*stackTerm:
**********************************************************************)
and stackTerm parsingtoplevel term amodule stack =
  (********************************************************************
  *stackApply:
  * Stacks a 'pseudo' apply operator.
  ********************************************************************)
  let stackApply () =
    let apply = (makeApplyOp (Absyn.getTermPos (getTermTerm term))) in
    let stack' = stackOperation parsingtoplevel apply amodule stack in
    (pushTerm term stack')
  in

  (*  Check whether to add a pseudo application operator. *)
  let state = getStackState stack in
  match state with
    PrefixState
  | PrefixrState
  | InfixState
  | InfixrState
  | NoneState -> (pushTerm term stack)
  
  | PrefixWithArgState
  | PrefixrWithArgState
  | InfixWithArgState
  | InfixrWithArgState
  | PostfixState
  | TermState -> stackApply ()
  | ErrorState -> stack

(**********************************************************************
*stackOperation:
* Handles stacking an operator on the parse stack; this is where the
* main decisions of shifting/reducing get made.  Note that an apply
* "pseudo" operator may have to be inserted between two adjacent terms
* in the input to ensure the appropriate reduction at a later stage.
*
* The function operates by recursively calling stackOperation' until there
* are no operations left to stack.
**********************************************************************)
and stackOperation parsingtoplevel o amodule stack =
  (********************************************************************
  *stackOperation':
  ********************************************************************)
  let rec stackOperation' stack =
    let state = (getStackState stack) in
    let name = (Absyn.getConstantName (getStackOpConstant o)) in
    let fixity = (getStackOpFixity o) in
    let prec = (getStackOpPrec o) in
    let pos = (getStackItemPos o) in
    
    (******************************************************************
    *precedenceConflict:
    * Prints an error related to a conflict of precedences.
    ******************************************************************)
    let precedenceConflict () =
      Errormsg.error pos
        ("'" ^ name ^ "': conflict in operator precedences:" ^
        (Errormsg.info "precedence: " ^ (string_of_int prec)) ^ "." ^
        (Errormsg.info "fixity: " ^
        (StringLabels.lowercase_ascii (Absyn.string_of_fixity fixity))) ^ ".")
    in

    (******************************************************************
    *preOrInf:
    * Handles the case where stack state is Prefix or Infix.
    ******************************************************************)
    let preOrInf stack =
      if (Absyn.isFixityPrefix fixity) && (prec > (getStackPrec stack)) then
        (pushOperation o stack)
      else if (Absyn.isFixityPrefix fixity) then
        (precedenceConflict ();
        raise TermException)
      else
        let pos = getStackItemPos (stackTop stack) in
        (Errormsg.error pos 
          ("missing left argument for " ^
          (StringLabels.lowercase_ascii (Absyn.string_of_fixity fixity)) ^
          " operator");
        raise TermException)
    in

    (******************************************************************
    *preOrInfr:
    * Handles the case where state is PrefixrState or InfixrState.
    ******************************************************************)
    let preOrInfr stack =
      if (Absyn.isFixityPrefix fixity) && (prec >= (getStackPrec stack)) then
        pushOperation o stack
      else if (Absyn.isFixityPrefix fixity) then
        (precedenceConflict ();
        raise TermException)
      else
        let pos = getStackItemPos (stackTop stack) in
        (Errormsg.error pos 
          ("missing right argument for " ^
          (StringLabels.lowercase_ascii (Absyn.string_of_fixity fixity)) ^
          " operator");
        raise TermException)
    in
    
    (******************************************************************
    *preOrInfWithArg:
    * Handles case where stack state is PrefixWithArgState or 
    * InfixWithArgState.
    ******************************************************************)
    let preOrInfWithArg stack =
      match fixity with
        Absyn.Prefix
      | Absyn.Prefixr ->
          let stack' =
            stackOperation parsingtoplevel (makeApplyOp pos) amodule stack in
          (pushOperation o stack')

      | Absyn.Infix
      | Absyn.Infixr
      | Absyn.Postfix ->
          if prec > (getStackPrec stack) then
            pushOperation o stack
          else if prec < (getStackPrec stack) then
            let stack' = (reduceOperation parsingtoplevel amodule stack) in
            stackOperation' stack'
          else
            (precedenceConflict ();
            raise TermException)

      | Absyn.Infixl
      | Absyn.Postfixl ->
          if prec > (getStackPrec stack) then
            pushOperation o stack
          else
            let stack' = reduceOperation parsingtoplevel amodule stack in
            stackOperation' stack'
      | Absyn.NoFixity -> stackOperation' stack
    in
    
    (******************************************************************
    *preOrInfrWithArg:
    * Handles case where state is PrefixrWithArgState or
    * InfixrWithArgState.
    ******************************************************************)
    let preOrInfrWithArg stack =
      match fixity with
        Absyn.Prefix
      | Absyn.Prefixr ->
          let stack' =
            stackOperation parsingtoplevel (makeApplyOp pos) amodule stack in
          (pushOperation o stack')

      | Absyn.Infix
      | Absyn.Infixr
      | Absyn.Postfix ->
          if prec >= (getStackPrec stack) then
            (pushOperation o stack)
          else
            let stack' = reduceOperation parsingtoplevel amodule stack in
            stackOperation' stack'

      | Absyn.Infixl
      | Absyn.Postfixl ->
          if prec > (getStackPrec stack) then
            pushOperation o stack
          else if prec < (getStackPrec stack) then
            let stack' = reduceOperation parsingtoplevel amodule stack in
            stackOperation' stack'
          else
            (precedenceConflict ();
            raise TermException)
      | Absyn.NoFixity -> stackOperation' stack
    in
    
    (******************************************************************
    *post:
    * Handles the case where state is PostfixState.
    ******************************************************************)
    let post stack =
      match fixity with
        Absyn.Prefix
      | Absyn.Prefixr ->
          let stack' = stackOperation parsingtoplevel (makeApplyOp pos)
            amodule stack in
          pushOperation o stack'

      | Absyn.Infix
      | Absyn.Infixr
      | Absyn.Postfix ->
          if prec < (getStackPrec stack) then
            let stack' = reduceOperation parsingtoplevel amodule stack in
            stackOperation' stack'
          else
            (precedenceConflict ();
            raise TermException)

      | Absyn.Infixl
      | Absyn.Postfixl ->
          if prec <= (getStackPrec stack) then
            let stack' = reduceOperation parsingtoplevel amodule stack in
            stackOperation' stack'
          else
            (precedenceConflict ();
            raise TermException)
      | Absyn.NoFixity -> stackOperation' stack
    in
    
    (******************************************************************
    *termState:
    * Handles the case where state is TermState.
    ******************************************************************)
    let termState stack =
      match fixity with
        Absyn.Prefix
      | Absyn.Prefixr ->
          let stack' =
            stackOperation parsingtoplevel (makeApplyOp pos) amodule stack in
          pushOperation o stack'
      | _ -> pushOperation o stack
    in
    
    (*  Body of stackOperation' *)
    match state with
      NoneState ->
        if not (Absyn.isFixityPrefix fixity) then
          (Errormsg.error pos
            ("missing left argument for " ^
            (StringLabels.lowercase_ascii (Absyn.string_of_fixity fixity)) ^
            " operator");
          raise TermException)
        else
          (pushOperation o stack)
    | PrefixState
    | InfixState -> preOrInf stack
    | PrefixrState
    | InfixrState -> preOrInfr stack
    | PrefixWithArgState
    | InfixWithArgState -> preOrInfWithArg stack
    | PrefixrWithArgState
    | InfixrWithArgState -> preOrInfrWithArg stack
    | PostfixState -> post stack
    | TermState -> termState stack
    | _ -> errorStack
  in
  (stackOperation' stack)

(**********************************************************************
*pushTerm:
* Pushes a term onto the stack.  Returns the new stack.
**********************************************************************)
and pushTerm term stack =
  let Stack(ss,state,f,p) = stack in
  let stackdata = (StackTerm(term))::ss in
  match (getStackState stack) with
    PrefixState -> Stack(stackdata,PrefixrWithArgState,f,p)
  | PrefixrState -> Stack(stackdata, PrefixrWithArgState,f,p)
  | InfixState -> Stack(stackdata,InfixWithArgState,f,p)
  | InfixrState -> Stack(stackdata,InfixrWithArgState,f,p)
  | NoneState -> Stack(stackdata,TermState,f,p)
  | _ -> errorStack

and pushOperation o stack =
  let stackdata = o :: (getStackStack stack) in
  let prec = getStackOpPrec o in
  let fix = (getStackOpFixity o) in
  match fix with
    Absyn.Prefix -> Stack(stackdata, PrefixState, prec, fix)
  | Absyn.Prefixr -> Stack(stackdata, PrefixrState, prec, fix)
  | Absyn.Infix
  | Absyn.Infixl -> Stack(stackdata, InfixState, prec, fix)
  | Absyn.Infixr -> Stack(stackdata, InfixrState, prec, fix)
  | Absyn.Postfix
  | Absyn.Postfixl -> Stack(stackdata, PostfixState, prec, fix)
  | Absyn.NoFixity ->
      errorStack

(**********************************************************************
*newParseState:
*Producing a new state after a reduction on the parse stack. Note that
*in such a case the top of stack item must be a term and, if the stack has
*at least two items, the one below this must be a prefix or infix operator.
**********************************************************************)
and newParseState stack =
  let Stack(list,state, prec, fix) = stack in
  match list with
    (_::ot::_) ->
      let prec' = getStackOpPrec ot in
      let fix' = getStackOpFixity ot in
      (match fix' with
        Absyn.Prefix -> Stack(list, PrefixWithArgState, prec', fix')
      | Absyn.Prefixr -> Stack(list, PrefixrWithArgState, prec', fix')
      | Absyn.Infix
      | Absyn.Infixl -> Stack(list, InfixWithArgState, prec', fix')
      | Absyn.Infixr -> Stack(list, InfixrWithArgState, prec', fix')
      | _ -> Errormsg.impossible Errormsg.none 
                                 "Parse.newParseState: invalid fixity")
  | _ ->
    Stack(list, TermState, prec, fix)

(**********************************************************************
*makeAbstraction:
* Given the body of an abstraction and its type and the variable abstracted, 
* returns a pair of the abstracted term and its type.
* Abstractions in the returned form are over exactly one variable, i.e. 
* abstraction over a list of variables has to be converted into a sequence of 
* abstractions. Notice also that the convention when a list of variables is 
* used is that the closest abstracted variable appears first, i.e. 
* (lam x lam y t) would have its list of variables as [y,x] and not [x,y]
**********************************************************************)
and makeAbstraction termmol bv pos =
  (********************************************************************
  *makeTerm:
  ********************************************************************)
  let makeTerm currentterm bv =
    Absyn.AbstractionTerm(
      Absyn.NestedAbstraction(bv, currentterm),
      pos) 
  in

  let term = getTermTerm termmol in
  let term' = makeTerm term bv in

  let mol = getTermMolecule termmol in
  let bvtype = Absyn.getTypeSymbolType bv in
  let ty' = Absyn.makeArrowType (Types.getMoleculeType mol) [bvtype] in
  let env' = Types.getMoleculeEnvironment mol in
    Term(term', Types.Molecule(ty', env'))

(**********************************************************************
*makeApplyOp:
* Create a generic application operator that should go between terms
* when two terms are encountered directly next to each other.
**********************************************************************)
and makeApplyOp pos =
  StackOp(Pervasive.genericApplyConstant, [], pos)
  
(**********************************************************************
*makeApply:
* Makes an application term, performing type checking.
**********************************************************************)
and makeApply f arg =
  let term = Absyn.ApplicationTerm(
    Absyn.CurriedApplication(getTermTerm f, getTermTerm arg),
    Absyn.getTermPos (getTermTerm f)) in
  let ty = Types.checkApply (getTermMolecule f) (getTermMolecule arg) term in
    if ty = Types.errorMolecule then
      errorTerm
    else
      Term(term, ty)

(**********************************************************************
*makeBinaryApply:
* Make an application term, performing type checking.  Special case
* of makeApply, used to display better error information.
**********************************************************************)
and makeBinaryApply f (arg1 : ptterm) (arg2 : ptterm) =
  let term = Absyn.ApplicationTerm(
    Absyn.CurriedApplication(
      Absyn.ApplicationTerm(
        Absyn.CurriedApplication(getTermTerm f, getTermTerm arg1),
        Absyn.getTermPos (getTermTerm f)),
      getTermTerm arg2),
    Absyn.getTermPos (getTermTerm f)) in
  
  let ty = Types.checkApply (getTermMolecule f) (getTermMolecule arg1) term in
  let ty' = Types.checkApply ty (getTermMolecule arg2) term in
  
  if ty' = Types.errorMolecule then
    errorTerm
  else
    Term(term, ty')

(**********************************************************************
*removeOverloads:
**********************************************************************)
and removeOverloads term =
  let removeTypeSymbolOverloads ti =
    let replace r =
      if Option.isSome r then
        Some (Types.replaceTypeSetType (Option.get r))
      else
        r
    in
    match ti with
        Absyn.ImplicitVar(s,c,b,tor) ->
          let _ = tor := replace !tor in
          ti
      | Absyn.BoundVar(s,c,b,tor) ->
          let _ = tor := replace !tor in
          ti
      | Absyn.AnonymousImplicitVar(s,c,b,tor) ->
          let _ = tor := replace !tor in
          ti
  in

  match term with
    Absyn.IntTerm(_) -> term
  | Absyn.RealTerm(_) -> term
  | Absyn.StringTerm(_) -> term
  | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tsym), p) ->
      let tsym' = removeTypeSymbolOverloads tsym in
      Absyn.FreeVarTerm(Absyn.NamedFreeVar(tsym'), p)
  | Absyn.FreeVarTerm(_) ->
      (Errormsg.impossible Errormsg.none
        "Parse.removeOverloads: invalid free variable term";)
  | Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym), p) ->
      let tsym' = removeTypeSymbolOverloads tsym in
      Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym'), p)
  | Absyn.BoundVarTerm(_) ->
      (Errormsg.impossible Errormsg.none
        "Parse.removeOverloads: invalid bound variable term";)
  | Absyn.AbstractionTerm(Absyn.NestedAbstraction(t, body), p) ->
      let body' = removeOverloads body in
      Absyn.AbstractionTerm(Absyn.NestedAbstraction(t, body'), p)
  | Absyn.AbstractionTerm(Absyn.UNestedAbstraction(ts, c, body), p) ->
      let body' = removeOverloads body in
      Absyn.AbstractionTerm(Absyn.UNestedAbstraction(ts, c, body'), p)
  | Absyn.ApplicationTerm(Absyn.FirstOrderApplication(f, args, i), p) ->
      let f' = removeOverloads f in
      let args' = List.map removeOverloads args in
      Absyn.ApplicationTerm(Absyn.FirstOrderApplication(f', args', i), p)
  | Absyn.ApplicationTerm(Absyn.CurriedApplication(f, a), p) ->
      let f' = removeOverloads f in
      let a' = removeOverloads a in
      Absyn.ApplicationTerm(Absyn.CurriedApplication(f', a'), p)

  | Absyn.ConstantTerm(c, tl, p) ->
      if Pervasiveutils.isOverloaded c then
        let ty = List.hd tl in
        let ty' = Absyn.dereferenceType ty in
        (match ty' with
          Absyn.TypeSetType(def, l, _) ->
            let k =
              (if (List.length (!l)) = 0 then
                Errormsg.impossible p "Parse.removeOverloads: invalid type set"
              else if (List.length (!l)) = 1 then
                Absyn.getTypeKind (List.hd !l)
              else
                Absyn.getTypeKind def) in
            Absyn.ConstantTerm(Pervasiveutils.getOverload k c, [], p)
        | _ -> Absyn.ErrorTerm)
      else
        Absyn.ConstantTerm(c, List.map Types.replaceTypeSetType tl, p)
  | Absyn.ErrorTerm -> Absyn.ErrorTerm

(**********************************************************************
*removeNestedAbstractions:
* Converts a term in which every abstraction is over a single variable into a
* term in which abstractions are over lists of variables AND there are no 
* abstractions nested immediately within other abstractions. The former kind of
* abstraction has the form:
*   Absyn.AbstractionTerm(Absyn.NestedAbstraction(...),...)
* and the latter has the form:
*   Absyn.AbstractionTerm(Absyn.UNestedAbstraction(...),...).
* Notice that in the latter representation the deepest abstracted variable
* appears earliest in the  list, i.e. (lam x lam y t) will have the list
* of abstracted variables as [y,x] and NOT as [x,y].
**********************************************************************)
and removeNestedAbstractions term =
  let rec remove term =
    match term with
      Absyn.AbstractionTerm(abst, p) ->
        let (tsyms, body) = removeAbstraction abst [] in
        Absyn.AbstractionTerm(
          Absyn.UNestedAbstraction(tsyms,List.length tsyms, body), p)
    | Absyn.ApplicationTerm(Absyn.CurriedApplication(t1, t2), p) ->
        let t1' = removeNestedAbstractions t1 in
        let t2' = removeNestedAbstractions t2 in
        Absyn.ApplicationTerm(Absyn.CurriedApplication(t1', t2'), p) 
    | Absyn.ApplicationTerm(
        Absyn.FirstOrderApplication(head, args, l), p) ->
        let head' = removeNestedAbstractions head in
        let args' = List.map removeNestedAbstractions args in
        Absyn.ApplicationTerm(
          Absyn.FirstOrderApplication(head', args', l), p)
    | _ -> term

  and removeAbstraction abst tsyms =
    match abst with 
      Absyn.NestedAbstraction(tsym, body) ->
        (match body with
          Absyn.AbstractionTerm(abst', p) -> 
            removeAbstraction abst' (tsym :: tsyms)
        | _ -> (List.rev (tsym :: tsyms), remove body))
    (* This instance may occur while processing queries
     * because queries are toplevel terms *)
    (* | Absyn.UNestedAbstraction(tsyms,nabs,body) ->
     *    (tsyms,body) *)
  | Absyn.UNestedAbstraction(_) ->
      (Errormsg.impossible Errormsg.none
        "Parse.removeNestedAbstractions: unexpected unnested abstraction")
  in
  remove term									   


(**********************************************************************
*Beta normalization:
**********************************************************************)
and getEntryTerm = function
    TermEntry(t) -> t
  | _ -> Errormsg.impossible Errormsg.none "Parse.getEntryTerm: invalid entry"

and getEntrySuspensionTerm = function
    SuspensionEntry(t,_) -> t
  | _ -> Errormsg.impossible Errormsg.none 
                             "Parse.getEntrySuspensionTerm: invalid entry"
  
and getEntrySuspensionEnv = function
    SuspensionEntry(_,env) -> env
  | _ -> Errormsg.impossible Errormsg.none 
                             "Parse.getEntrySuspensionEnv: invalid entry"

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
and normalizeTerm term =
  let makeSuspension tsym term env tail =
    EnvironmentCell(tsym, ref (SuspensionEntry(term, env))) :: tail
  in
  
  let makeTerm tsym term tail =
    EnvironmentCell(tsym, ref (TermEntry(term))) :: tail
  in
  
  let getEnvironmentSize = List.length in
  let emptyEnvironment = [] in
  
  (*  Find an entry in an environment, if it exists.  *)
  let findEntry env tsym =
    let rec find' env =
      match env with
        EnvironmentCell(sym,entry)::es ->
          if (sym = tsym) then
            Some(entry)
          else
            find' es
      | [] -> None
    in
    (find' env)
  in
  
  let rec normalize term env whnf =
    match term with
      Absyn.IntTerm(_)
    | Absyn.RealTerm(_)
    | Absyn.StringTerm(_)
    | Absyn.FreeVarTerm(_)
    | Absyn.ConstantTerm(_) -> (TermEntry(term))
    
    (*  Bound Variables: First check to see if the variable has been
        encountered.  If so, check for an environment entry, and use
        the corresponding term or suspension.  Otherwise, simply use
        the term. *)
    | Absyn.BoundVarTerm(Absyn.NamedBoundVar(tsym), pos) ->
        let entryop = (findEntry env tsym) in
        if (Option.isSome entryop) then
          let entry = Option.get entryop in
          if (isEntrySuspension (!entry)) then
            let susp = (!entry) in
            let entry' = normalize (getEntrySuspensionTerm susp) 
                          (getEntrySuspensionEnv susp) whnf in
            (entry := entry';
            entry')
          else
            (!entry)
        else
          (TermEntry(term))
    (*  Application *)
    | Absyn.ApplicationTerm(Absyn.CurriedApplication(l, r), p) ->
        let l' = normalize l env true in
        if (isEntrySuspension l') then
          let t' = (getEntrySuspensionTerm l') in
          let sym = (Absyn.getTermAbstractionVar t') in
          let body = (Absyn.getTermAbstractionBody t') in
          let env' = makeSuspension sym r env (getEntrySuspensionEnv l') in
          let result = normalize body env' whnf in
            result
        else
          let l' = (getEntryTerm l') in
          let r' = (getEntryTerm (normalize r env false)) in
          let result = 
            TermEntry(Absyn.ApplicationTerm(
              Absyn.CurriedApplication(l', r'), p)
            ) in
            result
    
    | Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym, aterm), pos) ->
        if whnf then
          SuspensionEntry(term, env)
        else if (getEnvironmentSize env) > 0 then
          let tsym' = Absyn.BoundVar(Absyn.getTypeSymbolSymbol tsym, ref None, 
                                     ref false, 
                                     ref(Some(Absyn.getTypeSymbolType tsym))) in
          let t = Absyn.makeBoundVarTerm tsym' pos in
          let t' = makeTerm tsym t env in
          let aterm' = (getEntryTerm (normalize aterm t' false)) in
          if aterm <> aterm' then
            TermEntry(
              Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym', aterm'), pos))
          else
            TermEntry(term)
        else
          let aterm' = (getEntryTerm (normalize aterm emptyEnvironment false)) in
          if aterm <> aterm' then
            TermEntry(Absyn.AbstractionTerm(Absyn.NestedAbstraction(tsym, aterm'), 
                                            pos))
          else
            TermEntry(term)
    | Absyn.ErrorTerm -> TermEntry(Absyn.ErrorTerm)
    | _ ->
        (Errormsg.error (Absyn.getTermPos term) "Parse.normalize: invalid term type";
        TermEntry(Absyn.ErrorTerm))
  in

  let result = normalize term emptyEnvironment false in
  let term = getEntryTerm result in
    term

(**********************************************************************
* fixTerm:
* Gets a term in curried and named form but with no nested abstractions. 
* 
* Returns the term converted into de Bruijn representation with applications in 
* curried form and with closedness annotations and the sets of free term and 
* type variables in the term (indicated by unique Absyn.atypesymbol cells 
* in one case * and Absyn.atype cells of the form:
*   Absyn.TypeVarType(Absyn.BindableTypeVar(ref NONE))
* in the other case.
*
* Will flag a term as in error if implications occur in it.
**********************************************************************)
and fixTerm term =
  (* checks appearances of :- and => embedded in terms *)
  let checkIllegalConstant c pos =
    if ((c = Pervasive.implConstant) || (c = Pervasive.colondashConstant)) then
      Errormsg.error pos ("Symbol " ^ (Absyn.getConstantPrintName c) ^
        " is not permitted within terms")
    else () 
  in

  (*  a general list function needed to truncated type environments here *)
  let rec trunclist l n = 
    if n = 0 then []
    else 
      match l with
        (h::t) -> (h::(trunclist t (n-1)))
      | _ -> Errormsg.impossible Errormsg.none "
               Parse.trunclist: invalid arguments."
  in

  (*  count the binders back to the one binding the given bound variable 
      occurrence; needed to compute the de Bruijn index *)
  let rec findBVinList bv bvs =
    let rec findBVinList' bv bvs n =
      match bvs with
          (hbv::tbvs) ->
            if (bv == hbv) then n
            else (findBVinList' bv tbvs (n + 1))
        | _ -> Errormsg.impossible Errormsg.none
                "Parse.fixTerm.findBVinList: bound variable not bound in term."
    in findBVinList' bv bvs 1
  in

  (*  un-curry applications generating a head, arg list pair to be fixed before
      being put together using a first-order like application. 
      Wonder why this was not already done in the call to 
      removeNestedAbstractions in translateTermTopLevel *)
  let rec unCurryTopLevelApps appterm args =
    match appterm with
        (Absyn.CurriedApplication(f,a)) -> 
          (match f with
              (Absyn.ApplicationTerm(appterm',_)) -> 
                unCurryTopLevelApps appterm' (a::args)
            | _ -> (f, a :: args))
      | _ -> Errormsg.impossible Errormsg.none
          ("Parse.fixTerm.unCurryTopLevelApps: " ^ 
             "found non-Curried form of application")
  in

  (* the main function in fixTerm. Like fixTerm except that it adds the 
     free variable and the bound variable lists as arguments and returns also
     the highest de Bruijn index within the term *)
  let rec fixTerm' term bvars fvars ftyvars =
    match term with 
        Absyn.IntTerm(i,p) -> (Absyn.IntTerm(i,p),fvars,ftyvars,0)
      | Absyn.RealTerm(r,p) -> (Absyn.RealTerm(r,p),fvars,ftyvars,0)
      | Absyn.StringTerm(s,p) -> (Absyn.StringTerm(s,p),fvars,ftyvars,0)
      | Absyn.ConstantTerm(c,tenv,p) -> 
          (* collect type variables in (needed components of) type environment 
             and check constant is legal here *)
         let _ = checkIllegalConstant c p in
          let neededtenv = 
            trunclist tenv (Absyn.getConstantTypeEnvSize false c) in
            (Absyn.ConstantTerm(c,neededtenv,p),fvars,
             Types.getNewVarsInTypes neededtenv ftyvars,0)
      | Absyn.FreeVarTerm(fv,p) -> 
          (match fv with
            Absyn.NamedFreeVar(tysy) -> 
              let nfvars = 
                if (List.memq tysy fvars) then
                  fvars
                else
                  (tysy :: fvars)
              in
              (Absyn.FreeVarTerm(fv,p),nfvars,ftyvars,0)
          | _ -> Errormsg.impossible 
                   Errormsg.none "Parse.fixTerm.FreeVarTerm: non-named var.")
      | Absyn.BoundVarTerm(bv,p) ->
          (match bv with 
            Absyn.NamedBoundVar(tysy) ->
               let ind = (findBVinList tysy bvars) in
                 (Absyn.BoundVarTerm(Absyn.DBIndex(ind),p),fvars,ftyvars,ind)
          | _ -> Errormsg.impossible 
                   Errormsg.none "Parse.fixTerm.BoundVarTerm: non-named form.")
      | Absyn.AbstractionTerm(abs,p) ->
          (* note that the list of abstracted variables is in reverse order
             after removeNestedAbstractions *)
          (match abs with
            (Absyn.UNestedAbstraction(tysyl,i,body)) ->
                let (nbody,nfvars,nftyvars,ind) =
                  fixTerm' body ((List.rev tysyl) @ bvars) fvars ftyvars
                in
                let ind' = (if (ind > 0) then (ind - 1) else 0) in
                (Absyn.AbstractionTerm(Absyn.UNestedAbstraction(tysyl,i,nbody),
                  p),
                  nfvars,nftyvars,ind')
          | _ -> Errormsg.impossible Errormsg.none 
                  "Parse.fixTerm.AbstractionTerm: nested abstractions?")
      | Absyn.ApplicationTerm(appterm,p) ->
         let (h,args) = unCurryTopLevelApps appterm [] in
         let (nh,nfvars,nftyvars,ind) = fixTerm' h bvars fvars ftyvars in
         let (nargs,nfvars',nftyvars',ind') = 
           (fixTerms args bvars nfvars nftyvars ind) in
         (Absyn.ApplicationTerm(Absyn.FirstOrderApplication(nh,nargs,
                                                            List.length nargs),
                                   p),nfvars',nftyvars',ind')
      | Absyn.ErrorTerm -> (Absyn.ErrorTerm, fvars, ftyvars, 0)

  and fixTerms ts bvars fvars ftyvars ind = 
    match ts with
        [] -> ([],fvars,ftyvars,ind)
      | (t::ts') -> 
          let (nt,fvars',ftyvars',ind') = (fixTerm' t bvars fvars ftyvars) in
          let ind'' = if (ind' > ind) then ind' else ind in
          let (nts',fvars'',ftyvars'',ind'') = 
                      (fixTerms ts' bvars fvars' ftyvars' ind'') in
             (nt::nts',fvars'',ftyvars'',ind'')
            
  in 
  let (t,fvars,ftyvars,_) = fixTerm' term [] [] [] in
    (t, fvars, ftyvars)  

let unitTests () =
  let absyn = Absyn.Module("UnitTests", [], [],
                           ref (Pervasive.pervasiveConstants), 
                           ref (Pervasive.pervasiveKinds), 
                           Table.empty, [], [], [], [], [], ref [], [], ref [],
                           ref (Absyn.ClauseBlocks([])))
  in

  let test t =
    let (t',_,_,_) = Option.get (translateTermTopLevel t absyn) in
    let _ = Errormsg.log Errormsg.none 
      ("Preabsyn Term: " ^ (Preabsyn.string_of_term t)) in
    let _ = Errormsg.log Errormsg.none 
      ("Absyn Term: " ^ (Absyn.string_of_term t')) in
    ()
  in
  
  let _ = Errormsg.log Errormsg.none ("Parse Unit Tests:") in
    
  let t1 = Preabsyn.SeqTerm(
    [Preabsyn.IdTerm(Symbol.symbol "true", 
      None, Preabsyn.ConstID, Errormsg.none)], 
    Errormsg.none) in
  let t2 = Preabsyn.SeqTerm([Preabsyn.IntTerm(1, Errormsg.none); 
                             Preabsyn.IdTerm(Symbol.symbol "+", None, 
                                             Preabsyn.ConstID, Errormsg.none); 
                             Preabsyn.IntTerm(1, Errormsg.none)], 
                            Errormsg.none) in
  let t3 = 
    Preabsyn.SeqTerm([Preabsyn.IntTerm(1, Errormsg.none); 
                      Preabsyn.IdTerm(Symbol.symbol "+", None, Preabsyn.ConstID, 
                                      Errormsg.none); t2], 
                     Errormsg.none) in  
  let t5 = Preabsyn.ListTerm([Preabsyn.IntTerm(1, Errormsg.none); 
                              Preabsyn.IdTerm(Symbol.symbol ",", None, 
                                              Preabsyn.ConstID, Errormsg.none); 
                              Preabsyn.IntTerm(2, Errormsg.none)], 
                             Errormsg.none) in
  let t6 = Preabsyn.SeqTerm([Preabsyn.IntTerm(1, Errormsg.none); 
                             Preabsyn.IdTerm(Symbol.symbol "::", None, 
                                             Preabsyn.ConstID, Errormsg.none); 
                             Preabsyn.IdTerm(Symbol.symbol "nil", None, 
                                             Preabsyn.ConstID, Errormsg.none)], 
                            Errormsg.none) in
  let t7 = Preabsyn.SeqTerm([Preabsyn.IntTerm(1, Errormsg.none); 
                             Preabsyn.IdTerm(Symbol.symbol "::", None, 
                                             Preabsyn.ConstID, Errormsg.none); 
                             Preabsyn.IntTerm(2, Errormsg.none); 
                             Preabsyn.IdTerm(Symbol.symbol "::", None, 
                                             Preabsyn.ConstID, Errormsg.none); 
                             Preabsyn.IdTerm(Symbol.symbol "nil", None, 
                             Preabsyn.ConstID, Errormsg.none)], 
                            Errormsg.none) in

  let () = test t1 in
  let () = test t2 in
  let () = test t3 in
  let () = test t5 in
  let () = test t6 in
  let () = test t7 in
  
  let _ = Errormsg.log Errormsg.none ("Parse Unit Tests Compete.\n") in
  ()
