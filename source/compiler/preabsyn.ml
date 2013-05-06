(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Zach Snow
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
(**********************************************************************
*Preabsyn Module:
* The prebstract syntax for Teyjus.
**********************************************************************)

type symbol = Symbol.symbol
type pos = Errormsg.pos

(* Kinds of Identifiers *)
type pidkind =
  | CVID
  | ConstID
  | AVID
  | VarID

type pfixitykind =
  | Infix of pos
  | Infixl of pos
  | Infixr of pos
  | Prefix of pos
  | Prefixr of pos
  | Postfix of pos
  | Postfixl of pos
    
(* Symbols *)
type psymbol = Symbol of symbol * pidkind * pos

(* Type Symbols *)
and ptypesymbol = TypeSymbol of (symbol * ptype option * pidkind * pos)

(* Types *)
and ptype =
  | Atom of symbol * pidkind * pos
  | App of ptype * ptype * pos
  | Arrow of ptype * ptype * pos
  | ErrorType

and ptypeabbrev = TypeAbbrev of psymbol * psymbol list * ptype * pos

and pboundterm = BoundTerm of ptypesymbol list * pterm list

(* Terms *)
and pterm =
  | SeqTerm of pterm list * pos
  | ListTerm of pterm list * pos
  | ConsTerm of pterm list * pterm * pos
  | LambdaTerm of ptypesymbol list * pterm list * pos
  | IdTerm of (symbol * ptype option * pidkind * pos)
  | RealTerm of float * pos
  | IntTerm of int * pos
  | StringTerm of string * pos
  | ErrorTerm
  
and pclause = Clause of pterm

(* Constants *)
and pconstant = Constant of psymbol list * ptype option * pos

(* Kinds *)
and pkind = Kind of psymbol list * int option * pos

(* Fixity *)
and pfixity = Fixity of psymbol list * pfixitykind * int * pos

(********************************************************************
* Module:
*  This type stores information about a preabsyn module.
*  See interface for details.
********************************************************************)
type pmodule =
  | Module of string * pconstant list * pconstant list * 
      pconstant list * pconstant list * pconstant list * pfixity list *
      pkind list * pkind list * ptypeabbrev list * pclause list * psymbol list *
      psymbol list * psymbol list * psymbol list
  | Signature of string * pconstant list * pconstant list *
      pconstant list * pkind list *
      ptypeabbrev list * pfixity list * psymbol list * psymbol list

let string_of_pos pos = Errormsg.string_of_pos pos

let map_with_commas f list = String.concat ", " (List.map f list)
  
let rec string_of_termlist list =
  map_with_commas string_of_term list

and string_of_typesymbollist list =
  map_with_commas string_of_typesymbol list

and string_of_typesymbol = function
  | TypeSymbol(tsym, Some t, idk, pos) ->
      "TypeSymbol(" ^ (Symbol.name tsym) ^ ", " ^
        (string_of_type t) ^ ", " ^ (string_of_idkind idk) ^ ", " ^
        (string_of_pos pos) ^ ")"
  | TypeSymbol(tsym, None, idk, pos) ->
      "TypeSymbol(" ^ (Symbol.name tsym) ^ ", " ^
        (string_of_idkind idk) ^ ", " ^ (string_of_pos pos) ^ ")"

and string_of_term = function
  | SeqTerm(tlist, pos) ->
      "SeqTerm([" ^ (string_of_termlist tlist) ^ "], " ^
        (string_of_pos pos) ^ ")"
  | ListTerm(tlist, pos) ->
      "ListTerm([" ^ (string_of_termlist tlist) ^ "], " ^
        (string_of_pos pos) ^ ")"
  | ConsTerm(tlist, t, pos) ->
      "ConsTerm([" ^ (string_of_termlist tlist) ^ "], " ^
        (string_of_term t) ^ ", " ^ (string_of_pos pos) ^ ")"
  | IdTerm(sym, None, k, pos) ->
      "IdTerm(" ^ (Symbol.name sym) ^ ", " ^ (string_of_idkind k) ^ ", " ^
        (string_of_pos pos) ^ ")"
  | IdTerm(sym, Some t, k, pos) ->
      "IdTerm(" ^ (Symbol.name sym) ^ ", " ^ (string_of_type t) ^ ", " ^
        (string_of_idkind k) ^ ", " ^ (string_of_pos pos) ^ ")"
  | RealTerm(r, pos) ->
      "RealTerm(" ^ (string_of_float r) ^ ", " ^
        (string_of_pos pos) ^ ")"
  | IntTerm(i, pos) ->
      "IntTerm(" ^ (string_of_int i) ^ ", " ^
        (string_of_pos pos) ^ ")"
  | StringTerm(s, pos) ->
      "StringTerm(" ^ s ^ ", " ^ (string_of_pos pos) ^ ")"
  | LambdaTerm(lt, t, pos) ->
      "LambdaTerm([" ^ (string_of_typesymbollist lt) ^ "], " ^
        (string_of_termlist t) ^ ", " ^ (string_of_pos pos) ^ ")"
  | ErrorTerm ->
      "Error"
        
and string_of_boundterm = function
  | BoundTerm(tysy, tl) ->
      "BoundTerm([" ^ (string_of_typesymbollist tysy) ^
        "], [" ^ (string_of_termlist tl) ^ "])"

and string_of_idkind = function
  | CVID -> "CVID"
  | ConstID -> "ConstID"
  | AVID -> "AVID"
  | VarID -> "VarID"

and string_of_type = function
  | Atom(sym, k, pos) ->
      "Atom(" ^ (Symbol.name sym) ^ ", " ^ (string_of_idkind k) ^ ", " ^
        (string_of_pos pos) ^ ")"
  | App(t1, t2, pos) ->
      "App(" ^ (string_of_type t1) ^ ", " ^ (string_of_type t2) ^ ", " ^
        (string_of_pos pos) ^ ")"
  | Arrow(t1, t2, pos) ->
      "Arrow(" ^ (string_of_type t1) ^ ", " ^ (string_of_type t2) ^ ", " ^
         (string_of_pos pos) ^ ")"
  | ErrorType ->
      "Error"
        
let string_of_symbol = function
  | Symbol(sym, k, pos) ->
      "Symbol(" ^ (Symbol.name sym) ^ ", " ^ (string_of_idkind k) ^ ", " ^
        (string_of_pos pos) ^ ")"

let string_of_symbollist list =
  map_with_commas string_of_symbol list

let string_of_constant = function
  | Constant(symlist, Some t, pos) ->
      "Constant(" ^ (string_of_symbollist symlist) ^
        ", " ^ (string_of_type t) ^ ", " ^ (string_of_pos pos) ^ ")"
  | Constant(symlist, None, pos) ->
      "Constant(" ^ (string_of_symbollist symlist) ^
        ", " ^ (string_of_pos pos) ^ ")"
        
let string_of_typeabbrev = function
  | TypeAbbrev(name, arglist, ty, pos) ->
      "TypeAbbrev(" ^ (string_of_symbol name) ^
        (string_of_symbollist arglist) ^ ", " ^
        (string_of_type ty) ^ ", " ^ (string_of_pos pos) ^ ")"

let string_of_fixitykind = function
  | Infix(pos) -> "Infix(" ^ (string_of_pos pos) ^ ") "
  | Infixl(pos) -> "Infixl(" ^ (string_of_pos pos) ^ ") "
  | Infixr(pos) -> "Infixr(" ^ (string_of_pos pos) ^ ") "
  | Prefix(pos) -> "Prefix(" ^ (string_of_pos pos) ^ ") "
  | Prefixr(pos) -> "Prefixr(" ^ (string_of_pos pos) ^ ") "
  | Postfix(pos) -> "Postfix(" ^ (string_of_pos pos) ^ ") "
  | Postfixl(pos) -> "Postfixl(" ^ (string_of_pos pos) ^ ") "

let string_of_fixity = function
  | Fixity(names, k, prec, pos) ->
      "Fixity(" ^ (string_of_symbollist names) ^
        ", " ^ (string_of_fixitykind k) ^ ", " ^ (string_of_int prec) ^ ", " ^
        (string_of_pos pos) ^ ")"
          
let string_of_kind = function
  | Kind(symlist, Some i, pos) ->
      "Kind(" ^ (string_of_symbollist symlist) ^
        ", " ^ (string_of_int i) ^ ", " ^ (string_of_pos pos) ^ ")"
  | Kind(symlist, None, pos) ->
      "Kind(" ^ (string_of_symbollist symlist) ^
        ", " ^ (string_of_pos pos) ^ ")"

let string_of_clause = function
  | Clause(ts) -> "Clause(" ^ (string_of_term ts) ^ ")"

(********************************************************************
 * printPreAbsyn:
 *  Prints all information about a preabsyn module.
 ********************************************************************)
let printPreAbsyn m out =
  (* Text output functions *)
  let output_line s = output_string out (s ^ "\n") in
  let output_list f list = List.iter (fun t -> output_line (f t)) list in
    match m with
      | Module(name, gconstants, lconstants, cconstants, uconstants,
               econstants, fixities, gkinds, lkinds, tabbrevs, clauses, accummod,
               accumsig, usesig, impmods) ->
          output_line ("Module: " ^ name) ;
          output_line "Constants:" ;
          output_list string_of_constant gconstants ;
          output_list string_of_constant lconstants ;
          output_list string_of_constant cconstants ;
          output_line "" ;
          output_line "Kinds:" ;
          output_list string_of_kind gkinds ;
          output_line "" ;
          output_list string_of_kind lkinds ;
          output_line "Type Abbrevs:" ;
          output_list string_of_typeabbrev tabbrevs ;
          output_line "Clauses:" ;
          output_list string_of_clause clauses ;
          output_line "Fixities:" ;
          output_list string_of_fixity fixities
            
      | Signature(name, gconstants, _,_, gkinds, tabbrevs, fixities, accumsig, usig) ->
          output_line ("Signature: " ^ name) ;
          output_line "Constants:" ;
          output_list string_of_constant gconstants ;
          output_line "Kinds:" ;
          output_list string_of_kind gkinds ;
          output_line "Type Abbrevs:" ;
          output_list string_of_typeabbrev tabbrevs

let getFixityPos = function
  | Infix(pos) -> pos
  | Infixl(pos) -> pos
  | Infixr(pos) -> pos
  | Prefix(pos) -> pos
  | Prefixr(pos) -> pos
  | Postfix(pos) -> pos
  | Postfixl(pos) -> pos

let getTermPos = function
  | SeqTerm(_, pos) -> pos
  | ListTerm(_, pos) -> pos
  | ConsTerm(_, _, pos) -> pos
  | IdTerm(_, _, _, pos) -> pos
  | RealTerm(_, pos) -> pos
  | IntTerm(_, pos) -> pos
  | StringTerm(_, pos) -> pos
  | LambdaTerm(_,_,pos) -> pos
  | ErrorTerm ->
      Errormsg.impossible Errormsg.none "Preabsyn.getTermPos: invalid term"

let getClauseTerm = function
  Clause(t) -> t

let getModuleClauses = function
  | Module(name, gconsts, lconsts, cconsts, uconsts, econsts, fixities,
      gkinds, lkinds, tabbrevs, clauses, accummods,
      accumsigs, usesigs, impmods) -> clauses
  | _ ->
      Errormsg.impossible Errormsg.none
        "Preabsyn.getModuleClauses: invalid module"

(* Retrieve all the constants in a pmod *)
let getAllConstants pmod =
  match pmod with 
  | Module(name, gconsts, lconsts, cconsts, uconsts, econsts, fixities,
      gkinds, lkinds, tabbrevs, clauses, accummods,
      accumsigs, usesigs, impmods) ->
      gconsts @ lconsts @ cconsts @ uconsts @ econsts
  | Signature(name, gconstants, useonly, exportdef, gkinds, tabbrevs, 
              fixities, accumsig, usig) ->
      gconstants @ useonly 

let clause_sym = Symbol.symbol "clause__" 
let fact_sym = Symbol.symbol "fact__" 
let implies_sym = Symbol.symbol "implies__" 

let explicify pmod all_consts = 
  let explicify_name name = name ^ "_exp" in

  let typ_clause = 
    Arrow(
      Atom(Symbol.symbol "o", ConstID, Errormsg.none), 
      Arrow(
        App(
            Atom(Symbol.symbol "list", ConstID, Errormsg.none),
            Atom(Symbol.symbol "o", ConstID, Errormsg.none), 
            Errormsg.none
        ),   
        Atom(Symbol.symbol "o", ConstID, Errormsg.none), 
        Errormsg.none), 
      Errormsg.none)
  in

  let typ_fact = 
    Arrow(
      Atom(Symbol.symbol "o", ConstID, Errormsg.none), 
      Atom(Symbol.symbol "o", ConstID, Errormsg.none), 
      Errormsg.none) 
  in

  let cons = IdTerm(Symbol.symbol "::", None, ConstID, Errormsg.none) in
  let nil = IdTerm(Symbol.symbol "nil", None, ConstID, Errormsg.none) in

  (* Given a list of pterms with at most one symbol sym:
   - returns None if the symbol is not present
   - returns Some(before, after) where 
        before and after are respectively the elements before 
        and after the symbol *)
  let split_symbol ptermlist sym = 
    let rec aux ptermlist before   =
      match ptermlist with
        | [] -> None
        | IdTerm(symbol, _, _, _)::q
            when ((Symbol.name symbol) = sym) ->
              Some(List.rev before, q)
        | x::q -> aux q (x::before)
    in
     aux ptermlist [] 
  in

  (* Given  a list of pterms p x1 .. xn, (q y1 ... ym, r z1 ... zk) returns
  * a list of lists: 
  * [p x1 .. xn] ; [q y1 ... ym] ; [r z1 ... zk] 
  *
  * The function does not recursively flatten conjuncts i.e.
  * ff an argument of a predicate is of the shape (s ..., u ...)
  * it is not modified *)
  let rec flatten_conjuncts ptermlist = 
    let rec aux ptermlist acc =
      match ptermlist with 
        | [] -> [List.rev acc] 
        | (IdTerm(symbol, _, _, _)::q) when Symbol.name symbol = "," -> 
            (List.rev acc)::(aux q [])
        | [SeqTerm(l, p)] when acc = [] -> 
            (* Testing if acc = [] avoids  parsing the follwoing
             * p :- q, f (r, s)
             * as p :- q, f r, s *)
            begin
            match (aux l []) with
              | [] -> 
                  Errormsg.impossible Errormsg.none 
                    "Flatten conjuncts: impossible case"
              | [single] -> (List.rev acc) :: [single]
              | other ->  other
            end
        | other::q -> aux q (other::acc) 
    in
      aux ptermlist []
  in

  let rec insert_cons : pterm list -> pterm list = fun ptermlist ->
    match ptermlist with
      | [] -> [nil]
      | x::q -> x :: cons  :: (insert_cons q)
  in

  (* Explicify a list of pterms. 
   * top_level indicates if we at the top level of a rule's body 
   * i.e. we will need to wrap the return value in a list
   *)
  let rec exp_list l top_level =

    (* core : pterm list -> bool -> pterm list 
    * Given a list of pterms __without__ commas at top-level we recursively
    * make explicit all the elements
    * is_arg is here to indicate if the first element is in position
    * to be the argument of a predicate.
    * Indeed we need in cases such as
    *    type f o -> o.
    *    type x o.
    *    f x.
    * to embed the term x into the singleton list since
    * every predicate's type is transformed such that every atom o
    * in an argument position is transformed into list o *)
    let rec core ptermlist is_arg = 
      match ptermlist with
      | [] -> []
      | SeqTerm(l, p)::q -> 
          (* is_arg can still be true.
           * For instance while reading
           * type f o -> o -> o.
           * type a int -> o.
           * f (a 1) (a 2).
           * we will have
           * SeqTerm( a 1 ) :: SeqTerm ( a 2) *)
          (exp_list l false)::(core q is_arg)
      | (IdTerm(sym, ptypopt, ConstID, pos) as idt)::q 
        when 
          (* If symbol's type is o and is in an argument's position,
           * then we transform the term into a singleton *)
          (List.exists 
             (fun const -> 
                let o = Symbol.symbol "o" in
                  match const with
                    | Constant(psym_list, 
                               Some(
                                 Atom(t_sym, ConstID, _)
                               ),
                               _) when (t_sym = o) -> 
                        (List.exists 
                           (fun (Symbol(sym', _, _)) -> sym = sym') 
                           psym_list)
                    | _ -> false
             )
             all_consts) 
        ->
          (* In both cases the next IdTerm is still in position to be an 
           * argument (if we have for instance type f o -> o -> o *)
          if is_arg then
            SeqTerm(idt::cons::[nil], Errormsg.none)::(core q true)
          else
            idt::(core q true)
      | (IdTerm(_, _, _, _) as idt)::q -> 
          idt::(core q true)
(*      | LambdaTerm(symlist, plist, pos)::q ->*)
(*          LambdaTerm(symlist, [exp_list plist false], pos)::(core q)*)
      | other::q -> other::(core q false)
    in

    (* First of all we split list of terms into a list of lists
     * according the commas at the top level *)
    let ll = flatten_conjuncts l in
      match ll with
        | [] -> 
            Errormsg.impossible Errormsg.none 
              "Explicit list: impossible case"
        | [no_more] ->
            (* There are no commas at top level. 
             * We need however to continue inside the lists which may 
             * be deep inside *)
            let core_exp = core no_more false in
              if top_level then
                SeqTerm(insert_cons [SeqTerm(core_exp, Errormsg.none)], 
                        Errormsg.none) 
              else
                SeqTerm((core no_more false), Errormsg.none)
        | other -> 
            (* We recursively make explicit all the elements of the list
             * which are independent and then finally put them together
             * in a sequence where are delimited by :: *)
            let other_exp = 
              List.map (fun x -> SeqTerm(core x false, Errormsg.none)) other in
              SeqTerm(insert_cons other_exp, Errormsg.none)
  in


  let clause_psym = Symbol(clause_sym, ConstID, Errormsg.none) in 
  let clause_constant = Constant([clause_psym], Some(typ_clause), 
                                  Errormsg.none) in 
  let fact_psym = Symbol(fact_sym, ConstID, Errormsg.none) in 
  let fact_constant = Constant([fact_psym], Some(typ_fact), 
                                  Errormsg.none) in 

  (* To explicify a clause we: 
   - split the clause into two lists: the head and the body 
    (None if it is a fact)
   - Make the explicit the list of terms contained in the body
    of the rule (or the fact) *)                                 
  let explicify_clause pterm = 
(*    Printf.printf "term bef = %s\n\n" (string_of_term pterm);*)
    match pterm with 
      | SeqTerm(ptermlist, pos) -> 
          begin
          match (split_symbol ptermlist ":-") with
            | None -> 
                (* Fact *)
                let fact_exp = exp_list ptermlist false in
                let seq = 
                  SeqTerm(
                    [IdTerm(fact_sym, Some(typ_fact), 
                            ConstID, Errormsg.none);
                     fact_exp],
                    pos) in
(*                  Printf.printf "term aft = %s\n\n" (string_of_term seq);*)
                  seq
            | Some(head, body) -> 
                (* Rule *)
                let body_exp = exp_list body true in
                let seq = 
                  SeqTerm(
                    [IdTerm(clause_sym, Some(typ_clause), 
                            ConstID, Errormsg.none);
                     SeqTerm(head, Errormsg.none);
                     body_exp],
                    pos) in
(*                    Printf.printf "term aft = %s\n\n" (string_of_term seq);*)
                  seq

          end
      (* No need to take care of :
       * - ConsTerm since it is removed by the first translation. 
       * - ListTerm since => cannot appear inside *) 
      | other -> other
  in

            
  (* Each constant's type is transformed in the following way:
   * If there is an atom o in an argument position, then
   * it is changed as list o *)
  let exp_const ((Constant(psymlist, ptypopt, pos)) as const)= 
    let rec exp_type typ output_pos = 
      match typ with 
        | Atom(sym, _, pos) when 
            (Symbol.name sym = "o" && output_pos = false)
          -> 
            App(
              Atom(Symbol.symbol "list", ConstID, Errormsg.none),
              Atom(Symbol.symbol "o", ConstID,  Errormsg.none), 
              pos)
        | Arrow(ptyp_i, ptyp_o, pos) ->
            let ptyp_i' = exp_type ptyp_i false in
            let ptyp_o' = exp_type ptyp_o true in
              Arrow(ptyp_i', ptyp_o', pos)
        | _ -> typ
    in
      match ptypopt with 
        | None -> const
        | Some(typ) -> 
            Constant(psymlist, Some(exp_type typ true), pos)
  in

  match pmod with 
  | Module(name, gconsts, lconsts, cconsts, uconsts, econsts, fixities,
      gkinds, lkinds, tabbrevs, clauses, accummods,
      accumsigs, usesigs, impmods) ->
      let name' = explicify_name name in
      (* Clauses are explicified after a first pass so the preabstract 
       * syntax is already under some normal form shape. 
       * We thus expect the following restrictions: 
       - Each clause contains at most one :-
       - There are no => to the left of a :- (at any depth)
       - Two => appearing at the same depth belong to two different conjunctions
       - The body of a clause (and of a fact) has the shape a, (b, (c, ...))           - Facts do not contain commas at their top level 
         (only within arguments of predicates)

      *)   
      let clauses' = List.map (fun cl -> 
                                 let pterm = getClauseTerm cl in
                                 let pterm' = explicify_clause pterm  in 
                                   Clause(pterm')) clauses in

      let gconsts_exp = List.map (exp_const) gconsts in
      Module(name', gconsts_exp, lconsts, cconsts, uconsts, econsts, fixities,
             gkinds, lkinds, tabbrevs, clauses', accummods,
             accumsigs, usesigs, impmods) 
  | Signature(name, gconstants, useonly, exportdef, gkinds, tabbrevs, 
              fixities, accumsig, usig) ->
      let gconstants_exp = List.map (exp_const) gconstants in
      let gconstants' = (fact_constant::clause_constant::gconstants_exp) in
(*      List.iter (fun t -> Printf.printf "t = %s\n" (string_of_constant t)) gconstants' ;*)
      Signature(name, gconstants', useonly, exportdef, gkinds, tabbrevs, 
                fixities, accumsig, usig)
