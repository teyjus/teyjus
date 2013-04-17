(****************************************************************************
*Copyright 2008, 2013
*  Andrew Gacek, Steven Holte, Gopalan Nadathur, Xiaochu Qi, Fabien Renaud,
*  Zach Snow
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
open Absyn


(* Related to the new constants we introduce *)

let kbool = Kind(Symbol.symbol "o", Some 0, ref 2, PervasiveKind, Errormsg.none)

let tyskel = 
  Skeleton(
    ArrowType(
      ApplicationType(kbool, []), 
      ArrowType(
        ApplicationType(kbool, []), 
        ApplicationType(kbool, [])
      )
    ), 
    ref None, 
    ref false)

let clause_constant =
  let symbol = Symbol.symbol "__clause__" in
  let fixity = Infixl in
  let prec = 0 in 
  let exp_def = false in
  let use_only = false in
  let tenv_size = 0 in 
  let skel =  tyskel in
  let index = 0 in (* TODO: check this is correct *)
    makeGlobalConstant symbol fixity prec exp_def 
      use_only tenv_size skel index 

(* val explicify_clause : Absyn.aterm  ->  Absyn.aterm list *)
let rec explicify_clause clause = 
    match clause with
    | ApplicationTerm(
        FirstOrderApplication(ConstantTerm(const, term_list, pos_cons), 
                              args, numargs),
        pos) when const = Pervasive.implConstant ->
        let f' = makeConstantTerm clause_constant term_list pos_cons in
          ApplicationTerm(FirstOrderApplication(f', args, numargs), pos)
    | _ -> clause




let print_constant const = 
    Printf.printf "pred = %s\n" (getConstantPrintName const)

let print_clause clause = 
  let pred = getClausePred clause in 
    print_constant pred

let print_aclausesblock acb =
  let clauses = getClauseBlockClauses acb in
    List.iter (print_clause) clauses
      
let explicify_consts gconsts = gconsts


(*  explicify : 
    Absyn.amodule -> 
    Absyn.aterm list -> 
    Absyn.aterm list->
    (Absyn.amodule * Absyn.aterm list * Absyn.aterm list) *)
let explicify m clauses newclauses = 
  match m with
    | Module(name, implist, acclist, kind_t, const_t, tabb_t, strings,
            gkinds, lkinds, gconsts, lconsts, hidden_const, skel, hskel, ci) ->
        let clauses' = List.map (explicify_clause) clauses in
        let gconsts' = explicify_consts gconsts in
        let m' = Module(name, implist, acclist, kind_t, const_t, tabb_t, strings,
            gkinds, lkinds, gconsts', lconsts, hidden_const, skel, hskel, ci) in
        (m', clauses', newclauses)
    | _ -> (m, clauses, newclauses)
