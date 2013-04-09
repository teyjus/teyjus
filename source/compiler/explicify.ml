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

(* val explicify_clauses : Absyn.aterm list ->  Absyn.aterm list *)
let rec explicify_clauses clauses = match clauses with
  | [] -> []
  | (clause::q) -> Printf.printf "%s\n" (string_of_term clause);
                     (clause::(explicify_clauses q))


(*  explicify : 
    Absyn.amodule -> 
    Absyn.aterm list -> 
    Absyn.aterm list->
    (Absyn.amodule * Absyn.aterm list * Absyn.aterm list) *)
let explicify m clauses newclauses = 
  match m with
    | Module(name, implist, acclist, kind_t, const_t, tabb_t, strings,
            gkinds, lkinds, gconsts, lconsts, hidden_const, skel, hskel, ci) ->
        let  clauses' = explicify_clauses clauses in
        (m, clauses', newclauses)
    | _ -> Errormsg.impossible Errormsg.none "Explicify: invalid module"
