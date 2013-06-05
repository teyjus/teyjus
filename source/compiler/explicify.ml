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
let klist = Kind(Symbol.symbol "list", Some 1, ref 4, PervasiveKind, 
                 Errormsg.none)
let o_type = Absyn.makeKindType kbool
let list_type = Absyn.makeKindType klist

let tyskel_clause = 
  Skeleton(
    ArrowType(
      ApplicationType(kbool, []), 
      ArrowType(
        ApplicationType(klist, [ApplicationType(kbool, [])]), 
        ApplicationType(kbool, [])
      )
    ), 
    ref None, 
    ref false)

let tyskel_fact = 
  Skeleton(
    ArrowType(
      ApplicationType(kbool, []), 
      ApplicationType(kbool, [])),
    ref None, 
    ref false)

let tyskel_forall = 
  Skeleton(
    ArrowType(
      ArrowType(
        SkeletonVarType((ref 0)), 
        ApplicationType(klist, [ApplicationType(kbool, [])])
      ), 
      ApplicationType(kbool, [])
    ),
    ref None, 
    ref false)

let clause_constant =
  let symbol = Symbol.symbol "clause" in
  let fixity = Infixl in
  let prec = 0 in 
  let exp_def = false in
  let use_only = false in
  let tenv_size = 0 in 
  let skel =  tyskel_clause in
  let index = 0 in (* TODO: check this is correct *)
    makeGlobalConstant symbol fixity prec exp_def 
      use_only tenv_size skel index 

let fact_constant =
  let symbol = Symbol.symbol "fact" in
  let fixity = NoFixity in
  let prec = 0 in 
  let exp_def = false in
  let use_only = false in
  let tenv_size = 0 in 
  let skel =  tyskel_fact in
  let index = 0 in (* TODO: check this is correct *)
    makeGlobalConstant symbol fixity prec exp_def 
      use_only tenv_size skel index 

let implies_constant =
  let symbol = Symbol.symbol "implies" in
  let fixity = Infixr in
  let prec = 130 in 
  let exp_def = false in
  let use_only = false in
  let tenv_size = 0 in 
  let skel =  tyskel_clause in
  let index = 0 in (* TODO: check this is correct *)
    makeGlobalConstant symbol fixity prec exp_def 
      use_only tenv_size skel index 

let forall_constant =
  let symbol = Symbol.symbol "pi'" in
  let fixity = NoFixity in
  let prec = 0 in 
  let exp_def = false in
  let use_only = false in
  let tenv_size = 1 in 
  let skel =  tyskel_forall in
  let index = 0 in (* TODO: check this is correct *)
    makeGlobalConstant symbol fixity prec exp_def 
      use_only tenv_size skel index 

let rec embed_terms_in_list t_list =
  match t_list with 
    | [] -> 
        Absyn.ConstantTerm( 
          Pervasive.nilConstant,
          [o_type],
          Errormsg.none)
    | elt::q ->
        Absyn.ApplicationTerm(FirstOrderApplication( 
          Absyn.ConstantTerm(
            Pervasive.consConstant,
            [o_type],
            Errormsg.none),
          elt::[(embed_terms_in_list q)],
          2), Errormsg.none)

let rec flatten_ands term = 
    match term with
      | ApplicationTerm(
          FirstOrderApplication(
            ConstantTerm(const, atyp_list, const_pos),
            [left;right],
            2
          ),
          pos) when Pervasive.isandConstant const ->
          left::(flatten_ands right)
      | other -> [other]

(* Explicify a constant term in an argument position *)
let explicify_const term = 
  match term with
    | ConstantTerm(const, tys, pos) as ct -> 
        begin
        let skel = getConstantSkeletonValue const in
        let ty = getSkeletonType skel in
          match ty with
            | ApplicationType(t, []) when t = kbool ->
(*                Printf.printf "!! Explicify const : %s of type %s\n" *)
(*                  (string_of_term_ast term) (string_of_type_ast ty);*)
                embed_terms_in_list [ct] 
            | _ -> 
                ct
        end
    | _ -> term 


(* val explicify_term : Absyn.aterm  ->  Absyn.aterm  *)
let rec explicify_term term add_sing top_level = 
(*  Printf.printf "t = %s\n" (string_of_term_ast term);*)
    match term with
      (* :- *)
      | ApplicationTerm(
          FirstOrderApplication(
            ConstantTerm(const, typ_list, pos_const), 
            body::[head], _), pos) 
          when const = Pervasive.implConstant ->
(*          Printf.printf "app term = %s\n" (string_of_term_ast body);*)
        let body_flat = flatten_ands body  in
        let body_exp = List.map (fun x -> explicify_term x false false) body_flat in
        let head_exp = explicify_term head false false in
        let body_exp_list = embed_terms_in_list body_exp in
          if top_level then
            ApplicationTerm(
              FirstOrderApplication(
                ConstantTerm(clause_constant, typ_list, pos_const), 
                [head_exp; body_exp_list], 2), 
              pos) 
          else
            let term_exp = ApplicationTerm(
              FirstOrderApplication(
                ConstantTerm(implies_constant, typ_list, pos_const), 
                [head_exp; body_exp_list], 2), 
              pos) in
              if add_sing then
                embed_terms_in_list [term_exp]
              else
                term_exp


      (* , i.e. conjunction not the list separator *)
      | ApplicationTerm(
          FirstOrderApplication(
            ConstantTerm(const, typ_list, pos_const), 
            args, _), pos) 
          when const = Pervasive.andConstant ->
          let term_flat = flatten_ands term  in
          let term_flat_exp = 
            List.map (fun x -> explicify_term x false false) term_flat in
            embed_terms_in_list term_flat_exp 

      (* pi *)
      | ApplicationTerm(
          FirstOrderApplication(
            ConstantTerm(const, typ_list, pos_const), 
            args, nb_args), pos) 
          when const = Pervasive.allConstant ->
          let args_exp = List.map (fun x -> explicify_term x true false) args in
          let term_exp = 
            ApplicationTerm(
              FirstOrderApplication(
                ConstantTerm(forall_constant, typ_list, pos_const), 
                args_exp, List.length args_exp), pos)  in
              term_exp

      (* Any other predicate *)
      | ApplicationTerm(
          FirstOrderApplication(
            ConstantTerm(const, typ_list, pos_cons), 
            args, nbargs), pos) ->
(*            Printf.printf "***  Term : %s \n"*)
(*                                (string_of_term_ast term);*)
          let exp_args = 
            List.map (fun x -> explicify_term x true false) args in
          let term_exp  = 
            ApplicationTerm(
              FirstOrderApplication(
                ConstantTerm(const, typ_list, pos_cons),
                exp_args, List.length exp_args), pos) in
(*            Printf.printf "*** Considering const : %s \n"*)
(*                                (string_of_term_ast term);*)
            let skel = getConstantSkeletonValue const in
            let cons_ty = getSkeletonType skel in
              if top_level then
                (* This is a fact *)
                begin
                ApplicationTerm(
                  FirstOrderApplication(
                    ConstantTerm(fact_constant, [], Errormsg.none),
                    [term_exp],
                    1),
                  Errormsg.none)
                end
              else
                if add_sing && 
                   (cons_ty = o_type || 
                    (isArrowType cons_ty && (getArrowTypeTarget cons_ty = o_type))) then
                  begin
(*                    Printf.printf "*** Considering const : %s \n"*)
(*                      (string_of_term_ast term);*)
                    embed_terms_in_list [term_exp] 
                  end
                else
                  term_exp

      | ConstantTerm(_, _, _)  as ct when add_sing -> explicify_const ct
      | ConstantTerm(_, _, _)  as ct -> 
          if top_level then
            (* This is a fact *)
            ApplicationTerm(
              FirstOrderApplication(
                ConstantTerm(fact_constant, [], Errormsg.none),
                [ct],
                1),
              Errormsg.none)
          else
              ct

      | AbstractionTerm(
          UNestedAbstraction(asymlist, nb, body), pos) ->
          let body_exp = explicify_term body true false in
            AbstractionTerm(UNestedAbstraction(asymlist, nb, body_exp), pos)
      | AbstractionTerm(NestedAbstraction(_,_),_) ->
            failwith "Nested Abs"
      | _ -> term

(* Every "o" except the one in the target position is replaced by "list o".
* For instance (A -> o) -> o is transformed into (A -> list o) -> o *)
let explicify_const_ty const = 
  let rec o_to_list_o ty = 
    match ty with
      | ApplicationType(t, []) when t = kbool ->
          ApplicationType(klist, [o_type])
      | ArrowType(left, right) -> 
          let left' = o_to_list_o left in
          let right' = o_to_list_o right in
            makeArrowType right' [left']
      | ApplicationType(left, [t]) when t = o_type ->
          ApplicationType(left,
                          [ApplicationType(klist, [o_type])]
          )
      | ApplicationType(left, right) ->
          let right' = List.map o_to_list_o right in
          ApplicationType(left, right')
      | _ -> ty
  in

  let explicify_type ty = 
    let args = getArrowTypeArguments ty in
    let tgt = getArrowTypeTarget ty in
    let exp_args =  List.map o_to_list_o args in
      makeArrowType tgt exp_args
  in

    match const with
      | Constant(sym, fix, prec, expdef, use, nodefs, closed, typ_pres, red,
                skelref, typ_size, skel_need, need, code_info, const_ty, index,
                 pos) ->
          let skelopt = !skelref in 
            match skelopt with
              | None -> const 
              | Some(skel) ->
                  let ty = getSkeletonType skel in
                    if isArrowType ty then
                      let ty_exp = explicify_type ty in
                      let skel_exp = makeSkeleton ty_exp in
                        Constant(sym, fix, prec, expdef, use, nodefs, closed, 
                                 typ_pres, red, ref (Some(skel_exp)), typ_size, 
                                 skel_need, need, code_info, const_ty, index, 
                                 pos) 
                    else
                      const

let add_constants amod = 
  match amod with
    | Module(modname, modimps, modaccs, ctable, ktable, atable, astring,
                   gkinds, lkinds, gconsts, lconsts, hconsts, skels, hskels,
                   aclinfo ) ->
        let gconsts_exp = List.map explicify_const_ty gconsts in
        let gconsts_exp' = implies_constant::clause_constant::
                           fact_constant::forall_constant::gconsts_exp in
        Module(modname, modimps, modaccs, ctable, ktable, atable, astring,
                     gkinds, lkinds, gconsts_exp', lconsts, 
                     hconsts, skels, hskels, aclinfo)
    | _ -> amod


