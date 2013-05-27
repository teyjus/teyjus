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

let tyskel = 
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

let clause_constant =
  let symbol = Symbol.symbol "clause" in
  let fixity = Infixl in
  let prec = 0 in 
  let exp_def = false in
  let use_only = false in
  let tenv_size = 0 in 
  let skel =  tyskel in
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
            | ApplicationType(kbool, []) ->
                embed_terms_in_list [ct] 
            | _ -> 
                ct
        end
    | _ -> term 


(* val explicify_term : Absyn.aterm  ->  Absyn.aterm  *)
let rec explicify_term clause = 
    match clause with
      | ApplicationTerm(
          FirstOrderApplication(
            ConstantTerm(const, typ_list, pos_const), 
            body::[head], _), pos) 
          when const = Pervasive.implConstant ->
        let f' = makeConstantTerm clause_constant typ_list pos_const in
        let body_exp = explicify_term body in
        let body_list = flatten_ands body_exp in
        let body_list_term = embed_terms_in_list body_list in 
        let head_exp = explicify_term head in
          ApplicationTerm(
            FirstOrderApplication(f', [head_exp; body_list_term], 
                                  List.length body_list+1), 
            pos)
      | ApplicationTerm(
          FirstOrderApplication(
            ConstantTerm(const, typ_list, pos_cons), 
            [arg], nbargs), pos) ->
          (* When a predicate has a single argument it may be because:
           * - it is really simply a single argument 
           * - this is the representation of a conjunction *)
          let exp_arg =  explicify_term arg in
          let flat_args = flatten_ands exp_arg in 
            begin
              match flat_args with 
                | [single] -> 
                    (* No ands.
                    *  However, we may still to need this unique argument in 
                    *  a list if it is of type "o" *)
                    let single' = explicify_const single in
                    ApplicationTerm(
                      FirstOrderApplication(
                        ConstantTerm(const, typ_list, pos_cons),
                        [single'], 1), pos)
                | _ -> 
                    (* Ands *)
                    let flat_args_list = embed_terms_in_list flat_args in
                    ApplicationTerm(
                      FirstOrderApplication(
                        ConstantTerm(const, typ_list, pos_cons),
                        [flat_args_list], 1), pos)
            end
      | ApplicationTerm(
          FirstOrderApplication(
            ConstantTerm(const, typ_list, pos_cons), 
            args, nbargs), pos) ->
          (* Many arguments thus no conjuncts at the arguments level *)
          let exp_args = List.map explicify_term args in
          (* Each argument may have to be transformed as a list singleton if 
           * it is of type "o" *)
          let exp_args' = 
            if isGlobalConstant const || isLocalConstant const then 
              List.map explicify_const exp_args 
            else
              (* We do not want to interfere with pervasive constants *)
              exp_args
          in
            ApplicationTerm(
              FirstOrderApplication(
                ConstantTerm(const, typ_list, pos_cons),
                exp_args', nbargs), pos)
      | AbstractionTerm(
          UNestedAbstraction(asymlist, nb, body), pos) ->
          let body_exp = explicify_term body in
            AbstractionTerm(UNestedAbstraction(asymlist, nb, body_exp), pos)
      | AbstractionTerm(NestedAbstraction(_,_),_) ->
            failwith "Nested Abs"
      | _ -> clause

(* Every "o" except the one in the target position is replaced by "list o".
* For instance (A -> o) -> o is transformed into (A -> list o) -> o *)
let explicify_const_ty const = 
  let rec o_to_list_o ty = 
    match ty with
      | ApplicationType(kbool, []) ->
          ApplicationType(klist, [ApplicationType(kbool, [])])
      | ArrowType(left, right) as ar -> 
          let left' = o_to_list_o left in
          let right' = o_to_list_o right in
            makeArrowType right' [left']
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
    | Absyn.Module(modname, modimps, modaccs, ctable, ktable, atable, astring,
                   gkinds, lkinds, gconsts, lconsts, hconsts, skels, hskels,
                   aclinfo ) ->
        let gconsts_exp = List.map explicify_const_ty gconsts in
        Absyn.Module(modname, modimps, modaccs, ctable, ktable, atable, astring,
                     gkinds, lkinds, clause_constant::gconsts_exp, lconsts, 
                     hconsts, skels, hskels, aclinfo)
    | _ -> amod


