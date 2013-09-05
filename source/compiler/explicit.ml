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


(* Turn a lambda prolog file into an equivalent one where clauses,
 * implications, and quantifications are explicit 
 * Since this transformation comes after some simplifications we can assume
 * that all the clauses    P :- Q    do not contain any implication or comma 
 * in P.
*)

(* TODO:
 * Have a nice translation for 
 type foo A -> B -> o.
 type bar A -> o.


 foo A  B :- bar (A,B).
 bar A :- A, true. 
Currently variables are embeded inside lists whereas it seems that 
should not be the case.
*)



(* Some kinds needed for the constant we introduce *)
let o_type = Absyn.makeKindType Pervasive.kbool
let list_type = Absyn.makeKindType Pervasive.klist

(* :- *)
let clause_constant =
  let tyskel_clause = 
    Skeleton(
      ArrowType(
        ApplicationType(Pervasive.kbool, []), 
        ArrowType(
          ApplicationType(Pervasive.klist, 
                          [ApplicationType(Pervasive.kbool, [])]), 
          ApplicationType(Pervasive.kbool, [])
        )
      ), 
      ref None, 
      ref false) in
  let symbol = Symbol.symbol "clause" in
  let fixity = Infixl in
  let prec = 0 in 
  let exp_def = false in
  let use_only = false in
  let tenv_size = 0 in 
  let skel =  tyskel_clause in
  let index = 0 in 
    makeGlobalConstant symbol fixity prec exp_def 
      use_only tenv_size skel index 

(* => *)
let implies_constant =
  let tyskel_implies = 
    Skeleton(
      ArrowType(
        ApplicationType(Pervasive.klist, 
                        [ApplicationType(Pervasive.kbool, [])]), 
        ArrowType(
          ApplicationType(Pervasive.klist, 
                          [ApplicationType(Pervasive.kbool, [])]), 
          ApplicationType(Pervasive.kbool, [])
        )
      ), 
      ref None, 
      ref false) in
  let symbol = Symbol.symbol "implies" in
  let fixity = Infixr in
  let prec = 130 in 
  let exp_def = false in
  let use_only = false in
  let tenv_size = 0 in 
  let skel =  tyskel_implies in
  let index = 0 in 
    makeGlobalConstant symbol fixity prec exp_def 
      use_only tenv_size skel index 

(* This a lambda prolog fact *)
let fact_constant =
  let tyskel_fact = 
    Skeleton(
      ArrowType(
        ApplicationType(Pervasive.kbool, []), 
        ApplicationType(Pervasive.kbool, [])),
      ref None, 
      ref false) in
  let symbol = Symbol.symbol "fact" in
  let fixity = NoFixity in
  let prec = 0 in 
  let exp_def = false in
  let use_only = false in
  let tenv_size = 0 in 
  let skel =  tyskel_fact in
  let index = 0 in 
    makeGlobalConstant symbol fixity prec exp_def 
      use_only tenv_size skel index 

(* pi x\ ... *) 
let forall_constant =
  let tyskel_forall = 
    Skeleton(
      ArrowType(
        ArrowType(
          SkeletonVarType((ref 0)), 
          ApplicationType(Pervasive.klist, 
                          [ApplicationType(Pervasive.kbool, [])])
        ), 
        ApplicationType(Pervasive.kbool, [])
      ),
      ref None, 
      ref false) in
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


let explicit_constants = [clause_constant; implies_constant; fact_constant; 
                          forall_constant]



(* Given a (OCaml) list of aterm, transform them into an aterm list  *)
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

let is_term_a_list term = 
  match term with 
    | Absyn.ConstantTerm(n, _, _) when n = Pervasive.nilConstant -> true 
    | Absyn.ApplicationTerm(
        FirstOrderApplication(Absyn.ConstantTerm(cons,_, _), _, _ ),_) 
        when cons = Pervasive.consConstant-> true
    | _ -> false

(* A lambda prolog declaration p1, p2, ..., pn is parsed in
* abstract syntax as (p1, (p2 , ...(pn-1, ))) with , a binary connective 
* This function transforms this term into a list of terms *)
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

(* Make a constant term in an argument position explicit *)
let explicit_const term = 
  match term with
    | ConstantTerm(const, tys, pos) as ct -> 
        begin
        let skel = getConstantSkeletonValue const in
        let ty = getSkeletonType skel in
          match ty with
            | ApplicationType(t, []) when t = Pervasive.kbool ->
                embed_terms_in_list [ct] 
            | _ -> 
                ct
        end
    | _ -> term 


let rec explicit_term term add_sing top_level = 
    match term with
      (* :- *)
      | ApplicationTerm(
          FirstOrderApplication(
            ConstantTerm(const, typ_const, pos_const), 
            body::[head], _), pos) 
          when const = Pervasive.implConstant ->
        let body_flat = flatten_ands body  in
        let body_exp = List.map 
                         (fun x -> explicit_term x false false) body_flat in
        let head_exp = explicit_term head false false in
        let body_exp_list = embed_terms_in_list body_exp in
          if top_level then
            ApplicationTerm(
              FirstOrderApplication(
                ConstantTerm(clause_constant, typ_const, pos_const), 
                [head_exp; body_exp_list], 2), 
              pos) 
          else
            let head_exp' = 
              if not (is_term_a_list head_exp) then
                embed_terms_in_list [head_exp]
              else
                head_exp 
            in
            let term_exp = ApplicationTerm(
              FirstOrderApplication(
                ConstantTerm(implies_constant, typ_const, pos_const), 
                [body_exp_list; head_exp'], 2), 
              pos) in
              if add_sing then
                embed_terms_in_list [term_exp]
              else
                term_exp


      (* , conjunction not the list separator *)
      | ApplicationTerm(
          FirstOrderApplication(
            ConstantTerm(const, typ_const, pos_const), 
            args, _), pos) 
          when const = Pervasive.andConstant ->
          let term_flat = flatten_ands term  in
          let term_flat_exp = 
            List.map (fun x -> explicit_term x false false) term_flat in
            embed_terms_in_list term_flat_exp 

      (* pi *)
      | ApplicationTerm(
          FirstOrderApplication(
            ConstantTerm(const, typ_const, pos_const), 
            args, nb_args), pos) 
          when const = Pervasive.allConstant ->
          let args_exp = List.map (fun x -> explicit_term x true false) args in
          let term_exp = 
            ApplicationTerm(
              FirstOrderApplication(
                ConstantTerm(forall_constant, typ_const, pos_const), 
                args_exp, List.length args_exp), pos)  in
              term_exp

      (* Any other predicate *)
      | ApplicationTerm(
          FirstOrderApplication(
            ConstantTerm(const, typ_const, pos_cons), 
            args, nbargs), pos) ->
          let exp_args = 
            List.map (fun x -> explicit_term x true false) args in
          let term_exp  = 
            ApplicationTerm(
              FirstOrderApplication(
                ConstantTerm(const, typ_const, pos_cons),
                exp_args, List.length exp_args), pos) in
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
                (* This predicate appears in the body of a clause *)
                if add_sing && 
                   (cons_ty = o_type || 
                    (isArrowType cons_ty && 
                     (getArrowTypeTarget cons_ty = o_type))) then
                    embed_terms_in_list [term_exp] 
                else
                  term_exp

      | ConstantTerm(_, _, _)  as ct when add_sing -> explicit_const ct
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
          let body_exp = explicit_term body true false in
            AbstractionTerm(UNestedAbstraction(asymlist, nb, body_exp), pos)
      | AbstractionTerm(NestedAbstraction(_, _), _) ->
            failwith "Nested Abs"
      | FreeVarTerm(NamedFreeVar(ImplicitVar(sym, hc, new_ty, typ_opt)), p) ->
          (match !typ_opt with
            | Some(ty) when ty = o_type && add_sing -> 
                embed_terms_in_list [term]
            | _ ->
                term)
      | _ -> term

let explicit_const_ty const = 
  let rec o_to_list_o ty = 
    match ty with
      | ArrowType(left, right) when right = o_type -> 
          (* By doing so, we cannot handle cases like:
           * type p (A -> o) -> A -> o.
           * p (x\ true, true) true.
           *
           * However this kind of situation seems to do not appear 
           * in practice.
           * In general for such a type we have:
           * p X Y :- X Y. 
           * which we can handle*)
          let left' = o_to_list_o left in
            makeArrowType right [left']
      | ArrowType(left, right) -> 
          let left' = o_to_list_o left in
          let right' = o_to_list_o right in
            makeArrowType right' [left']
      | ApplicationType(t, []) when t = Pervasive.kbool ->
          ApplicationType(Pervasive.klist, [o_type])
      | ApplicationType(left, [t]) when t = o_type ->
          ApplicationType(left,
                          [ApplicationType(Pervasive.klist, [o_type])]
          )
      | ApplicationType(left, right) ->
          let right' = List.map o_to_list_o right in
            ApplicationType(left, right')
      | _ -> ty
  in

  let explicit_type ty = 
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
                      let ty_exp = explicit_type ty in
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
        let gconsts_exp = List.map explicit_const_ty gconsts in
        let lconsts_exp = List.map explicit_const_ty lconsts in
        let hconsts_exp = List.map explicit_const_ty !hconsts in
        let gconsts_exp' = implies_constant::clause_constant::
                           fact_constant::forall_constant::gconsts_exp in
        Module(modname, modimps, modaccs, ctable, ktable, atable, astring,
                     gkinds, lkinds, gconsts_exp', lconsts_exp, 
                     ref hconsts_exp, skels, hskels, aclinfo)
    | _ -> amod



let interpreter_mod = "
/********* The interpreter *************/ 
type in A -> list A -> o.
in X (X::L).
in X (Y::L) :- in X L.

type append list A -> list A -> list A -> o.
append nil K K.
append (X::L) K (X::M) :- append L K M.



/* Entry point */
exrun X :- run' [] X.

type run' list o -> o -> o.
run' Env X :- fact X ; in X Env.
run' Env X :- 
    % term_to_string X F, print \"Trying to solve \", print F, print \"\\n\",
    (X clause Y), 
    % term_to_string (X clause Y) Cl,
    % print \" with clause: \\n   \", print Cl, print \"\\n\",
    run_body Env Y. 


type run_body list o -> list o -> o.
run_body _ [].
run_body Env ((A implies B)::L) :- 
    % term_to_string A As, %    term_to_string B Bs,
    % print \"Proving  the goal(s) \", print Bs, 
    % print \" under the new assumption(s) \", print As, print \"\\n\",
    append A Env NewEnv,
    run_body NewEnv B, 
    run_body Env L.
run_body Env ((pi' X\\ Y X)::L) :- 
    (pi X\\ run_body Env (Y X)), 
    run_body Env L.
run_body Env (X::L) :- 
    term_to_string X Xs,
    % print \"Proving the atomic goal \", print Xs, print \"\\n\",
    run' Env X, 
    run_body Env L.
/************             *************/"

let interpreter_sig = "type exrun o -> o."



