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
(********************************************************************)
(* create a free variable on simulator heap                         *)
(********************************************************************)
let buildFreeVariable tysy index =
  let _ = 
	Simerrors.handleSimExceptions 
	  (Ccode_stubs.buildFreeVariable (Absyn.getTypeSymbolName tysy) index)
  in
  ()
  
(********************************************************************)
(* create a free type variable on simulator heap                    *)
(********************************************************************)
let buildFreeTypeVariable freeTypeVar index =
  let _ = 
	Simerrors.handleSimExceptions (Ccode_stubs.buildFreeTypeVariable index)
  in
  ()

(********************************************************************)
(* layout free term/type variables on simulator heap; associate     *)
(* each with an index                                               *)
(********************************************************************)
let layOutVariables vars buildFunc =
  let rec layOutVariablesAux vars index varIndexAssoc =
	match vars with
	  [] -> List.rev varIndexAssoc
	| (var :: rest) -> 
		buildFunc var index;
		layOutVariablesAux rest (index + 1) ((var, index) :: varIndexAssoc)
  in
  layOutVariablesAux vars 0 []


(*********************************************************************)
(* find out the index of a type/term free variable                   *)
(*********************************************************************)
let rec indexOf var varIndexList =
  match varIndexList with
	[] -> Errormsg.impossible Errormsg.none 
		"indexOf: cannot find variable index"
  | (var', index) :: rest ->
	  if (var == var') then index
	  else indexOf var rest


(*********************************************************************)
(*                        type creation                              *)
(*********************************************************************)
let buildType tyfvIndex tyExps = 
  let rec buildTypeAux tyExps =
	match tyExps with
	  [] -> ()
	| (tyExp :: rest) ->
		let tyExp' = Absyn.dereferenceType tyExp in
		let restTypes =
		  match tyExp' with
			Absyn.ArrowType(arg, target)      -> 
			  let _ = 
				Simerrors.handleSimExceptions (Ccode_stubs.buildArrowType ())
			  in
			  (rest @ [arg; target])
		  | Absyn.ApplicationType(kind, [])   -> 
			  let _ =
				Simerrors.handleSimExceptions 
				  (Ccode_stubs.buildSortType (Absyn.getKindIndex kind))
			  in
			  rest
		  | Absyn.ApplicationType(kind, args) ->
			  let _ =
				Simerrors.handleSimExceptions 
				  (Ccode_stubs.buildStrType (Absyn.getKindIndex kind) 
					 (List.length args))
			  in
			  (rest @ args) 
		  | Absyn.TypeVarType(_) -> (* type variable *)
			  let _ =
				Simerrors.handleSimExceptions 
				  (Ccode_stubs.buildFreeVarType (indexOf tyExp' tyfvIndex))
			  in
			  rest
		  | _ -> Errormsg.impossible Errormsg.none 
				 "buildType: invalid type structure"
		in
		buildTypeAux restTypes
  in
  buildTypeAux tyExps
  
(*********************************************************************)
(*                         term creation                             *)
(*********************************************************************)


let buildTerm fvIndex tyfvIndex terms =
  let rec buildTermAux terms types =
	match terms with
	  [] -> buildType tyfvIndex types 
    | (term :: rest) ->
	  let (restTerms, restTypes) =
		match term with
		  Absyn.IntTerm(i, _)             -> 
			let _ =
			  Simerrors.handleSimExceptions (Ccode_stubs.buildIntTerm i) 
			in
			(rest, types)
		| Absyn.RealTerm(f, _)            -> 
			let _ =
			  Simerrors.handleSimExceptions (Ccode_stubs.buildRealTerm f)
			in
			(rest, types)
		| Absyn.StringTerm(s, _)          -> 
			let _ =
			  Simerrors.handleSimExceptions 
				(Ccode_stubs.buildStringTerm (Absyn.getStringInfoString s))
			in
			(rest, types)
		| Absyn.ConstantTerm(c, [], _)    -> 
			let _ = 
			  if (Pervasive.isnilConstant c) then
				Simerrors.handleSimExceptions (Ccode_stubs.buildNilTerm ())
			  else 
				Simerrors.handleSimExceptions 
				  (Ccode_stubs.buildMConstantTerm (Absyn.getConstantIndex c))
			in
			(rest, types)
		| Absyn.ConstantTerm(c, tyenv, _) -> 
			let _ =
			  Simerrors.handleSimExceptions 
				(Ccode_stubs.buildPConstantTerm (Absyn.getConstantIndex c)
				(List.length tyenv))
			in
			(rest, types @ tyenv)
		| Absyn.FreeVarTerm(_)               ->
			let _ =
			  Simerrors.handleSimExceptions 
				(Ccode_stubs.buildFreeVarTerm 
				   (indexOf (Absyn.getTermFreeVariableTypeSymbol term) fvIndex))
			in
			(rest, types)
		| Absyn.BoundVarTerm(_)              ->
			let _ =
			  Simerrors.handleSimExceptions 
				(Ccode_stubs.buildDBTerm 
				   (Absyn.getTermBoundVariableDBIndex term))
			in
			(rest, types)
		| Absyn.AbstractionTerm(_)           ->
			let _ =
			  Simerrors.handleSimExceptions 
				(Ccode_stubs.buildAbstractionTerm 
				   (Absyn.getTermAbstractionNumberOfLambda term))
			in
			(rest @ [Absyn.getTermAbstractionBody term], types)
		| _ -> (* application *)
			let func = Absyn.getTermApplicationHead term in
			let args = Absyn.getTermApplicationArguments term in
			if (Absyn.isTermConstant func) && 
			   (Pervasive.isconsConstant (Absyn.getTermConstant func)) then
			  let _ = 
				Simerrors.handleSimExceptions (Ccode_stubs.buildConsTerm ())
			  in
			  (rest @ args, types)
			else
			  let _ =
				Simerrors.handleSimExceptions 
				  (Ccode_stubs.buildApplicationTerm 
					 (Absyn.getTermApplicationArity term))
			  in
			  (rest @ (func :: args), types)
	  in
	  buildTermAux restTerms restTypes
  in
  buildTermAux terms []

(*********************************************************************)
(* processing term and type structures to calculate the sizes of term*)
(* and type queues needed for C part of term/type construction.      *)
(*********************************************************************)
let termAndTypeSize tm ty =

  let rec typeNodes tys numNodes =
	match tys with
	  []           -> numNodes
	| (ty :: rest) ->
		match Absyn.dereferenceType ty with
		  Absyn.ApplicationType(kind, args) ->
			typeNodes (rest @ args) (numNodes + (List.length args))
		| Absyn.ArrowType(arg, target)      ->
			typeNodes (rest @ [arg ; target]) (numNodes + 2)
		| Absyn.TypeVarType(_) -> (* free type variable *)
			typeNodes rest numNodes
		| _ -> Errormsg.impossible Errormsg.none 
			   "getTermAndTypeSize: invalid type structure"
  in
  
  let rec termNodes tms numTmNodes numTyNodes =
	match tms with 
	  [] -> (numTmNodes, numTyNodes)
	| (tm :: rest) ->
		match tm with
		  Absyn.ConstantTerm(c, tyenv, _)     ->
			termNodes rest numTmNodes 
			  ((typeNodes tyenv (List.length tyenv)) + numTyNodes)
		| Absyn.AbstractionTerm(_)               ->
			termNodes (rest @ [Absyn.getTermAbstractionBody tm])
			  (numTmNodes + 1) numTyNodes
		| Absyn.ApplicationTerm(_)               ->
			let func = Absyn.getTermApplicationHead tm in
			let args  = Absyn.getTermApplicationArguments tm in
			let arity = Absyn.getTermApplicationArity tm in
			if (Absyn.isTermConstant func) && 
		       (Pervasive.isconsConstant (Absyn.getTermConstant func)) then
			  termNodes (rest @ args) (numTmNodes + arity) numTyNodes
			else
			  termNodes (rest @ (func :: args)) (numTmNodes + arity + 1)
				numTyNodes
		| _ -> termNodes rest numTmNodes numTyNodes
  in
  termNodes [tm] 1 (typeNodes [ty] 1)

(********************************************************************)
(*                       interface function                         *)
(********************************************************************)
(* NG: No longer in use, since queries are now compiled *)
(* let readTermAndType tm tymol fvars tyfvars =
 *     let getTypeMoleculeType tymol =
 *     let rec getTypeMoleculeTypeAux tyenv tyskel =
 *       match (Absyn.dereferenceType tyskel) with
 *         Absyn.SkeletonVarType(ind) -> List.nth tyenv (!ind)
 *       | Absyn.ArrowType(l, r) ->
 *           Absyn.ArrowType(getTypeMoleculeTypeAux tyenv l, 
 *             getTypeMoleculeTypeAux tyenv r)
 *       | Absyn.ApplicationType(k, args) ->
 *           Absyn.ApplicationType(k,
 *             List.map (getTypeMoleculeTypeAux tyenv) args)
 *       | _ ->
 *           Errormsg.impossible Errormsg.none 
 *             "Readterm.getTypeMoleculeType: invalid type skeleton"
 *     in
 *     
 *     let tyskel = Types.getMoleculeType tymol in
 *     let tyenv  = Types.getMoleculeEnvironment tymol in
 *     getTypeMoleculeTypeAux tyenv tyskel
 *   in
 *   let ty = getTypeMoleculeType tymol in
 *   let (numTmNodes, numTyNodes) = termAndTypeSize tm ty in
 *   let _ = 
 *     Simerrors.handleSimExceptions 
 *       (Ccode_stubs.initLocalTabs (List.length fvars) (List.length tyfvars)  
 * 	 numTmNodes numTyNodes)
 *   in
 *   let fvIndexes = layOutVariables fvars buildFreeVariable in
 *   let tyfvIndexes = layOutVariables tyfvars buildFreeTypeVariable in
 *   buildType tyfvIndexes [ty];
 *   buildTerm fvIndexes tyfvIndexes [tm];
 *   Ccode_stubs.setQueryFreeVariables 0; (\* argument not needed *\)
 *   Ccode_stubs.cleanLocalTabs () *)

(* ntyfvars is the total number of distinct free type variables in 
 * the types of the free variables. 
 * ie. for input variables
 *    X : A -> B
 *    Y : int -> (B -> C) -> D
 *    Z : D -> E
 * the number of free type variables is 5.
 * This is the number of type variable registers that need to be initialized.
 *)
let initVariables fvars ntyfvars =
  let _ =
    Simerrors.handleSimExceptions
      (Ccode_stubs.initLocalTabsQuery (List.length fvars)) in
  let _ = layOutVariables fvars buildFreeVariable in
  (* TODO: It might be nicer to use this instead of building them in query_c:solveQuery *)
  (* let _ = layOutVariables tyfvars buildFreeTypeVariable in *)
  (* This sets the number of variables in IO_freeVarTab,
   * as well as the number of free type variables that need to be initialized. *)
  Ccode_stubs.setQueryFreeVariables ntyfvars;
  Ccode_stubs.cleanLocalTabs ()
