(********************************************************************)
(* create a free variable on simulator heap                         *)
(********************************************************************)
let buildFreeVariable tysy index =
  Readtermutil.buildFreeVariable 
	(Absyn.getTypeSymbolName tysy) index
  
(********************************************************************)
(* create a free type variable on simulator heap                    *)
(********************************************************************)
let buildFreeTypeVariable freeTypeVar index =
  Readtermutil.buildFreeTypeVariable index
 
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
		let restTypes =
		  match tyExp with
			Absyn.ArrowType(arg, target)      -> 
			  Readtermutil.buildArrowType ();
			  (rest @ [arg; target])
		  | Absyn.ApplicationType(kind, [])   -> 
			  Readtermutil.buildSortType (Absyn.getKindIndex kind);
			  rest
		  | Absyn.ApplicationType(kind, args) ->
			  Readtermutil.buildStrType (Absyn.getKindIndex kind) 
				(List.length args);
			  (rest @ args) 
		  | _ -> (* type variable *)
			  Readtermutil.buildFreeVarType (indexOf tyExp tyfvIndex);
			  rest
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
		  Absyn.IntTerm(i, _, _)             -> 
			Readtermutil.buildIntTerm i;
			(rest, types)
		| Absyn.RealTerm(f, _, _)            -> 
			Readtermutil.buildRealTerm f;
			(rest, types)
		| Absyn.StringTerm(s, _, _)          -> 
			Readtermutil.buildStringTerm (Absyn.getStringInfoString s);
			(rest, types)
		| Absyn.ConstantTerm(c, [], _, _)    -> 
			(if (Pervasive.isnilConstant c) then Readtermutil.buildNilTerm ()
			else Readtermutil.buildMConstantTerm (Absyn.getConstantIndex c));
			(rest, types)
		| Absyn.ConstantTerm(c, tyenv, _, _) -> 
			Readtermutil.buildPConstantTerm (Absyn.getConstantIndex c) 
			  (List.length tyenv);
			(rest, types @ tyenv)
		| Absyn.FreeVarTerm(_)               ->
			Readtermutil.buildFreeVarTerm 
			  (indexOf (Absyn.getTermFreeVariableTypeSymbol term) fvIndex);
			(rest, types)
		| Absyn.BoundVarTerm(_)              ->
			Readtermutil.buildDBTerm (Absyn.getTermBoundVariableDBIndex term);
			(rest, types)
		| Absyn.AbstractionTerm(_)           ->
			Readtermutil.buildAbstractionTerm 
			  (Absyn.getTermAbstractionNumberOfLambda term);
			(rest @ [Absyn.getTermAbstractionBody term], types)
		| _ -> (* application *)
			let func = Absyn.getTermApplicationFunc term in
			let args = Absyn.getTermApplicationArgs term in
			if (Absyn.isTermConstant func) && 
			   (Pervasive.isconsConstant (Absyn.getTermConstant func)) then
			  (Readtermutil.buildConsTerm ();
			   (rest @ args, types))
			else 
			  (Readtermutil.buildApplicationTerm 
				 (Absyn.getTermApplicationArity term);
			   (rest @ (func :: args), types))
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
		match ty with
		  Absyn.ApplicationType(kind, [])   -> typeNodes rest numNodes
		| Absyn.ApplicationType(kind, args) ->
			typeNodes (rest @ args) (numNodes + (List.length args))
		| Absyn.ArrowType(arg, target)      ->
			typeNodes (rest @ [arg ; target]) (numNodes + 2)
		| _ -> (* free type variable *)
			typeNodes rest numNodes
  in
  
  let rec termNodes tms numTmNodes numTyNodes =
	match tms with 
	  [] -> (numTmNodes, numTyNodes)
	| (tm :: rest) ->
		match tm with
		  Absyn.ConstantTerm(c, [], _, _)        -> 
			termNodes rest numTmNodes numTyNodes
		| Absyn.ConstantTerm(c, tyenv, _, _)     ->
			termNodes rest numTmNodes 
			  ((typeNodes tyenv (List.length tyenv)) + numTyNodes)
		| Absyn.AbstractionTerm(_)               ->
			termNodes (rest @ [Absyn.getTermAbstractionBody tm])
			  (numTmNodes + 1) numTyNodes
		| Absyn.ApplicationTerm(_)               ->
			let func = Absyn.getTermApplicationFunc tm in
			let args  = Absyn.getTermApplicationArgs tm in
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
(*                       interface function  (to be changed)        *)
(********************************************************************)
let myReadTermAndType tm ty fvs tyfvs =
  let (numTmNodes, numTyNodes) = termAndTypeSize tm ty in
  Readtermutil.initLocalTabs (List.length fvs) (List.length tyfvs)  
	numTmNodes numTyNodes;
  let fvIndexes = layOutVariables fvs buildFreeVariable in
  let tyfvIndexes = layOutVariables tyfvs buildFreeTypeVariable in
  buildType tyfvIndexes [ty];
  buildTerm fvIndexes tyfvIndexes [tm];
  Readtermutil.cleanLocalTabs () 



	
