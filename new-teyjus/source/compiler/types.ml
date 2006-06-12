type pos = Errormsg.pos

type typemolecule =
	Molecule of (Absyn.atype * Absyn.atype list * bool)

type typeandbindings =
	TypeAndBindings of (Absyn.atype * Absyn.atype Table.SymbolTable.t)

(*	TypeAndBindings Accessors	*)
let getTypeAndBindingsType =
	fun t ->
		(match t with
			TypeAndBindings(t, _) -> t)		
let getTypeAndBindingsBindings = function
	TypeAndBindings(_, bs) -> bs

type typeandenvironment =
	TypeAndEnvironment of (Absyn.atype * int * bool)

(*	TypeAndEnvironment Accessors	*)
let getTypeAndEnvironmentType = function
	TypeAndEnvironment(t, _, _) -> t
		
let getTypeAndEnvironmentSize = function
	TypeAndEnvironment(_, i, _) -> i

let getTypeAndEnvironmentPreserving = function 
	TypeAndEnvironment(_, _, b) -> b

type argstypes =
	ArgsTypes of (int * Absyn.atype list * Absyn.atype list)

(*	Unification results	*)
type unifyresult =
		OccursCheckFailure
	|	ClashFailure
	|	Success

let typeSkeletonIndex = ref 0

(**********************************************************************
*rationalizeTypeAbbrevVar:
*	Used when translating a type variable while translating a typeabbrev
* into absyn.
**********************************************************************)
let rationalizeTypeAbbrevVar =
	fun sym symtable p ->
		(Errormsg.error p ("unbound variable in type abbreviation");
		TypeAndBindings(Absyn.ErrorType, symtable))

(**********************************************************************
*rationalizeSkeletonVar:
*	Used when translating a type variable while translating a type into
*	a type skeleton. 
**********************************************************************)
let rationalizeSkeletonVar =
	fun sym symtable p ->
		let t = Absyn.SkeletonVarType(!typeSkeletonIndex) in
		(typeSkeletonIndex := !typeSkeletonIndex + 1;
		TypeAndBindings(t, (Table.add sym t symtable)))

(**********************************************************************
*rationalizeVar:
*	Used when translating a type variable while translating a type into
*	an absyn type.
**********************************************************************)
let rationalizeVar =
	fun sym symtable p ->
		let t = Absyn.TypeVarType(None, false) in
		TypeAndBindings(t, (Table.add sym t symtable))

(********************************************************************
*rationalizeType:
*********************************************************************)
let rec rationalizeType = fun
	ty vartable kindtable typeabbrevtable newsymfunc transvarfunc ->
		
		(******************************************************************
		*translateArrow:
		*	Translate an arrow from preabsyn to absyn.
		******************************************************************)
		let rec translateArrow = function Preabsyn.Arrow(l,r,p) ->
			let TypeAndBindings(l', ls) = rationalizeType l vartable kindtable typeabbrevtable newsymfunc transvarfunc in
			let TypeAndBindings(r', rs) = rationalizeType r ls kindtable typeabbrevtable newsymfunc transvarfunc in

			TypeAndBindings(Absyn.ArrowType(l', r'), rs)
		in
	
		(******************************************************************
		*translateApp:
		*	Translate an application from preabsyn to absyn.
		******************************************************************)
		let rec translateApp = function Preabsyn.App(f,t,p) ->
			(**************************************************************
			*translateArgs:
			*	Gets the arguments as a list instead of a tree.
			**************************************************************)
			let rec translate' = fun t ts ->
				match t with
					Preabsyn.App(f,arg,p') ->
						let TypeAndBindings(argtype, ts') = rationalizeType arg ts kindtable typeabbrevtable newsymfunc transvarfunc in
						let (head, ts'', argtypes) = (translate' f ts') in
						(head, ts'', argtype :: argtypes)
				|	_ -> (t, ts, [])
			in

			let (head, ts, args) = translate' ty vartable in
			
			(match head with
				Preabsyn.Atom(sym,k,p) ->
						(match k with
							Preabsyn.VarID ->
								(Errormsg.error p "found type variable, expected a constructor";
								TypeAndBindings(Absyn.ErrorType, vartable))
						|	Preabsyn.AVID ->
								(Errormsg.error p "found type variable, expected a constructor";
								TypeAndBindings(Absyn.ErrorType, vartable))
						|	Preabsyn.CVID ->
								(Errormsg.error p "found type variable, expected a constructor";
								TypeAndBindings(Absyn.ErrorType, vartable))
						| _ ->
							(match (Table.find sym vartable) with
								Some t ->
									(Errormsg.error p "found type variable, expected a constructor";
									TypeAndBindings(Absyn.ErrorType, vartable))
							|	None ->
									(match (Table.find sym kindtable) with
										Some k ->
											if (Absyn.getKindArity k) <> (List.length args) then
												(Errormsg.error p ("type constructor has arity " ^ (string_of_int (Absyn.getKindArity k)) ^ ", but given " ^ (string_of_int (List.length args)) ^ " arguments");
												TypeAndBindings(Absyn.ErrorType, vartable))
											else
												TypeAndBindings(Absyn.AppType(k, args), ts)
									|	None ->
												(match (Table.find sym typeabbrevtable) with
													Some t ->
														(translateTypeAbbrevCall t args ts p)
												|	None ->
													(Errormsg.error p ("undefined constructor " ^ (Symbol.name sym));
													TypeAndBindings(Absyn.ErrorType, vartable))))))
			| _ ->
				(Errormsg.error p "expected a constructor");
				TypeAndBindings(Absyn.ErrorType, vartable))
		in	
		match ty with
			Preabsyn.Atom(s, Preabsyn.AVID, pos) ->
				(transvarfunc s vartable pos)
		|	Preabsyn.Atom(s, Preabsyn.VarID, pos) ->
				(**************************************************************
				*	If the variable is in the variable table,
				*	just return the type associated with it.  Otherwise,
				*	create a new one.
				**************************************************************)
				(match (Table.find s vartable) with
					Some t -> TypeAndBindings(t, vartable)
				|	None -> transvarfunc s vartable pos)
		|	Preabsyn.Atom(s, Preabsyn.CVID, pos) ->
				(match (Table.find s vartable) with
					Some t -> TypeAndBindings(t, vartable)
				|	None -> transvarfunc s vartable pos)
		|	Preabsyn.Atom(s, _, pos) ->
				(match (Table.find s kindtable) with
					Some k ->
						if (Absyn.getKindArity k) <> 0 then
							(Errormsg.error pos ("type constructor has arity " ^ (string_of_int (Absyn.getKindArity k)));
							TypeAndBindings(Absyn.ErrorType, vartable))
						else
							TypeAndBindings(Absyn.AppType(k, []), vartable)
				|	None ->
					(Errormsg.error pos ("undefined symbol " ^ (Symbol.name s));
					TypeAndBindings(Absyn.ErrorType, vartable)))
				
		|	Preabsyn.App(f, t, p) ->
				translateApp ty

		|	Preabsyn.Arrow(l, r, p) ->
				translateArrow ty

		|	Preabsyn.ErrorType -> TypeAndBindings(Absyn.ErrorType, vartable)

(**********************************************************************
*translateType:
*	Translate a preabsyn representation of a type into an absyn type.
**********************************************************************)
and translateType = fun ty kindtable typeabbrevtable ->
	let newSymFunc = fun () -> () in
	let TypeAndBindings(t, ts) = rationalizeType ty Table.SymbolTable.empty kindtable typeabbrevtable newSymFunc rationalizeVar in
	t

(**********************************************************************
*translateTypeSkeleton:
*	Translate a preabsyn representation of a type into an absyn type
*	skeleton.
**********************************************************************)
and translateTypeSkeleton = fun ty kindtable typeabbrevtable newsymfunc ->

	(*********************************************************************
	*translateArrow:
	*	Translate an arrow, with a check to ensure 
	*********************************************************************)
	let translateArrow = function Preabsyn.Arrow(l,r,pos) ->

		(*******************************************************************
		*getTarget:
		*	Get the target of an arrow type.
		*******************************************************************)
		let rec getTarget = fun t ->
			match t with
				Preabsyn.Arrow(l,r,p) -> getTarget r
			|	_ -> t
		in
		
		(*******************************************************************
		*getArgs:
		*	Get the arguments of an arrow type in list form.
		*******************************************************************) 
		let rec getArgs = fun t ->
			match t with
				Preabsyn.Arrow(l, r, p) -> l::(getArgs r)
			|	_ -> []
		in
		
		(*******************************************************************
		*translateArgs:
		*	Takes a list of arguments, and translates them.  Passes along the
		*	environment.
		*******************************************************************)
		let rec translateArgs = fun args ts ->
			match args with
				arg::rest ->
					let TypeAndBindings(t, ts') = rationalizeType arg ts kindtable typeabbrevtable newsymfunc rationalizeSkeletonVar in
					t::(translateArgs rest ts')
			|	[] -> []
		in
		
		(******************************************************************
		*buildArrow:
		*	Takes a 
		******************************************************************)
		let rec buildArrow = fun target args ->
			match args with
				arg::rest -> Absyn.ArrowType(arg, buildArrow target rest)
			| [] -> target
		in

		(*	Get the argument and target parts	*)
		let target = getTarget r in
		let args = getArgs ty in
		
		(*	First translate the target.  Store the skeleton index to check
				for type preservation.	*)
		let TypeAndBindings(target', ts) = rationalizeType target Table.SymbolTable.empty kindtable typeabbrevtable newsymfunc rationalizeSkeletonVar in
		let currentIndex = !typeSkeletonIndex in
		
		(*	Translate all of the arguments	*)
		let args' = translateArgs args ts in
		
		(*	Rebuild the arrow type as absyn	*)
		TypeAndEnvironment(buildArrow target' args', !typeSkeletonIndex, (currentIndex = !typeSkeletonIndex))
	in
	
	let _ = typeSkeletonIndex := 0 in
	
	match ty with
		Preabsyn.Arrow(l,r,pos) ->
			translateArrow ty
	|	_ ->
		let TypeAndBindings(t, ts) = rationalizeType ty Table.SymbolTable.empty kindtable typeabbrevtable newsymfunc rationalizeSkeletonVar in
		TypeAndEnvironment(t, !typeSkeletonIndex, true)	

(**********************************************************************
*translateFixities:
*	Translate a list of fixity declarations and a constant list into an
*	updated constant list.
**********************************************************************)
and translateFixities = fun fixities constants ->
	(********************************************************************
	*translate':
	*	Translate an individual fixity declaration.  Update the constant
	*	table.
	********************************************************************)
	let translate' = fun f ctable ->
		let rec addFixities = fun syms k prec pos ctable ->
			(****************************************************************
			*getFixity:
			*	Convert preabsyn fixity to absyn fixity.
			****************************************************************)
			let getFixity k =
				match k with
					Preabsyn.Infix(p) -> Absyn.Infix
				|	Preabsyn.Infixl(p) -> Absyn.Infixl
				|	Preabsyn.Infixr(p) -> Absyn.Infixr
				|	Preabsyn.Prefix(p) -> Absyn.Prefix
				|	Preabsyn.Prefixr(p) -> Absyn.Prefixr
				|	Preabsyn.Postfix(p) -> Absyn.Postfix
				|	Preabsyn.Postfixl(p) -> Absyn.Postfixl
			in
			
			let getFixityArity k =
				match k with
					Preabsyn.Infix(p) -> 2
				|	Preabsyn.Infixl(p) -> 2
				|	Preabsyn.Infixr(p) -> 2
				|	Preabsyn.Prefix(p) -> 1
				|	Preabsyn.Prefixr(p) -> 1
				|	Preabsyn.Postfix(p) -> 1
				|	Preabsyn.Postfixl(p) -> 1
			in
			
			let rec getTypeArity = function
				[Absyn.ArrowType(_,r)] -> 1 + (getTypeArity [r])
			|	[_] -> 0
			|	[] -> (Errormsg.impossible pos "getTypeArity(): invalid skeleton list")
			in
			
			match syms with
				[] -> ctable
			|	Preabsyn.Symbol(sym,_,pos)::ss ->
					(match Table.find sym ctable with
						Some Absyn.Constant(asym,fix,p,b1,b2,b3,s,i1,skellist,tlist,codeinfo,ctype,pos') ->
							if (getFixityArity k) <> (getTypeArity tlist) then
								(Errormsg.error pos ("declared fixity is incompatible with declared type arity" ^
									(Errormsg.see pos' "type declaration"));
								ctable)
							else if not (checkFixity fix (getFixity k)) then
								(Errormsg.error pos ("constant already declared with fixity " ^ (Absyn.string_of_fixity fix) ^
									(Errormsg.see pos' "type declaration"));
								ctable)
							else
								let ctable' = Table.remove sym ctable in
								(Table.add sym (Absyn.Constant(asym,(getFixity k),prec,b1,b2,b3,s,i1,skellist,tlist,codeinfo,ctype,pos)) ctable')
					|	None ->
							(Errormsg.error pos ("fixity declaration: undefined constant " ^
								(Symbol.name sym));
							ctable))
		in
		match f with
			Preabsyn.Fixity(syms, k, prec, pos) -> addFixities syms k prec pos ctable
	in
	
	match fixities with
		[] -> constants
	|	f::fs ->
			let constants' = translateFixities fs constants in
			(translate' f constants')

(**********************************************************************
*translateLocalKinds:
**********************************************************************)
and translateLocalKinds = fun kinds ->
	let kindIndex = ref 0 in
	let buildkind = fun sym arity pos ->
		let k = Absyn.LocalKind(sym, arity, Absyn.KindIndex(!kindIndex), pos) in
		(kindIndex := !kindIndex + 1;
		k)
	in
	translateKinds kinds buildkind Table.SymbolTable.empty

(**********************************************************************
*translateGlobalKinds:
**********************************************************************)
and translateGlobalKinds = fun kinds ->
		let kindIndex = ref 0 in
		let buildkind = fun sym arity pos ->
			let k = Absyn.GlobalKind(sym, arity, Absyn.KindIndex(!kindIndex), pos) in
			(kindIndex := !kindIndex + 1;
			k)
		in
		translateKinds kinds buildkind Table.SymbolTable.empty

(**********************************************************************
*translateKinds:
*	Translate a list of kinds and return a symbol table.
**********************************************************************)
and translateKinds = fun klist buildkind kindtable-> 
	let rec translate' = fun klist ktable ->
		match klist with
			[] -> ktable
		|	k::ks ->
				let ktable' = (translateKind k buildkind ktable) in
				(translate' ks ktable')
	in
	translate' klist kindtable

(**********************************************************************
*translateKind:
**********************************************************************)
and translateKind = fun kind buildkind kindtable ->
	(********************************************************************
	*addKind:
	********************************************************************)
	let rec addKind = fun syms a pos ktable ->
		(match syms with
			[] -> ktable
		|	Preabsyn.Symbol(sym,_,_)::ss -> 
				let ktable' = Table.add sym (buildkind sym a pos) ktable in
				(addKind ss a pos ktable'))
	in
	
	match kind with
		Preabsyn.Kind(syms, a, pos) ->
			(addKind syms a pos kindtable)


and translateGlobalConstants = fun clist ctable kindtable typeabbrevtable ->
	let buildConstant = fun sym ty tyskel pres pos ->
		Absyn.Constant(sym, Absyn.NoFixity, -1, false, false, false, false, pres, tyskel, ty, Absyn.Clauses([]), Absyn.LocalConstant, pos)
	in
	translateConstants clist ctable kindtable typeabbrevtable buildConstant

and translateLocalConstants = fun clist ctable kindtable typeabbrevtable ->
	let buildConstant = fun sym ty tyskel pres pos ->
		Absyn.Constant(sym, Absyn.NoFixity, -1, false, false, false, false, pres, tyskel, ty, Absyn.Clauses([]), Absyn.LocalConstant, pos)
	in
	translateConstants clist ctable kindtable typeabbrevtable buildConstant

(********************************************************************
*translateConstants:
*	Translates a list of constant declarations in preabsyn
*	representation into a constant table.
********************************************************************)
and translateConstants = fun clist ctable kindtable typeabbrevtable buildconstant -> 
	let rec translate' = fun clist ctable ->
		match clist with
			c::cs ->
				let ctable' = translateConstant c ctable kindtable typeabbrevtable buildconstant in
				translate' cs ctable'
		|	[] -> ctable
	in
	translate' clist Table.SymbolTable.empty


(**********************************************************************
*translateConstant:
*	Translate a preabsyn constant into an absyn constant and enter it
*	into a table.
**********************************************************************)
and translateConstant = fun c ctable kindtable typeabbrevtable buildconstant ->
	(********************************************************************
	*translate':
	*	Enter all names into table.
	********************************************************************)
	let rec enter = fun names ty tyskel pres table ->
		match names with
			Preabsyn.Symbol(name,_,p)::ns ->
				let table' = (Table.add name (buildconstant name ty tyskel pres p) table) in
				(enter ns ty tyskel pres table')
		|	[] ->	table
	in
	
	let rec newSymFunc = fun () -> ()
	in

	match c with
		Preabsyn.Constant(names, Some t, pos) ->
			let TypeAndEnvironment(tyskel, size, pres) = translateTypeSkeleton t kindtable typeabbrevtable newSymFunc in
			let ty = translateType t kindtable typeabbrevtable in
			(enter names [ty] [Absyn.Skeleton(tyskel, size, pres)] pres ctable)
	|	Preabsyn.Constant(names, None, pos) ->
			(enter names [] [] false ctable)

(********************************************************************
*translateTypeAbbrevs:
*	Translates a list of type abbreviations in preabsyn representation
*	into a type abbreviation table.
********************************************************************)
and translateTypeAbbrevs = fun tabbrevs kindtable -> 
	let rec translate' = fun tlist abbrevtable ->
		match tlist with
			[] -> abbrevtable
		|	t::ts ->
				let abbrevtable' = translateTypeAbbrev t abbrevtable kindtable in
				translate' ts abbrevtable'
	in
	translate' tabbrevs Table.SymbolTable.empty

(********************************************************************
*translateTypeAbbrev:
*	Translate a type abbreviation from preabsyn to absyn.
********************************************************************)
and translateTypeAbbrev =
	fun abbrev abbrevtable kindtable ->
		let Preabsyn.TypeAbbrev(name, arglist, ty, pos) = abbrev in
		
		(****************************************************************
		*getName:
		****************************************************************)
		let getName = function
			Preabsyn.Symbol(n,Preabsyn.ConstID,p) -> n
		|	Preabsyn.Symbol(n,k,p) -> (Errormsg.error p "type abbreviation: expected abbreviation name";
																n)
		in
		
		(****************************************************************
		*checkArgs:
		****************************************************************)
		let rec checkArgs = function
			[] -> []
		|	Preabsyn.Symbol(n,Preabsyn.CVID,p)::ss -> n::(checkArgs ss)
		|	Preabsyn.Symbol(n,k,p)::ss -> (Errormsg.error p "type abbreviation: expected argument name";
																		[])
		in
		
		(****************************************************************
		*buildTable:
		****************************************************************)
		let rec buildTable = fun syms i ->
			match syms with
				[] -> Table.SymbolTable.empty
			|	sym::ss -> (Table.SymbolTable.add sym (Absyn.SkeletonVarType(i)) (buildTable ss (i + 1)))
		in
		
		(******************************************************************
		*newSymFunc:
		*	If a new symbol is encountered when rationalizing a type while
		*	parsing type abbreviations, it is an error.
		******************************************************************)
		let newSymFunc = fun () -> () 
		in

		
		(*	Get the name and arguments	*)
		let abbrevname = getName name in
		let args = checkArgs arglist in
		
		(*	Build a symbol table of the args	*)
		let symtable = buildTable args 0 in
		
		(*	Translate the type body	*)
		let TypeAndBindings(bodytype,bindings) = (rationalizeType ty symtable kindtable abbrevtable newSymFunc rationalizeTypeAbbrevVar) in
		
		(Table.add abbrevname (Absyn.TypeAbbrev(abbrevname, args, bodytype, pos)) abbrevtable)

(********************************************************************
*translateTypeAbbrevCall:
*	Given a variable table and arguments, instantiates a type abbrev.
********************************************************************)
and translateTypeAbbrevCall = fun abbrev args vartable pos ->
	let Absyn.TypeAbbrev(name, syms, target, pos') = abbrev in
	
	let rec replaceArg = fun argnum a t ->
		match t with
			Absyn.ArrowType(l,r) -> Absyn.ArrowType(replaceArg argnum a l, replaceArg argnum a r)
		|	Absyn.AppType(k,tlist) ->
				let tlist' = List.map (replaceArg argnum a) tlist in
				Absyn.AppType(k, tlist')
		|	Absyn.TypeVarType(_,_) -> t
		|	Absyn.SkeletonVarType(i) ->
				if i = argnum then
					a
				else
					t
		|	Absyn.TypeRefType(t') -> (replaceArg argnum a t')
		|	Absyn.ErrorType -> Absyn.ErrorType
	in
	
	(*	Replaces each argument placeholder with a real type	*)
	let rec replaceAll = fun argnum args target ->
		match args with
			arg::aa ->
				let target' = replaceArg argnum arg target in
				(replaceAll (argnum + 1) aa target')
		|	[] ->
				target
	in
	
	if (List.length syms) <> (List.length args) then
		(Errormsg.error pos ("typeabbrev expected " ^ (string_of_int (List.length syms)) ^ " arguments" ^
			(Errormsg.see pos' "typeabbrev declaration"));
		TypeAndBindings(Absyn.ErrorType, vartable))
	else
		TypeAndBindings((replaceAll 0 args target), vartable)

(********************************************************************
*getTargetType:
********************************************************************)
and getTargetType =
	fun t ->
		match t with
			Absyn.ArrowType(t', _) -> t'
		|	_ -> t

(********************************************************************
*dereferenceType:
*	Turns the type to which a type reference refers.
********************************************************************)
and dereferenceType t =
	match t with
		Absyn.TypeRefType(t) -> dereferenceType(t)
	|	t -> t


(******************************************************************
*mergeTypeAbbrevs:
******************************************************************)
and mergeTypeAbbrevs = fun t1 t2 ->
	let merge = fun sym tabbrev table ->
		let Absyn.TypeAbbrev(s, args, ty, p) = tabbrev in
		
		match (Table.find sym table) with
			Some Absyn.TypeAbbrev(s', args', ty', p') ->
				if args <> args' then
					(Errormsg.error p "typeabbrev already declared with different arguments";
					table)
				else if ty <> ty' then
					(Errormsg.error p "typeabbrev already declared with different type";
					table)
				else
					table
		|	None ->
			(Table.add sym tabbrev table)
	in
	(Table.SymbolTable.fold merge t1 t2)

(*	checkFixity:	Determines whether two absyn fixities are compatible.	*)
and checkFixity = fun f1 f2 ->
	(f1 = f2 || (f1 = Absyn.NoFixity || f2 = Absyn.NoFixity))
		
and checkPrec = fun f1 f2 ->
	(f1 = f2 || (f1 = -1 || f2 = -1))


(**********************************************************************
*translate:
*	Convert from a preabsyn module to an absyn module.
**********************************************************************)
let rec translate = fun mod' sig' ->
		let asig = translateSignature sig' in
		let amod = translateModule mod' asig in
		amod

(**********************************************************************
*translateSignature:
**********************************************************************)
and translateSignature = function
	Preabsyn.Signature(name, gconsts, gkinds, tabbrevs, fixities,accumsigs) ->
		(******************************************************************
		*mergeGlobalKinds:
		*	Adds the global kinds from one signature into the global kinds
		*	of all signatures.
		******************************************************************)
		let mergeGlobalKinds = fun kt1 kt2 ->
			let merge = fun sym kind ktable ->
				let Absyn.GlobalKind(s, Some a, _, p) = kind in
				
				(*	If the kind is already in the table, match the arity.
						Otherwise, add it to the table.	*)
				match (Table.find sym ktable) with
					Some Absyn.GlobalKind(s', Some a', _, p') ->
						if a <> a' then
							(Errormsg.error p ("kind already defined with arity " ^ (string_of_int a') ^
								(Errormsg.see p' "kind declaration"));
							ktable)
						else
							ktable
				|	None -> (Table.add sym kind ktable)
			in
			(Table.SymbolTable.fold merge kt1 kt2)
		in
		
		(******************************************************************
		*mergeGlobalConstants:
		*	Adds the constants from one signature into the constants from
		*	all accumulated signatures.
		******************************************************************)
		let mergeGlobalConstants = fun ct1 ct2 ->
			let merge = fun sym c ctable ->
				let Absyn.Constant(s,fix,prec,exp,useonly,nodefs,closed,pres,skels,tys,ci,ctype,p) = c in
				
				match (Table.find sym ctable) with
					Some Absyn.Constant(s',fix',prec',exp',useonly',nodefs',closed',pres',skels',tys',ci',ctype',p') ->
						if not (checkFixity fix fix') then
							(Errormsg.error p ("1constant already declared with fixity " ^ (Absyn.string_of_fixity fix') ^
								(Errormsg.see p' "constant declaration"));
							ctable)
						else if not (checkPrec prec prec') then
							(Errormsg.error p ("constant already declared with precedence" ^ (string_of_int prec'));
							ctable)
						else if (skels <> skels') || (tys <> tys') then
							(Errormsg.error p "constant already declared with different type";
							ctable)
						else
							ctable
				|	None -> (Table.add sym c ctable)
			in
			(Table.SymbolTable.fold merge ct1 ct2)
		in		
		
		(******************************************************************
		*processAccumSigs:
		*	Convert a list of accumulated signature filenames into a list of
		*	preabsyn signatures.
		******************************************************************)
		let rec processAccumSigs = function
			Preabsyn.Symbol(accum,_,_)::rest ->
				(Compile.compileSignature (Symbol.name accum))::(processAccumSigs rest)
		| [] -> []
		in
		
		(******************************************************************
		*translateAccumSigs:
		******************************************************************)
		let rec translateAccumSigs = function
			s::rest ->
				let (kinds, consts, tabbrevs) = translateSignature s in
				let (kinds', consts', tabbrevs') = translateAccumSigs rest in
				
				(mergeGlobalKinds kinds kinds', 
				mergeGlobalConstants consts consts',
				mergeTypeAbbrevs tabbrevs tabbrevs')
		|	[] ->
				(Table.SymbolTable.empty, Table.SymbolTable.empty, Table.SymbolTable.empty)
		in
		
		(*	Process accumulated signatures:	*)
		let sigs = processAccumSigs accumsigs in
		let (accumkindtable, accumconstanttable, accumtabbrevtable) = translateAccumSigs sigs in
		
		(*	Process kinds	*)
		let gkindtable = translateGlobalKinds gkinds in
		let kindtable = mergeGlobalKinds gkindtable accumkindtable in
		
		(*	Process type abbreviations	*)
		let tabbrevtable = translateTypeAbbrevs tabbrevs gkindtable in
		let tabbrevtable = mergeTypeAbbrevs tabbrevtable accumtabbrevtable in
		
		(*	Translate constants	*)
		let gconstanttable = translateGlobalConstants gconsts Table.SymbolTable.empty  gkindtable tabbrevtable in
		
		(*	Translate fixities *)
		let gconstanttable = translateFixities fixities gconstanttable in
		
		let constanttable = mergeGlobalConstants gconstanttable accumconstanttable in
		
		(kindtable,constanttable,tabbrevtable)

and translateModule = fun mod' sig' ->
		(********************************************************************
		*mergeLocalKinds:
		*	Merges the list of local kinds and the kind table.  Any local
		*	kind without an associated arity must be declared already.
		********************************************************************)
		let mergeLocalKinds = fun t1 t2 ->
			let merge = fun sym k table ->
				match k with
					Absyn.LocalKind(s, Some a, _, p) ->
						(match (Table.find sym table) with
							Some Absyn.GlobalKind(s', Some a', _, p') ->
								if a <> a' then
									(Errormsg.error p ("kind already declared with arity " ^ (string_of_int a));
									table)
								else
									(let table' = Table.remove sym table in
									(Table.add sym k table))
						|	Some Absyn.LocalKind(s', Some a', _, p') ->
								if a <> a' then
									(Errormsg.error p ("kind already declared with arity " ^ (string_of_int a));
									table)
								else
									table
						|	None -> (Table.add sym k table))
				|	Absyn.LocalKind(s, None, _, p) ->
						(match (Table.find sym table) with
							Some Absyn.GlobalKind(s', Some a', m, p') ->
								let table' = Table.remove sym table in
								(Table.add sym (Absyn.LocalKind(s', Some a', m, p')) table)
						|	Some Absyn.LocalKind(s', Some a', m, p') ->
								table
						|	None ->
								(Errormsg.error p ("undeclared kind " ^ (Symbol.name sym));
								table))
				|	_ -> (Errormsg.impossible (Absyn.getKindPos k) "mergeLocalKinds(): invalid kind type")
			in
			(Table.SymbolTable.fold merge t1 t2)
		in
		
		(********************************************************************
		*mergeGlobalKinds:
		*	Merges the global kinds declared in the module with the kind table.
		*	All global kinds are added as locals unless they appear as globals
		*	in the kindtable already.
		********************************************************************)
		let mergeGlobalKinds = fun t1 t2 ->
			let merge = fun sym kind table ->
				let Absyn.GlobalKind(s,Some a,m,p) = kind in
				match (Table.find sym table) with
					Some Absyn.GlobalKind(s',Some a',_, p') ->
						if a <> a' then
							(Errormsg.error p ("kind already declared with arity " ^ (string_of_int a) ^
								(Errormsg.see p' "kind declaration"));
							table)
						else
							table
				|	None ->
						(Table.add sym (Absyn.LocalKind(s, Some a, m, p)) table)
			in
			(Table.SymbolTable.fold merge t1 t2)
		in
		
		(********************************************************************
		*mergeAccumKinds:
		*	Merges all accumulated kinds with the signature kinds.
		*	All global kinds in the accumulated modules become local kinds
		*	in the result unless they are also in the signature.
		********************************************************************)
		let mergeAccumKinds = fun t1 t2 ->
			let merge = fun sym kind table ->
				let Absyn.GlobalKind(s, Some a,m,p) = kind in
				(match (Table.find sym table) with
					Some Absyn.GlobalKind(s', Some a',_,p') ->
						(if a <> a' then
							(Errormsg.error p ("kind already declared with arity " ^ (string_of_int a) ^ ". " ^
								(Errormsg.see p' "kind declaration"));
							table)
						else
							table)
				|	None ->
						(Table.add sym (Absyn.LocalKind(s, Some a, m, p)) table))
			in
			(Table.SymbolTable.fold merge t1 t2)
		in
		
		(********************************************************************
		*mergeLocalConstants:
		*	Merge the local constants in the module into the constant table.
		*	If a constant is declared as local and has no declared type then
		*	the constant must already exist as a global.  If the type is
		*	declared then it must either match an existing global declaration
		*	or there must not be any global declaration.
		********************************************************************)
		let mergeLocalConstants = fun t1 t2 ->
			let merge = fun sym constant table ->
				let Absyn.Constant(s,fix,prec,exp,useonly,nodefs,closed,pres,skels,tys,ci,ctype,p) = constant in
				match skels with
					[] ->	(*	This local has no defined type	*)
						(match (Table.find sym table) with
							Some Absyn.Constant(s',fix',prec',exp',useonly',nodefs',closed',pres',skels',tys',ci',ctype',p') ->
								if not (checkFixity fix fix') then
									(Errormsg.error p ("2constant already declared with fixity " ^ (Absyn.string_of_fixity fix') ^
										(Errormsg.see p' "constant declaration"));
									table)
								else if not (checkPrec prec prec') then
									(Errormsg.error p ("constant already declared with precedence " ^ (string_of_int prec') ^
										(Errormsg.see p' "constant declaration"));
									table)
								else if (skels <> skels') || (tys <> tys') then
									(Errormsg.error p ("constant already declared with different type" ^
										(Errormsg.see p' "constant declaration"));
									table)
								else
									let table' = (Table.remove sym table) in
									(Table.add sym (Absyn.Constant(s',fix',prec',exp',useonly',nodefs',closed',pres',skels',tys',ci',Absyn.LocalConstant,p')) table')
						|	None ->
								(Errormsg.error p ("local constant declared without type, and no global constant exists");
								table))
				|	skel::ss ->	(*	This local was defined with a type	*)
						(match (Table.find sym table) with
							Some Absyn.Constant(s',fix',prec',exp',useonly',nodefs',closed',pres',skels',tys',ci',ctype',p') ->
								if not (checkFixity fix fix') then
									(Errormsg.error p ("3constant already declared with fixity " ^ (Absyn.string_of_fixity fix') ^
										(Errormsg.see p' "constant declaration"));
									table)
								else if not (checkPrec prec prec') then
									(Errormsg.error p ("constant already declared with precedence " ^ (string_of_int prec') ^
										(Errormsg.see p' "constant declaration"));
									table)
								else if (skels <> skels') || (tys <> tys') then
									(Errormsg.error p ("constant already declared with different type" ^
										(Errormsg.see p' "constant declaration"));
									table)
								else
									let table' = (Table.remove sym table) in
									(Table.add sym (Absyn.Constant(s',fix',prec',exp',useonly',nodefs',closed',pres',skels',tys',ci',Absyn.LocalConstant,p')) table')
						|	None ->
								(Table.add sym constant table))
			in
			(Table.SymbolTable.fold merge t1 t2)
		in
		
		(********************************************************************
		*mergeGlobalConstants:
		*	Merge the global constants in the module into the constant table.
		*	This follows the same process as mergeAccumConstants.
		********************************************************************)
		let mergeGlobalConstants = fun t1 t2 ->
			let merge = fun sym constant table ->
				let Absyn.Constant(s,fix,prec,exp,useonly,nodefs,closed,pres,skels,tys,ci,ctype,p) = constant in
				match (Table.find sym table) with
					Some Absyn.Constant(s',fix',prec',exp',useonly',nodefs',closed',pres',skels',tys',ci',ctype',p') ->
						if not (checkFixity fix fix') then
							(Errormsg.error p ("constant already declared with fixity " ^ (Absyn.string_of_fixity fix') ^
								(Errormsg.see p' "constant declaration"));
							table)
						else if not (checkPrec prec prec') then
							(Errormsg.error p ("constant already declared with precedence " ^ (string_of_int prec'));
							table)
						else if (skels <> skels') || (tys <> tys') then
							(Errormsg.error p "constant already declared with different type";
							table)
						else
							table
				|	None ->
						(Table.add sym (Absyn.Constant(s,fix,prec,exp,useonly,nodefs,closed,pres,skels,tys,ci,Absyn.LocalConstant,p)) table)
			in
			(Table.SymbolTable.fold merge t1 t2)
		in
		
		(********************************************************************
		*mergeAccumConstants:
		*	Merge the accumulated constants with the signature.  Globals are
		*	made into locals unless they are already defined as global.  All
		*	redeclarations are checked for consistency.
		********************************************************************)
		let mergeAccumConstants = fun t1 t2 ->
			let merge = fun sym constant table ->
				let Absyn.Constant(s,fix,prec,exp,useonly,nodefs,closed,pres,skels,tys,ci,ctype,p) = constant in
				match (Table.find sym table) with
					Some Absyn.Constant(s',fix',prec',exp',useonly',nodefs',closed',pres',skels',tys',ci',ctype',p') ->
						if not (checkFixity fix fix') then
							(Errormsg.error p ("constant already declared with fixity " ^ (Absyn.string_of_fixity fix') ^
								(Errormsg.see p' "constant declaration"));
							table)
						else if not (checkPrec prec prec') then
							(Errormsg.error p ("constant already declared with precedence" ^ (string_of_int prec'));
							table)
						else if (skels <> skels') || (tys <> tys') then
							(Errormsg.error p "constant already declared with different type";
							table)
						else
							table
				|	None ->
						(Table.add sym (Absyn.Constant(s,fix,prec,exp,useonly,nodefs,closed,pres,skels,tys,ci,Absyn.LocalConstant,p)) table)
			in
			(Table.SymbolTable.fold merge t1 t2)
		in
		
		(******************************************************************
		*processAccumModes:
		*	Convert a list of accumulated modules filenames into a list of
		*	preabsyn signatures.
		******************************************************************)
		let rec processAccumMods = function
			Preabsyn.Symbol(accum,_,_)::rest ->
				(Compile.compileSignature ((Symbol.name accum)))::(processAccumMods rest)
		| [] -> []
		in
		
		(********************************************************************
		*translateAccumMods:
		*	Goes through all of the accumulated modules, parses them, and
		*	gets the appropriate tables and suchlike.
		********************************************************************)
		let rec translateAccumMods = function
			s::rest ->
				let (kinds, consts, tabbrevs) = translateSignature s in
				let (kinds', consts', tabbrevs') = translateAccumMods rest in
				
				(mergeGlobalKinds kinds kinds', 
				mergeGlobalConstants consts consts',
				mergeTypeAbbrevs tabbrevs tabbrevs')
		|	[] ->
				(Table.SymbolTable.empty, Table.SymbolTable.empty, Table.SymbolTable.empty)
		in
		(*	Get the pieces of the module	*)
		let Preabsyn.Module(name, gconsts, lconsts, cconsts, fixities,
												gkinds, lkinds, tabbrevs, clauses, accummods,
												accumsigs, usesigs) = mod' in
		
		(*	Translate the signature	*)
		let (sigkinds, sigconstants, sigtabbrevs) = sig' in
		
		(*	Translate the accumulated modules	*)
		let accummods' = processAccumMods accummods in
		let (accumkinds, accumconstants, accumtabbrevs) = translateAccumMods accummods' in
		
		(*	Translate local and global kinds, and get associated tables	*)
		let gkindtable = translateGlobalKinds gkinds in
		let lkindtable = translateLocalKinds lkinds in
		
		let kindtable = mergeAccumKinds accumkinds sigkinds in
		let kindtable = mergeGlobalKinds gkindtable kindtable in
		let kindtable = mergeLocalKinds lkindtable kindtable in
				
		(*	Translate type abbreviations and get the associated table	*)
		let tabbrevtable = translateTypeAbbrevs tabbrevs kindtable in
		let tabbrevtable = mergeTypeAbbrevs tabbrevtable sigtabbrevs in
		let tabbrevtable = mergeTypeAbbrevs tabbrevtable accumtabbrevs in
		
		(*	Translate local, global, and closed constants and get the
				associated tables. *)
		let gconsttable = translateGlobalConstants gconsts Table.SymbolTable.empty kindtable tabbrevtable in
		let lconsttable = translateLocalConstants lconsts Table.SymbolTable.empty kindtable tabbrevtable in
		
		let constanttable = mergeAccumConstants accumconstants sigconstants in
		let constanttable = mergeGlobalConstants gconsttable constanttable in
		let constanttable = mergeLocalConstants lconsttable constanttable in
		
		(*	Apply fixity flags	*)
		let constantTable = translateFixities fixities constanttable in
		
		Absyn.Module(name, constantTable, kindtable, tabbrevtable, [], [], [], [], [], [], [], [], [])
