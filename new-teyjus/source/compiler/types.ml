(********************************************************************
*translateTypeAbbrev:
*	Translate a type abbreviation call.  Replaces formals with the
*	actual arguments, ie: #1 -> #2 -> #1 with arguments "int" and
*	"string" becomes int -> string -> int.
********************************************************************)
let rec translateTypeAbbrev =
	fun abbrev abbrevtable ->
		let Preabsyn.TypeAbbrevCall(name, argtypes, pos) = abbrev in

		(****************************************************************
		*replaceSym:
		*	Given a type, replaces all instances of the given symbol within
		*	the type by a new type.
		****************************************************************)
		let rec replaceSym =
			fun sym rep t ->
				match t with
						Preabsyn.Atom(a) ->
							if sym = a then
								rep
							else
								t
					|	Preabsyn.Arrow(l, r, pos) ->
							Preabsyn.Arrow((replaceSym sym rep l), (replaceSym sym rep r), pos)
					|	Preabsyn.App(l, r, pos) ->
							Preabsyn.App((replaceSym sym rep l), (replaceSym sym rep r), pos)
					|	Preabsyn.TypeAbbrevCall(_,_,_) ->
							let newType = (translateTypeAbbrev t abbrevtable) in
							(replaceSym sym rep newType)
					|	Preabsyn.ErrorType -> t					
		in
		
		(****************************************************************
		*replaceAll:
		*	Given a list of symbols and a corresponding list of types, and
		*	a target type, replaces all symbols in the target type by their
		*	corresponding type.
		****************************************************************)
		let rec replaceAll =
			fun slist tlist targ ->
				match (slist, tlist) with
					((sym::syms), (t::ts)) ->
						(replaceAll syms ts (replaceSym sym targ t))
				|	([], []) -> targ
								
		in
		
		let Preabsyn.TypeAbbrevCall(abbrevname, _, _) = abbrev in
		let Preabsyn.TypeAbbrev(name, argnames, targtype, pos) = (Table.SymbolTable.find abbrevname abbrevtable) in
		
		(*	Check for correct number of arguments	*)
		if ((List.length argnames) <> (List.length argtypes)) then
			(Errormsg.error pos ("typeabbrev '" ^ name ^ "': invalid number of arguments.");
			Preabsyn.ErrorType)
		else
			(replaceAll argnames argtyps targtype)

(********************************************************************
*rationalizeType
********************************************************************)

(********************************************************************
*translateTypeSkeleton:
********************************************************************)
let translateTypeSkeleton =
	fun stable t f ->
		match t
			with	Preabsyn.Arrow(l, r, _) ->
							()
			|			Preabsyn.TypeAbbrevCall(_) ->
							let ty = (translateTypeAbbrev t) in
							(translateTypeSkeleton stable ty f)
			|			_ ->
							let ty = (rationalizeType t) in
							(TypeAndEnv(true, (getTypeAndBindingsType ty), typeSkeletonIndex))

(********************************************************************
*getTargetType:
********************************************************************)
let getTargetType =
	fun Absyn.ArrowType(t, _) -> t

(********************************************************************
*dereferenceType:
*	Turns the type to which a type reference refers.
********************************************************************)
let dereferenceType t =
	match t with
		Absyn.TypeRefType(t) -> dereferenceType(t)
	|	t -> t
