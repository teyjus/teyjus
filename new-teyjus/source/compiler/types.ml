type typemolecule =
	Molecule of (Absyn.atype * Absyn.atype list * bool)

type unifyresult =
		OccursCheckFailure
	|	ClashFailure
	|	Success

(**********************************************************************
*checkApply:
*	Check an application between a function and an argument.
**********************************************************************)
let checkApply = fun fty argty term ->
	let fty = dereferenceTypeMolecule fty in
	let fskel = getTypeMoleculeType fty in
	
	(*	Just fail on an error term.	*)
	if fskel = Absyn.errorType then
		errorTypeMolecule
	else
		(*	Check the function isn't an arrow type and isn't a type variable (and
				so cannot be instantiated to an arrow type).	*)
		if not ((Absyn.isTypeVariable fskel) || (Absyn.isArrorType fskel)) then
			(Errormsg.error (Absyn.getTermPos term) ("operator is not a function." ^
				(Errormsg.info ("Operator type: " ^ (typemolecule_of_string fty)) ^
				(Errormsg.info ("in expression: " ^ (Absyn.aterm_of_string term) ^ ".")));
			errorTypeMolecule)
		
		(*	Check if the function is a variable, and
		else if (Absyn.isTypeVariable fskel) then
			let fty' = TypeMolecule(Absyn.TypeVarType(None, false), []) in
			let argty = TypeMolecule(Absyn.TypeVarType(None, false), []) in
			