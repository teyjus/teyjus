type typemolecule =
	Molecule of (Absyn.atype * Absyn.atype list * bool)

type unifyresult =
		OccursCheckFailure
	|	ClashFailure
	|	Success

val checkApply : typemolecule -> typemolecule -> Absyn.aterm -> typemolecule