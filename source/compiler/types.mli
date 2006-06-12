type pos = Errormsg.pos

type typemolecule =
	Molecule of (Absyn.atype * Absyn.atype list * bool)

type typeandenvironment =
	TypeAndEnvironment of (Absyn.atype * int * bool)

type argstypes =
	ArgsTypes of (int * Absyn.atype list * Absyn.atype list)


type unifyresult =
		OccursCheckFailure
	|	ClashFailure
	|	Success

val translate : Preabsyn.pmodule -> Preabsyn.pmodule -> Absyn.amodule

val dereferenceType : Absyn.atype -> Absyn.atype
