type pos	
type typemolecule =
	Molecule of (Absyn.atype * Absyn.atype list * bool)

type typeandenvironment =
	TypeAndEnvironment of (Absyn.atype * Absyn.atype list)

type argstypes =
	ArgsTypes of (int * Absyn.atype list * Absyn.atype list)

val getTargetType : Absyn.atype -> Absyn.atype

type unifyresult =
		OccursCheckFailure
	|	ClashFailure
	|	Success

type newsymbolfunc = Symbol.symbol -> int -> int -> Absyn.akind

val unifyTypeMolecules : typemolecule -> typemolecule -> unifyresult
val unifiable : typemolecule -> typemolecule -> bool

val checkApplication : typemolecule -> typemolecule -> Absyn.aterm -> pos -> typemolecule

val translateTypeSkeleton : Absyn.atype Table.SymbolTable.t -> Preabsyn.ptype -> newsymbolfunc -> typeandenvironment
val translateTypeAbbrev : Preabsyn.ptype -> Absyn.atypeabbrev Table.SymbolTable.t -> Absyn.atype

val skeletonize : Absyn.atype -> typeandenvironment

val fixType : Absyn.atype -> Absyn.atype

val dereferenceType : Absyn.atype -> Absyn.atype
