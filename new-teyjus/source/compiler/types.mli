type typemolecule =
  Molecule of (Absyn.atype * Absyn.atype list * bool)
val errorMolecule : typemolecule
val getMoleculeEnvironment : typemolecule -> Absyn.atype list
val getMoleculeType : typemolecule -> Absyn.atype

type variablebindings = (Absyn.atype * int) list
  
type unifyresult =
    OccursCheckFailure
  | ClashFailure
  | Success

exception UnifyException of unifyresult

val checkApply : typemolecule -> typemolecule -> Absyn.aterm -> typemolecule
val unify : typemolecule -> typemolecule -> unifyresult
val string_of_typemolecule  : typemolecule -> string
val makeConstantType : Absyn.aconstant -> typemolecule
val makeKindType : Absyn.akind -> typemolecule