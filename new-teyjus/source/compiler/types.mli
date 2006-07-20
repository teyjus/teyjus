type typemolecule =
  Molecule of (Absyn.atype * Absyn.atype list * bool)

type unifyresult =
    OccursCheckFailure
  | ClashFailure
  | Success

exception UnifyException of unifyresult

val checkApply : typemolecule -> typemolecule -> Absyn.aterm -> typemolecule