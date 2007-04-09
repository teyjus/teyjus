type typemolecule =
  Molecule of (Absyn.atype * Absyn.atype list)
val errorMolecule : typemolecule
val getMoleculeEnvironment : typemolecule -> Absyn.atype list
val getMoleculeType : typemolecule -> Absyn.atype

type typeandenvironment =
  TypeAndEnvironment of (Absyn.atype * Absyn.atype list)

type variablebindings = (Absyn.atype * int) list
  
type unifyresult =
    OccursCheckFailure
  | ClashFailure
  | Success

exception UnifyException of unifyresult

val checkApply : typemolecule -> typemolecule -> Absyn.aterm -> typemolecule
val unify : typemolecule -> typemolecule -> unifyresult
val string_of_typemolecule  : typemolecule -> string

val makeConstantMolecule : Absyn.aconstant -> typemolecule
val makeKindMolecule : Absyn.akind -> typemolecule

val skeletonizeType : Absyn.atype -> typemolecule

(* added by Xiaochu *)
val equalMappedTypeSkels : Absyn.askeleton -> Absyn.askeleton -> bool
