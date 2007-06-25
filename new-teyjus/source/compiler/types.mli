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

(* Check whether the given type skeletons are identical                *)
val equalMappedTypeSkels : Absyn.askeleton -> Absyn.askeleton -> bool

(* Produce a list of type variables that appear free in the given type *)
(* expression and are new to the given list of type variables.         *)
val freeTypeVars : Absyn.atype -> (Absyn.atype list) -> Absyn.atype list

(* Accumulate the cells of the form Absyn.TypeVarType(Absyn.BindableTypeVar(ref None)) *)
(* that appear in one of the types in the first argument but not in the list of such *)
(* entries in the second argument into the second argument, thus producing the third. *)
(* Entries are unique as physical addresses, not as structures *)
val getNewVarsInTypes : Absyn.atype list -> Absyn.atype list -> Absyn.atype list

(* Like getNewVarsInTypes, except that the type now may have a skeleton and a *)
(* nontrivial environment and type variables may then have to be collected from *)
(* the latter *)
val getNewVarsInTypeMol : typemolecule -> Absyn.atype list -> Absyn.atype list)

val unitTests : unit -> unit
