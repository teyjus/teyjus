(****************************************************************************
*Copyright 2008
*  Andrew Gacek, Nathan Guermond, Steven Holte, 
*  Gopalan Nadathur, Xiaochu Qi, Zach Snow
****************************************************************************)
(****************************************************************************
* This file is part of Teyjus.
*
* Teyjus is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Teyjus is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Teyjus.  If not, see <http://www.gnu.org/licenses/>.
****************************************************************************)
(**********************************************************************
*typemolecule:
* A representation of an absyn type and its associated environment.
* Skeleton indices in the type refer to elements in the environment:
* an index n indicates the nth element in the list.
**********************************************************************)
type typemolecule =
  Molecule of (Absyn.atype * Absyn.atype list)

val errorMolecule : typemolecule
val getMoleculeEnvironment : typemolecule -> Absyn.atype list
val getMoleculeType : typemolecule -> Absyn.atype

(**********************************************************************
*unifyresult:
* Indicates the result of unififcation.  There are three possible
* results:
*   OccursCheckFailure: indicates that the occurs check failed, and
*     that variable binding would introduce a cycle in a type.
*   ClashFailure: indicates a type clash.
*   Success: indicates a successful unification; variable bindings
*     now reflect the unification (in both checkApply and unify these
*     bindings are performed as a side-effect).
**********************************************************************)
type unifyresult =
    OccursCheckFailure
  | ClashFailure
  | Success
exception UnifyException of unifyresult

(**********************************************************************
*checkApply:
* Checks whether the type of the argument to a function matches the
* expected argument type.  The parameters are the argument type and
* the function type (not the expected argument type).  In addition,
* the term that this unification pertains to is passed to aid error
* message printing.  The result is a typemolecule indicating the type
* of the application (that is, the return type) if the unification is
* successful, or the error molecule if the unification is not successful.
* Variables are bound as a side effect.
**********************************************************************)
val checkApply : typemolecule -> typemolecule -> Absyn.aterm -> typemolecule

(**********************************************************************
*unify:
* Determines whether two type molecules unify.  If they do, the necessary
* variables are bound as a side-effect.
**********************************************************************)
val unify : typemolecule -> typemolecule -> unifyresult

(**********************************************************************
*string_of_typemolecule:
* Produces a human-readable string representation of a type molecule.
**********************************************************************)
val string_of_typemolecule  : typemolecule -> string

(**********************************************************************
*makeConstantMolecule:
* Given an absyn constant, produces a type molecule with a valid type
* environment corresponding to the type of the constant. The boolean
* flag indicates whether to search the given constant's type to determine
* the maximum skeleton size (true), or to use the entry in the absyn
* constant (false).  Searching should only be done when the constant has
* been parsed from the top-level.
**********************************************************************)
val makeConstantMolecule : bool -> Absyn.aconstant -> typemolecule

(**********************************************************************
*makeKindMolecule:
* Given an absyn kind, produces a type molecule with a valid type
* environment corresponding to an application with this type as the
* head and no arguments.
**********************************************************************)
val makeKindMolecule : Absyn.akind -> typemolecule

(**********************************************************************
*skeletonizeType:
* Takes an absyn type with variables in it and produces a type molecule
* whose type is the original type with variables replaced with
* skeleton indices and whose type environment corresponds to the
* types of the replaced variables.
**********************************************************************)
val skeletonizeType : Absyn.atype -> typemolecule
val skeletonizeMolecule : typemolecule -> typemolecule

(**********************************************************************
*equalMappedTypeSkels:
* Check whether the given type skeletons are identical.
**********************************************************************)
val equalMappedTypeSkels : Absyn.askeleton -> Absyn.askeleton -> bool

(**********************************************************************
*freeTypeVars:
* Produce a list of type variables that appear free in the given type
* expression and are new to the given list of type variables.
**********************************************************************)
val freeTypeVars : Absyn.atype -> (Absyn.atype list) -> Absyn.atype list

(************************************** ********************************
* Accumulate the cells of the form:
*   Absyn.TypeVarType(Absyn.BindableTypeVar(ref None)
* that appear in one of the types in the first argument but not in the
* list of such entries in the second argument into the second argument,
* thus producing the third.  Entries are unique as physical addresses,
* not as structures.
**********************************************************************)
val getNewVarsInTypes : Absyn.atype list -> Absyn.atype list -> Absyn.atype list

(**********************************************************************
*getNewVarisInTypeMol:
* Like getNewVarsInTypes, except that the type now may have a skeleton and a
* nontrivial environment and type variables may then have to be collected from
* the latter
**********************************************************************)
val getNewVarsInTypeMol : typemolecule -> Absyn.atype list -> Absyn.atype list

(**********************************************************************
*replaceTypeSetType:
* Given a type set type, returns the corresponding normal type.  The
* type returned depends on the type set and its default.
**********************************************************************)
val replaceTypeSetType : Absyn.atype -> Absyn.atype


val unitTests : unit -> unit
