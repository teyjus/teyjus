val isOverloaded : Absyn.aconstant -> bool
val getOverload : Absyn.akind -> Absyn.aconstant -> Absyn.aconstant
val maxSkeletonIndex : int

val cutFailTerm : Absyn.aterm 

(**************************************************************************)
(* pervasive kind and constant index-data mapping table: needed for       *)
(* loading and disassembling.                                             *)
(**************************************************************************)
val pervasiveKindIndexMappingInit : unit -> unit
val findKindIndexMapping : int -> Absyn.akind option

val pervasiveConstantIndexMappingInit : unit -> unit
val findConstantIndexMapping : int -> Absyn.aconstant option
