(**********************************************************************
*reduceSkeletons:
* Given an absyn module, returns the module updated to reflect any
* possible skeleton optimizations for type-preserving skeletons.
**********************************************************************)
val reduceSkeletons : Absyn.amodule -> Absyn.amodule

(**********************************************************************
*reducePredicates:
* Given an absyn module, returns the module updated to reflect any
* possible predicate optimizations.
**********************************************************************)
val reducePredicates : Absyn.amodule -> Absyn.amodule
