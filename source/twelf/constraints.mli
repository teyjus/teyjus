
   exception Error of IntSyn.cnstr list

   val simplify : IntSyn.cnstr ref list -> IntSyn.cnstr ref list
   val warnConstraints : string list -> unit
