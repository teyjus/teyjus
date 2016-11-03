
   exception Error of IntSyn.cnstr list

   val simplify : IntSyn.cnstr list -> IntSyn.cnstr list
   val warnConstraints : string list -> unit
