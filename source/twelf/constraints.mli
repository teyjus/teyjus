
   exception Error of IntSyn.cnstrRef list

   val simplify : IntSyn.cnstrRef list -> IntSyn.cnstrRef list
   val warnConstraints : string list -> unit
