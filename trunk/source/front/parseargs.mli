val dualArgs : (Arg.key * Arg.key * Arg.spec * Arg.doc) list ->
  (Arg.key * Arg.spec * Arg.doc) list

val versionspec : Arg.key * Arg.key * Arg.spec * Arg.doc

val getModName : string -> string
  
val error : string -> 'a
