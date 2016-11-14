(* Parse: 
     contains functions for parsing implicit LF signatures (and queries) 
     and generating the explicit LF signature (and queries) 
*)

val parse_sig : string -> Lfsig.signature option
val parse_query : unit -> Lfabsyn.query option
