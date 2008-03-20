open Parseargs

external link : string -> int -> int = "FRONT_link"

let verbosity = ref 0
  
let moreVerbose () = incr verbosity

let specList = dualArgs
  [("-V", "--verbose", Arg.Unit moreVerbose,
    " Produce verbose output - use multiple times to increase verbosity");
   versionspec]

let usageMsg = "Usage: tjlink <module-name>"

let _ =
  Arg.parse (Arg.align specList) setInputName usageMsg ;
  ensureInputName () ;

  exit (link !inputName !verbosity) ;
