(** system initialization **)

(****************************************************************************)
(* initialization code for OCaml part of the system                         *)
(****************************************************************************)
let systemInit () =
  (* set the number of bytes contained by a word *)
  Bytecode.setWordSize (); 
  (* set pervasive kind and index mapping: needed for ocaml loader *)
  Pervasiveutils.pervasiveKindIndexMappingInit ();
  Pervasiveutils.pervasiveConstantIndexMappingInit ()

(****************************************************************************)
(* invocation of the initialization code for the C part of the system       *)
(****************************************************************************)
let systemInitC () =
  Front.systemInit (!Parseargs.heapSize)


(** link and load **)





(** *********************************************************************** **)
(** the main routine                                                        **)
(** *********************************************************************** **)
let main () =
  (* read command line *)
  if (Parseargs.parseArgs Parseargs.Tjsim) then
	(systemInit  ();         (* system initialization: ocaml *)
	 systemInitC ();         (* system initialization: c     *)
	 (* load *)
	 Front.simulatorInit (); (* intialize simulator memory   *)
	 Front.test();
	 0)
  else 1



(*	let modName = !(Parseargs.inputFileName) in
   let query = !(Parseargs.queryString) in
   loadModule modName;
   loadQuery queryString; 
	  0
*)



let _ = main ()
