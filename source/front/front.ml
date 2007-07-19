(** ********************************************************************** **)
(**             system initialization                                      **)
(** ********************************************************************** **)

(****************************************************************************)
(* initialization code for OCaml part of the system                         *)
(****************************************************************************)
let systemInitOCaml () =
  (* set the number of bytes contained by a word *)
  Bytecode.setWordSize (); 
  (* set pervasive kind and index mapping: needed for ocaml loader *)
  Pervasiveutils.pervasiveKindIndexMappingInit ();
  Pervasiveutils.pervasiveConstantIndexMappingInit ()

(****************************************************************************)
(* invocation of the initialization code for the C part of the system       *)
(****************************************************************************)
let systemInitC () =
  let _ = Simerrors.handleSimExceptions 
	  (Ccode_stubs.systemInit (!Parseargs.heapSize)) 
  in
  ()

(***************************************************************************)
(* interface function                                                      *)
(***************************************************************************)
let systemInit () =
  systemInitOCaml ();
  systemInitC ()

(** ********************************************************************** **)
(**            simulator initialization                                    **)
(** ********************************************************************** **)
let simulatorInit () =
  let _ = Simerrors.handleSimExceptions (Ccode_stubs.simulatorInit ()) in
  ()
  
let simulatorReInit imp =
  let _ = Simerrors.handleSimExceptions (Ccode_stubs.simulatorReInit imp) in
  ()
