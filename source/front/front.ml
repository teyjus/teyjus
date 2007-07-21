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
let systemInitC heapSize =
  Simerrors.handleSimExceptions (Ccode_stubs.systemInit heapSize)

(***************************************************************************)
(* interface function                                                      *)
(***************************************************************************)
let systemInit heapSize =
  systemInitOCaml () ;
  systemInitC heapSize

(** ********************************************************************** **)
(**            simulator initialization                                    **)
(** ********************************************************************** **)
let simulatorInit () =
  Simerrors.handleSimExceptions (Ccode_stubs.simulatorInit ())
  
let simulatorReInit imp =
  Simerrors.handleSimExceptions (Ccode_stubs.simulatorReInit imp)
