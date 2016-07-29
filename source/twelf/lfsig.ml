(** Describes a representation for fully explicit LF signatures. *)

type signature = Signature of (string list ref* Lfabsyn.typefam Symboltable.table)


(* print the signature by type family. for each type family print out 
   the corresponding objects in order *)
let print_signature (Signature(files, types)) =
  let print_for_type symb (Lfabsyn.TypeFam(_,_,_,_,_,objs,_) as ty) str =
    (Lfabsyn.print_typefam ty) ^ "\n" ^
    (List.fold_left (fun str objref -> str ^ (Lfabsyn.print_obj !objref) ^ "\n" ) "" (!objs)) ^ str
  in Symboltable.fold types print_for_type ""

let get_filenames (Signature(files, types)) = (!files)
let get_typetable (Signature(files, types)) = types
