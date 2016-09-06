(** Describes a representation for fully explicit LF signatures. *)

type signature = Signature of (string list ref* Lfabsyn.typefam Symboltable.table)


(* print the signature by type family. for each type family print out 
   the corresponding objects in order *)
let string_of_sig (Signature(files, types)) =
  let per_type symb (Lfabsyn.TypeFam(_,_,_,_,_,objs,_) as ty) str =
    (Lfabsyn.string_of_typefam ty) ^ "\n" ^
    (List.fold_left (fun str objref -> str ^ (Lfabsyn.string_of_obj !objref) ^ "\n" ) "" (!objs)) ^ str
  in Symboltable.fold types per_type ""

let get_filenames (Signature(files, types)) = (!files)
let get_typetable (Signature(files, types)) = types
