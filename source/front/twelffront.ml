(** Frontend for the tjtwelf tool *)

(* Compile and link the module before loading *)
let compile_and_link () =
  Sys.command "./tjcc top";
  print_endline "compiled!";
  Sys.command "./tjlink top";
  print_endline "linked!"


(* Generate the sig and mod files, and the metadata file. *)
let output_files metadata signature modul =
  let (out_md, out_sig, out_mod) = (open_out "top.md", open_out "top.sig", open_out "top.mod") in
  Printf.fprintf out_md "%s\n" metadata; 
  Printf.fprintf out_sig "%s\n" signature;
  Printf.fprintf out_mod "%s\n" modul;
  close_out out_md;
  close_out out_sig;
  close_out out_mod        

(* Generate the sig and mod file contents *)
let string_of_sig kinds constants terms =
  let per_kind kind =
    let rec getKindType n =
      match n with
          0 -> "type"
        | _ -> "type -> " ^ getKindType (n-1) 
    in
    "kind " ^ (Absyn.string_of_kind kind) ^ " " ^ (getKindType (Absyn.getKindArity kind)) ^"."
  in
  let per_const constant =
    "type " ^ (Absyn.getConstantPrintName constant) ^ " " ^(Absyn.string_of_skeleton (Absyn.getConstantSkeletonValue constant)) ^ "."
  in
  let per_term term =
    (Absyn.string_of_term term) ^ "."
  in
  let sigstr = Table.fold (fun s c b -> b ^ (per_const c) ^ "\n") 
                          constants 
                          ((Table.fold (fun s k b -> b ^ (per_kind k) ^ "\n") 
                                       kinds 
                                       "sig top.\n%%%kind decls\n") ^ "%%%const decls\n")
  in
  let modstr = List.fold_left (fun s t -> s ^ (per_term t) ^ "\n")
                              "module top.\n"
                              terms
                              
  in (sigstr, modstr)

(** main *)
let _ = 
  let _ = print_string "tjtwelf started!\n" in
  let res = Lfparse.parse "test.elf" in
  let sign =
    (match res with
         Some(s) -> (*print_string (Lfsig.string_of_sig s); *)
                    s
       | None -> print_string "failed to parse.\n";
                 exit 1) in
  let _ = Translator.set_translation "optimized";
          Optimization.Swap.set true;
          Optimization.Specialize.set false in
  let (metadata, kinds, constants, terms) = 
    match Translator.get_translation () with
        "optimized" -> Translator.OptimizedTranslation.translate sign 
      | "naive" -> Translator.NaiveTranslation.translate sign
  in
  let (sigstr, modstr) = string_of_sig kinds constants terms in
  let metadatastr = Metadata.string_of_metadata metadata in
  let _ = output_files metadatastr sigstr modstr in
  let _ = compile_and_link () in
  let (currmod, md) = Loader.load "top" in
  let lfquery = 
    match Lfparse.parse_query "query.elf" with 
        Some(q) -> q
      | None -> print_string "failed to parse query.\n";
                exit 1
  in
  let _ = print_endline (Lfabsyn.string_of_query lfquery) in
(*  let _ = Lfquery.submit_query lfquery md (Absyn.getModuleKindTable currmod) (Absyn.getModuleConstantTable currmod) in *)
  print_string "done!\n";
  exit 1
