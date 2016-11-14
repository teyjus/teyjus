(** Frontend for the tjtwelf tool *)

(* the signature file to parse. Should become a parameter. *)
let inputName = ref ""
let outputName = ref "top"

let setInputName ?(filter=(fun x -> x)) name =
  if !inputName = "" 
  then
    inputName := filter name 
  else
    (prerr_endline "Error: More than one input file specified.";
     exit 1)

let checkInput () =
  if !inputName = ""
  then
    (prerr_endline "Error: No input file specified.";
     exit 1)
  else 
    if Sys.file_exists !inputName
    then
      ()
    else
      (prerr_endline ("Error: Invalid input file: `"^ !inputName ^"'.");
      exit 1)

let specList = Parseargs.dualArgs
  [("-s", "--sig", Arg.Set_string inputName,
      " Specifies name of the input signature.") ;
   ("-o", "--output", Arg.Set_string outputName,
      " Specifies name of the generated Lambda Prolog module. Default is `top'.") ;
   ("-t", "--translation", Arg.String(Translator.set_translation),
      " Specifies the desired translation. Options: naive, optimized. Default is `optimized'.") ;
   ("-index", "--opt-index", Arg.Bool(Optimization.Swap.set),
      " Set whether the indexing optimization is run. Default is true.") ;
   ("-specialize", "--opt-specialize", Arg.Bool(Optimization.Specialize.set),
      " Set whether the specialized predicates optimization is run. Default is true.") ]

let usageMsg = 
  "Usage: tjtwelf [options] <signature-name>\n" ^
    "options are: "

let parse_args () =
  Arg.parse specList setInputName usageMsg 

(* Compile and link the module before loading *)
let compile_and_link () =
  Sys.command "./tjcc top";
  print_endline "compiled!";
  Sys.command "./tjlink top";
  print_endline "linked!"


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

(* Generate the sig and mod files, and the metadata file. *)
let output_files metadata signature modul =
  let (out_md, out_sig, out_mod) = (open_out (!outputName^".md"), open_out (!outputName^".sig"), open_out (!outputName^".mod")) in
  Printf.fprintf out_md "%s\n" metadata; 
  Printf.fprintf out_sig "%s\n" signature;
  Printf.fprintf out_mod "%s\n" modul;
  close_out out_md;
  close_out out_sig;
  close_out out_mod        

(** main *)
let _ = 
  let _ = print_string "tjtwelf started!\n" in
  let _ = parse_args () in
  let _ = checkInput () in
  (* parse LF signature *)
  let res = ParseTwelf.parse_sig (!inputName) in
  let sign =
    (match res with
         Some(s) -> (*print_string (Lfsig.string_of_sig s); *)
                    s
       | None -> print_string "failed to parse.\n";
                 exit 1) in

  (* translate LF sig and generate files. *)
  let _ = Translator.set_translation "optimized";
          Optimization.Swap.set true;
          Optimization.Specialize.set true in
  let (metadata, kinds, constants, terms) = 
    match Translator.get_translation () with
        "optimized" -> Translator.OptimizedTranslation.translate sign 
      | "naive" -> Translator.NaiveTranslation.translate sign
  in
  let (sigstr, modstr) = string_of_sig kinds constants terms in
  let metadatastr = Metadata.string_of_metadata metadata in
  let _ = output_files metadatastr sigstr modstr in

  (* compile and link LP module, load query state *)
  let _ = compile_and_link () in
  let (currmod, md) = Loader.load (!outputName) in
 
  (* Query solving interaction loop
     (based on funciton from simulatorfront)
     finds a solution for query and prints out. *)
  let rec solveQueryInteract () =
    let rec moreAnswers () =
      print_string "\nMore solutions (y/n)? ";
      match read_line () with
        | "y" -> true
        | "n" -> false
        | _ ->
            print_endline ("\nSorry, only options are `y' or `n'.\n" ^ 
                           "Let's try it again:");
            moreAnswers ()
    in
    if (Query.solveQuery ()) then
      if (Query.queryHasVars ()) then
        (Lfquery.show_answers currmod sign md; 
(*        (Query.showAnswers ();*)
         if (moreAnswers ()) then
           solveQueryInteract ()
         else
           print_endline "\nyes\n")
      else
        print_endline "\nyes\n"
    else
      print_endline "\nno (more) solutions\n"
  in
  let solveQuery query =
    (match query with 
         Some(lfquery) -> 
           print_endline ("LF query: " ^ Lfabsyn.string_of_query' lfquery);
           if Lfquery.submit_query lfquery md (Absyn.getModuleKindTable currmod) (Absyn.getModuleConstantTable currmod) then 
             solveQueryInteract ()
           else
             prerr_endline ""
       | None -> print_string "failed to parse query.\n");
    Module.cleanModule (); 
    Front.simulatorReInit false ;
    Module.initModuleContext ()  
  in
  (* enter interactive mode *)
  while true do 
(*    let _ = print_string ("[" ^ "top" ^"] ?- ") in *)
    solveQuery (ParseTwelf.parse_query ()) 
  done
