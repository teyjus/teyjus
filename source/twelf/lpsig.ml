type signature = Signature of (string * Lpabsyn.kinddecl Symboltable.table * Lpabsyn.typedecl Symboltable.table)

let print_signature (Signature(name, kinds, types)) =
  let kinddecls = 
    Symboltable.fold kinds 
                     (fun s k str -> str ^ (Lpabsyn.print_kinddecl k) ^ "\n")
                     "% kind decls\n"
  in
  let f s (Lpabsyn.TypeDec(_,_,clauses) as t) (sigstr, modstr) =
      let sigstr' = sigstr ^ (Lpabsyn.print_typedecl t) ^ "\n"
      in 
      let modstr' =
       List.fold_left (fun s c -> s ^ (Lpabsyn.print_clause !c) ^ "\n") modstr (!clauses)
      in (sigstr', modstr')
  in
  Symboltable.fold types f ((kinddecls ^ "\n% type decls\n"), "")

let get_filename (Signature(name, kinds, types)) = name
let get_kinds (Signature(name, kinds, types)) = kinds
let get_types (Signature(name, kinds, types)) = types
