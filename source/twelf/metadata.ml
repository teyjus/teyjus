(** Metadata generated during translation. *)

type metadata = (Symbol.symbol Symboltable.table * Symb.symbol Table.symboltable)

let getLP (lf_to_lp, _) s = Symboltable.lookup lf_to_lp s

let getLF (_, lp_to_lf) s = Table.find s lp_to_lf

let rec set_mapping (lf_to_lp, lp_to_lf) lfs lps =
  let lf_to_lp' = Symboltable.insert lf_to_lp lfs lps in
  let lp_to_lf' = Table.add lps lfs lp_to_lf in
  (lf_to_lp', lp_to_lf')


let new_mapping metadata lfs =
  let lps = Symbol.generateName (Symb.name lfs) in
  set_mapping metadata lfs lps

let string_of_metadata (lf_to_lp, _) =
  let string_of_entry lfs lps str =
    str  ^ (Symb.name lfs) ^ " " ^ (Symbol.name lps) ^ "\n" 
  in Symboltable.fold lf_to_lp string_of_entry "" 

let empty = (Symboltable.empty, Table.empty)
