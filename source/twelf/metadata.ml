(** Metadata generated during translation. *)

(* mapping of lf symbols to lp symbols, mapping of lp symbols to lf symbols, and collections of all LF constant names.
   constant names are used to check for clashes when generating new variable names. *)
type metadata = (Symbol.symbol Symboltable.table * Symb.symbol Table.symboltable) * string Symboltable.table

let isName ((_,_),names) n = Option.isSome(Symboltable.lookup names (Symb.symbol n))

let getLP ((lf_to_lp, _),_) s = Symboltable.lookup lf_to_lp s

let getLF ((_, lp_to_lf),_) s = Table.find s lp_to_lf

let rec set_mapping ((lf_to_lp, lp_to_lf), names) lfs lps =
  let lf_to_lp' = Symboltable.insert lf_to_lp lfs lps in
  let lp_to_lf' = Table.add lps lfs lp_to_lf in
  let names' = Symboltable.insert names lfs (Symb.name lfs) in
  ((lf_to_lp', lp_to_lf'), names')


(** for now leaving this simple and always add 'lf_' to front of
    name when translating to ensure no clashes with reserved 
    identifiers in Teyjus. *)
let new_mapping metadata lfs =
  let lps = Symbol.symbol ("lf_"^(Symb.name lfs)) in
  set_mapping metadata lfs lps

let string_of_metadata ((lf_to_lp, _),_) =
  let string_of_entry lfs lps str =
    str  ^ (Symb.printName lfs) ^ " " ^ (Symbol.printName lps) ^ "\n" 
  in Symboltable.fold lf_to_lp string_of_entry "" 

let empty = ((Symboltable.empty, Table.empty), Symboltable.empty)
