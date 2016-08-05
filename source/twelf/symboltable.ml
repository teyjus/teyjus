(** Provides symbol tables. *)

(** Describe an ordering for {!Symb.symbol}. *)
module OrderedType =
struct
  type t = Symb.symbol

  (** Order using less-than on integer identifier from symbol. *)
  let compare s1 s2 =
    let id1 = Symb.id s1 in
    let id2 = Symb.id s2 in
    if (id1 < id2) 
      then -1    
      else if (id1 > id2)
      then 1
      else 0
end

module SymbolTable = Map.Make(OrderedType)

type 'a table = 'a SymbolTable.t

let empty = SymbolTable.empty

let insert table key v = SymbolTable.add key v table

let lookup table key = 
  try
    Some(SymbolTable.find key table)
  with
    Not_found -> None

let remove table key = SymbolTable.remove key table

let fold table f v = SymbolTable.fold f table v

let iter table f = SymbolTable.iter f table
