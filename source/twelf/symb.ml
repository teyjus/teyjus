(** Maps strings to identifiers. *)

(** A symbol consists of a string, a unique integer identifier, and a
    print name which may differ from the base string. *)
type symbol = string * int * string

let hashtable = Hashtbl.create 10

let nextsym = ref 0

let symbol name = 
  try 
    let id = Hashtbl.find hashtable name
    in (name, id, name)
  with
    Not_found -> 
      let id = !nextsym
      in (nextsym := id+1); 
         (Hashtbl.add hashtable name id); 
         (name, id, name)

let name (n,i,pn) = n

let printName (n,i,pn) = pn

let id (n,i,pn) = i

