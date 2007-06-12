(**********************************************************************
*Symbol Module:
* Implements a simple module mapping strings to unique identifiers.
* Uses the standard library Hashtbl module.
**********************************************************************)
type symbol = string * int

let hashtable = Hashtbl.create 1

let nextsym = ref 0

let name (s,n) = s
let symbol = fun(s: string) ->
  try
    (s, (Hashtbl.find hashtable s))
  with
    Not_found ->  let id : int = !nextsym
                  in
                    begin
                      nextsym := id + 1;
                      (Hashtbl.add hashtable s id);
                      (s,id)
                    end
let id (s,n) = n

let currentId = ref 0
let generateName s =
  let s' = s ^ "_gen_" ^ (string_of_int (!currentId)) in
  let _ = currentId := !currentId + 1 in
  symbol s'

let generate () =
  generateName ""