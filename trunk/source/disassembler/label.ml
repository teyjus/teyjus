(***************************************************************************)
(* Implement a mapping from code offset to labels                          *)
(***************************************************************************)
type label = string 

let hashtable = Hashtbl.create 1

let nextIndex   = ref 0

let label offset =
  try
	Hashtbl.find hashtable offset
  with
	Not_found -> 
	  let index    = !nextIndex in 
	  let newlabel = ("L" ^ (string_of_int index)) in
	  nextIndex := index + 1;
	  Hashtbl.add hashtable offset newlabel;
	  newlabel


let assignLabel offset name =
  try 
	let l = Hashtbl.find hashtable offset in
	Errormsg.impossible Errormsg.none 
	  ("assignLabel: label " ^ l ^ " has already been assigned to address " ^
	   (string_of_int offset)) 
  with
	Not_found ->
	  Hashtbl.add hashtable offset name
	
let addLabel offset =
  try
	let _ = Hashtbl.find hashtable offset in
	()
  with
	Not_found ->
	  let index    = !nextIndex in 
	  let newlabel = ("L" ^ (string_of_int index)) in
	  nextIndex := index + 1;
	  Hashtbl.add hashtable offset newlabel

let findLabel offset =
  try
	Hashtbl.find hashtable offset
  with
	Not_found -> ""

