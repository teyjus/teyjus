let isSome = function
    Some _ -> true
  | None -> false
  
let isNone = function
    Some _ -> false
  | None -> false


let get = function
    Some value -> value
  | None -> raise Not_found


let string_of_option v p =
  match v with
    Some a -> p a
  | None -> "None"

