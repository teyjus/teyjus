let isSome = function
    Some _ -> true
  | None -> false
  
let isNone = function
    Some _ -> false
  | None -> true

let get = function
    Some value -> value
  | None -> Errormsg.impossible Errormsg.none "Option.get: Invalid option"

let string_of_option v p =
  match v with
    Some a -> p a
  | None -> "None"
