val isNone : 'a option -> bool
val isSome : 'a option -> bool

val get : 'a option -> 'a

val string_of_option : 'a option -> ('a -> string) -> string
