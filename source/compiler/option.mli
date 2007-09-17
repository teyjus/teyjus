(**********************************************************************
*Option
**********************************************************************)

(**********************************************************************
*isSome/isNone:
* Functions to test the value of an option.
**********************************************************************)
val isNone : 'a option -> bool
val isSome : 'a option -> bool

(**********************************************************************
*get:
* Returns the value of an option if it is a Some; raises an internal
* error otherwise.
**********************************************************************)
val get : 'a option -> 'a

(**********************************************************************
*string_of_option:
* Prints the value of an option with the given print function if the
* option is a Some; otherwise, prints "None".
**********************************************************************)
val string_of_option : 'a option -> ('a -> string) -> string
