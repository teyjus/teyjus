(** Descibes the abstract syntax representation for LF. *)


val maxPrec : int
val minPrec : int

(** A type-level declaration. 
    (constant name, kind, fixity, associativity, precedence,
    associated object constants, # implicit elements) *)
type typefam = TypeFam of (id * kind * fixity * assoc * int * (obj ref) list ref * int)

(** An object-level declaration.
    (constant name, type, fixity, associativity, precedence, # implicit arguments) *)
and obj = Object of (id * typ * fixity * assoc * int * int)

and query = Query of (id * typ) list * id * typ

and solution = (id * term) list * (term * term) list

and fixity =
  Infix
| Prefix
| Postfix
| NoFixity

and assoc =
  None
| Right
| Left

and kind =
  PiKind of (id * typ * kind)
| ImpKind of (typ * kind)
| Type 

and typ =
  PiType of (id * typ * typ)
| AppType of (id * term list)
| ImpType of (typ * typ)
| IdType of (id)

and term =
  AbsTerm of (id * typ * term)
| AppTerm of (id * term list)
| IdTerm of (id)

and id =
| Const of (string)
| Var of (string * typ) 
| LogicVar of (string * typ)

val string_of_typefam : typefam -> string
val string_of_obj : obj -> string
val string_of_kind : kind -> string
val string_of_typ : typ -> string
val string_of_term : term -> string
val string_of_id : id -> string
val string_of_query : query -> string
val string_of_solution : solution -> string

val get_typefam_implicit : typefam -> int
val get_obj_implicit : obj -> int

val get_typefam_name : typefam -> string
val get_obj_name : obj -> string
val get_id_name : id -> string

val get_typefam_kind : typefam -> kind
val get_obj_typ : obj -> typ
