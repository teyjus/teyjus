(** Descibes the abstract syntax representation for LF. *)

type pos = Errormsg.pos

(** A type-level declaration. 
    (constant name, kind, fixity, associativity, precedence,
    associated object constants, location in file) *)
type typefam = TypeFam of (id * kind * fixity * assoc * int * (obj ref) list ref * pos)

(** An object-level declaration.
    (constant name, type, fixity, associativity, precedence, location
     in file) *)
and obj = Object of (id * typ * fixity * assoc * int * pos)

and query = Query of (id * typ) list * id * typ

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
    PiKind of (id * typ * kind * pos)
  | ImpKind of (typ * kind * pos)
  | Type of pos

and typ =
    PiType of (id * typ * typ * pos)
  | AppType of (id * term list * pos)
  | ImpType of (typ * typ * pos)
  | IdType of (id * pos)

and term =
    AbsTerm of (id * typ * term * pos)
  | AppTerm of (id * term list * pos)
  | IdTerm of (id * pos)

and id =
  | Const of (string * pos)
  | Var of (string * typ * pos) 
  | LogicVar of (string * typ * pos)

val string_of_typefam : typefam -> string
val string_of_obj : obj -> string
val string_of_kind : kind -> string
val string_of_typ : typ -> string
val string_of_term : term -> string
val string_of_id : id -> string
val string_of_query : query -> string

val get_typefam_pos : typefam -> pos
val get_obj_pos : obj -> pos
val get_kind_pos : kind -> pos
val get_typ_pos : typ -> pos
val get_term_pos : term -> pos

val get_typefam_name : typefam -> string
val get_obj_name : obj -> string
val get_id_name : id -> string

val get_typefam_kind : typefam -> kind
val get_obj_typ : obj -> typ
