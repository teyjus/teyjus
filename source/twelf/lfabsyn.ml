(** Descibes the abstract syntax representation for LF. *)

type pos = Errormsg.pos

type typefam = TypeFam of (id * kind * fixity * assoc * int * (obj ref) list ref * pos)

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


let rec string_of_typefam (TypeFam(id,k,_,_,_,_,_)) =
  (string_of_id id) ^ " : " ^ (string_of_kind k) ^ "."

and string_of_obj (Object(id,ty,_,_,_,_)) =
  (string_of_id id) ^ " : " ^ (string_of_typ ty) ^ "."

and string_of_kind k =
  match k with
      PiKind(id,ty,body,_) -> 
        "({ " ^ (string_of_id id) ^ " : " ^ (string_of_typ ty) ^ "} " ^ (string_of_kind body) ^ ")"
    | ImpKind(ty,body,_) -> 
        "(" ^ (string_of_typ ty) ^ " -> " ^ (string_of_kind body) ^ ")"
    | Type(_) -> "type"

and string_of_typ ty =
  match ty with
      PiType(id,t1,t2,_) -> 
        "({ " ^ (string_of_id id) ^ " : " ^ (string_of_typ t1) ^ "} " ^ (string_of_typ t2) ^ ")"
    | AppType(t,tms,_) ->
        let tmlist =
          List.fold_left (fun s tm -> s ^ " " ^ (string_of_term tm)) "" tms
        in
        "(" ^ (string_of_id t) ^ " " ^ tmlist ^ ")"
    | ImpType(t1,t2,_) ->
        "(" ^ (string_of_typ t1) ^ " -> " ^ (string_of_typ t2) ^ ")"
    | IdType(id,_) ->
        string_of_id id

and string_of_term tm =
  match tm with
      AbsTerm(id,ty,body,_) -> 
        "([" ^ (string_of_id id) ^ " : " ^ (string_of_typ ty) ^"] " ^ (string_of_term body) ^ ")"
    | AppTerm(head, tms,_) -> 
        let tmlist = 
          List.fold_left (fun s t -> s ^ " " ^ (string_of_term t)) "" tms
        in
        "(" ^ (string_of_id head) ^ " " ^ tmlist ^ ")"
    | IdTerm(id,_) -> string_of_id id

and string_of_id id =
  match id with
      Const(n,_)
    | Var(n,_,_) 
    | LogicVar(n,_,_) -> n

let string_of_query (Query(_,id,ty)) =
  (string_of_id id) ^ " : " ^ (string_of_typ ty)

let get_typefam_pos (TypeFam(_,_,_,_,_,_,p)) = p
let get_obj_pos (Object(_,_,_,_,_,p)) = p
let get_kind_pos k = 
  match k with
      PiKind(_,_,_,p) -> p
    | ImpKind(_,_,p) -> p
    | Type(p) -> p
let get_typ_pos t =
  match t with
      PiType(_,_,_,p) -> p
    | AppType(_,_,p) -> p
    | ImpType(_,_,p) -> p
    | IdType(_,p) -> p
let get_term_pos t =
  match t with
      AbsTerm(_,_,_,p) -> p
    | AppTerm(_,_,p) -> p
    | IdTerm(_,p) -> p
let get_id_pos id =
  match id with
      Const(_,p) -> p
    | Var(_,_,p) -> p
    | LogicVar(_,_,p) -> p

let get_typefam_name (TypeFam(name,_,_,_,_,_,_)) = string_of_id name
let get_obj_name (Object(name,_,_,_,_,_)) = string_of_id name
let get_id_name id =
  match id with
      Const(n,_) -> n
    | Var(n,_,_) -> n
    | LogicVar(n,_,_) -> n

let get_typefam_kind (TypeFam(_,k,_,_,_,_,_)) = k
let get_obj_typ (Object(_,t,_,_,_,_)) = t
