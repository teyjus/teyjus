(** Descibes the abstract syntax representation for LF. *)

let maxPrec = 9999
let minPrec = 0

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



let rec string_of_typefam (TypeFam(id,k,_,_,_,_,implicit)) =
  (string_of_id id) ^ " : " ^ (string_of_kind (skip_kind implicit k)) ^ "."

and string_of_obj (Object(id,ty,_,_,_,implicit)) =
  (string_of_id id) ^ " : " ^ (string_of_typ (skip_typ implicit ty)) ^ "."

and skip_kind k knd =
  match (k, knd) with
      (0, _) -> knd
    | (n, PiKind(_,_,body))
    | (n, ImpKind(_, body)) -> skip_kind (k - 1) body
    | _ -> 
      Errormsg.error Errormsg.none ("Error: attempting to skip implicit argument(s) in non-function kind: "^(string_of_kind knd));
      knd

and skip_typ k ty =
  match k, ty with
      (0, _) -> ty
    | (n, PiType(_,_,t2)) 
    | (n, ImpType(_,t2)) -> skip_typ (n-1) t2
    | _ -> 
      Errormsg.error Errormsg.none ("Error: attempting to skip implicit argument(s) in non-function type: "^(string_of_typ ty));
      ty

and string_of_kind k =
  match k with
      PiKind(id,ty,body) -> 
        "({ " ^ (string_of_id id) ^ " : " ^ (string_of_typ ty) ^ "} " ^ (string_of_kind body) ^ ")"
    | ImpKind(ty,body) -> 
        "(" ^ (string_of_typ ty) ^ " -> " ^ (string_of_kind body) ^ ")"
    | Type(_) -> "type"

and string_of_typ ty =
  match ty with
      PiType(id,t1,t2) -> 
        "({ " ^ (string_of_id id) ^ " : " ^ (string_of_typ t1) ^ "} " ^ (string_of_typ t2) ^ ")"
    | AppType(t,tms) ->
        let tmlist =
          List.fold_left (fun s tm -> s ^ " " ^ (string_of_term tm)) "" tms
        in
        "(" ^ (string_of_id t) ^ " " ^ tmlist ^ ")"
    | ImpType(t1,t2) ->
        "(" ^ (string_of_typ t1) ^ " -> " ^ (string_of_typ t2) ^ ")"
    | IdType(id) ->
        string_of_id id

and string_of_term tm =
  match tm with
      AbsTerm(id,ty,body) -> 
        "([" ^ (string_of_id id) ^ " : " ^ (string_of_typ ty) ^"] " ^ (string_of_term body) ^ ")"
    | AppTerm(head, tms) -> 
        let tmlist = 
          List.fold_left (fun s t -> s ^ " " ^ (string_of_term t)) "" tms
        in
        "(" ^ (string_of_id head) ^ " " ^ tmlist ^ ")"
    | IdTerm(id) -> string_of_id id

and string_of_id id =
  match id with
      Const(n)
    | Var(n,_) 
    | LogicVar(n,_) -> n

let string_of_query (Query(_,id,ty)) =
  (string_of_id id) ^ " : " ^ (string_of_typ ty)

let string_of_solution (subst, disprs) =
  let string_of_subst subst =
    let rec string_of_subst_aux sub =
      match sub with
          ((id,tm) :: sub') ->
            (string_of_id id) ^ " = " ^ (string_of_term tm) ^ "\n" ^ (string_of_subst_aux sub')
        | [] -> ""
    in
    if subst = []
    then 
      ""
    else
      "The answer substitution:\n" ^ (string_of_subst_aux subst)
  in
  let string_of_disprs disprs =
    let rec string_of_disprs_aux prs =
      match prs with
          ((t1,t2) :: prs') ->
            "<" ^ (string_of_term t1) ^ ", " ^ (string_of_term t2) ^ ">\n" ^ (string_of_disprs_aux prs')
        | [] -> ""
    in
    if disprs = []
    then 
      ""
    else
      "The remaining disagreement pairs list:\n" ^ (string_of_disprs_aux disprs)
  in 
  (string_of_subst subst) ^ "\n" ^ (string_of_disprs disprs)

let get_typefam_implicit (TypeFam(_,_,_,_,_,_,p)) = p
let get_obj_implicit (Object(_,_,_,_,_,p)) = p


let get_typefam_name (TypeFam(name,_,_,_,_,_,_)) = string_of_id name
let get_obj_name (Object(name,_,_,_,_,_)) = string_of_id name
let get_id_name id =
  match id with
      Const(n) -> n
    | Var(n,_) -> n
    | LogicVar(n,_) -> n

let get_typefam_kind (TypeFam(_,k,_,_,_,_,_)) = k
let get_obj_typ (Object(_,t,_,_,_,_)) = t
