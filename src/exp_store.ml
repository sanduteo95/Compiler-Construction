(** Contains code for evaluating an expression.  *)
open Exp_errors
open Syntax

(** The global address used for the store. *)
let addr_gbl = ref 0

(** Creates a new location in memory. *)
let newref() =
  	addr_gbl := !addr_gbl + 1;
  	!addr_gbl

(** Looks up the value of the variable in the given environment. *)
let rec lookup s env = match env with
  	 | [] -> raise (VariableDeclaration ("This variable was not initialised: ", s))
  	 | (var, value)::env -> if (String.equal s var) then value else lookup s env

let rec lookup2 s env = match env with
     | [] -> Nothing
     | (var, value)::env -> if (String.equal s var) then value else lookup s env

(** Adds a new location to the store. *)
let extend store location value =
  	Hashtbl.add store location value; store

let rec extend_list store lvs = match lvs with
	| [] -> store
	| (location, value) :: lvs -> let store = extend store location value in extend_list store lvs

(** Accesses the variable stored at the given location. *)
let access store location =
  	try Hashtbl.find store location with
  		| Not_found -> raise (VariableDeclaration ("The variable was not initialised", ""))

(** Deleted the location of the variable. *)
let delete store location =
  	Hashtbl.remove store location

let rec delete_list store ls = match ls with
    | [] -> ()
    |location :: ls -> delete store location; delete_list store ls

(** Creates locations for function arguments. *)
let rec create_locations n aux = match n with
  | 0 -> aux
  | m -> create_locations (n-1) (aux@[newref()])

let rec get_pointer_values store aux = function
  | [] -> aux
  | p::ps ->
    (match p with
      | Pointer(location) -> get_pointer_values store (aux@[(location, access store location)])ps
      | _ -> get_pointer_values store aux ps)

(** Function splits a list into two lists, one with n elements, one with the rest of them. *)
let rec take aux n ls =  match (n, ls) with
  | (0, ls) -> aux
  | (n, l::ls) -> take (aux@[l]) (n-1) ls
  | (n, []) -> failwith "Impossible."

let rec split aux n ls =  match (n, ls) with
  | (0, ls) -> (aux, ls)
  | (n, l::ls) -> split (aux@[l]) (n-1) ls
  | (n, []) -> failwith "Impossible."
