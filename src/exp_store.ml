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

(** Gets the location in memory.  *)
let get_location = function
  | Pointer(location) -> location
  | _ -> raise (TypeError ("You can't dereference something that isn't a pointer", ""))

(** Creates locations for function arguments. *)
let rec create_locations n aux = match n with
  | 0 -> aux
  | m -> create_locations (n-1) (aux@[newref()])

let rec helper store aux es = match es with 
  | [] -> aux
  | e::es ->
    let location = get_location e in
    let v = access store location in helper store (aux@[v]) es

let get_tuple_content store tuple = match tuple with 
  | Tuple(es) -> let new_es = helper store [] es in Tuple(new_es)
  | _ -> failwith "Only tuples allowed."

let rec get_pointer_values store aux = function
  | [] -> aux
  | p::ps -> 
    (match p with
      | Pointer(location) -> get_pointer_values store (aux@[(location, access store location)])ps
      | _ -> get_pointer_values store aux ps)
