(** Contains methods used for type checking and creating the custom types. *)
open Syntax
open Exp_errors
open List 

(** Checks if user input is an integer. *)
let input_is_int s =
	try ignore (int_of_string s); true
	with _ -> false

(** Checks if user input is a float. *)
let input_is_float s =
	try ignore (float_of_string s); true
	with _ -> false

(** Checks if a variable is a boolean. *)
let is_bool = function
	| Boolean(_) -> true
	| _ -> false

(** Gets the value of a boolean. *)
let get_boolean = function
	| Boolean(b) -> b
	| _ -> failwith "This case is never reached."

let get_int_operator op = match op with 
	| Plus -> ( + )
	| Minus -> ( - )
	| Times -> ( * )
	| Divide -> ( / )
	| Modulus -> ( mod )
	| _ -> failwith "This case is never reached."

let get_mix_operator op = match op with
	| Less -> ( < ) 
	| Leq -> ( <= )
	| Greater -> ( > )
	| Geq -> ( >= )
	| Eq -> ( == )
	| Noteq -> ( != )
	| _ -> failwith "This case is never reached."

let get_float_operator op = match op with 
	| Plus -> ( +. )
	| Minus -> ( -. )
	| Times -> ( *. )
	| Divide -> ( /. )
	| Modulus -> raise (TypeError ("There is no modulus operator for floats", ""))
	| _ -> failwith "This case is never reached."

(** Applies prefix operators of the form int->int->bool or tuple->tuple->bool*)
let rec tuple_mix_operator op v1 v2 = match v1, v2 with
	| Tuple([]), Tuple([]) -> true
	| Tuple(e1::es1), Tuple(e2::es2) -> 
		let e =
		(match e1, e2 with 
			| Integer v1, Integer v2 -> let operator = get_mix_operator op in Boolean(operator v1 v2)
			| Float v1, Float v2 -> let operator = get_mix_operator op in Boolean(operator v1 v2)
			| String v1, String v2 -> let operator = get_mix_operator op in Boolean(operator v1 v2)
			| _, _ -> raise (TypeError ("Tuples can only hold integers, strings and floats and must match up", "")))
		in
		let v = 
		(match e with 
			| Boolean(true) -> true
			| Boolean(false) -> false
			| _ -> failwith "Only true or false.")
		in v && tuple_mix_operator op (Tuple(es1)) (Tuple(es2))
	| Tuple([]), Tuple(_) | Tuple(_), Tuple([]) -> raise (TypeError ("The two tuples have different sizes", ""))
	| _ -> failwith "This case is never reached."

(** Gets the type of a variable. *)
let get_type v = match v with
	| Null -> -1
	| Integer(_) -> 0
	| Float(_) -> 1
	| Boolean(_) -> 2
	| String(_) -> 3
	| Unit -> 4
	| Pointer(_) -> failwith "It's a pointer!"
	| _ -> raise (TypeError ("The only accepted types are null, int, float, bool, string, tuples or unit", ""))
