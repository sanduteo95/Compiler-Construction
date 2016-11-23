(** Contains methods used for type checking and creating the custom types. *)
open Syntax
open Exp_errors
open List
open Printf

(** Checks if user input is an integer. *)
let input_is_int s =
	try ignore (int_of_string s); true
	with _ -> false

(** Checks if user input is a float. *)
let input_is_float s =
	try ignore (float_of_string s); true
	with _ -> false

(** Gets the value of a boolean. *)
let get_boolean = function
	| Boolean(b) -> b
	| _ -> raise (TypeError ("The negation statament expects a boolean", ""))

(** Gets the operator used for integers.  *)
let get_int_operator op = match op with
	| Plus -> ( + )
	| Minus -> ( - )
	| Times -> ( * )
	| Divide -> ( / )
	| Modulus -> ( mod )
	| _ -> failwith "This case is never reached."

(** Gets the operator used for integers and floats.  *)
let get_mix_operator op = match op with
	| Less -> ( < )
	| Leq -> ( <= )
	| Greater -> ( > )
	| Geq -> ( >= )
	| Eq -> ( == )
	| Noteq -> ( != )
	| _ -> failwith "This case is never reached."

(** Gets the operator used for floats.  *)
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
		let operator = get_mix_operator op in
		(operator v1 v2) && tuple_mix_operator op (Tuple(es1)) (Tuple(es2))
	| Tuple([]), Tuple(_) | Tuple(_), Tuple([]) -> raise (TypeError ("The two tuples have different sizes", ""))
	| _ -> failwith "This case is never reached."

(** Used to print an expression. *)
let print_exp = function
	| Integer(i) -> printf "%d\n" i
	| Float(f) -> printf "%f\n" f
	| String(s) -> printf "%s\n" (String.sub s 1 (String.length s - 2))
	| Boolean(b) -> printf "%b\n" b
	| _ -> raise (TypeError ("The print function can only print integers, booleans or strings", "" ))

(** Used to read an expression. *)
let read_exp _ =
	let line  = read_line () in
	if (input_is_int line) then Integer(int_of_string line)
	else
	  if (input_is_float line) then Float(float_of_string line)
	  else
		if(line == "true" || line == "false") then Boolean(line == "true")
		else String(line)