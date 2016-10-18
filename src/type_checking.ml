(** Contains methods used for type checking and creating the custom types. *)
open Syntax

(** Checks if user input is an integer. *)
let input_is_int s =
	try ignore (int_of_string s); true
	with _ -> false

(** Checks if a variable is an integer. *)
let is_int = function
	| Const(_) -> true
	| _ -> false

(** Checks if a variable is a boolean. *)
let is_bool = function
	| MyBoolean(_) -> true
	| _ -> false

(** Gets the value of a variable. *)
let get_value = function
	| Const(i) -> i
	| MyBoolean(s) -> 
		(match s with 
			| "true" -> 1
			| "false" -> 0
			| _ -> failwith "A boolean can only take two values: true or false.")
	| _ -> failwith "You can only get the value of an int or a boolean."

(** Creates a new boolean. *)
let create_bool = function
	| true -> MyBoolean("true")
	| false -> MyBoolean("false")
