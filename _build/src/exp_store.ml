open Syntax
open Printf
exception TypeError of string
exception DivisionError of string
exception NotImplementedError of string
exception VariableNotDeclared of string

(** Stores variables that were created and their values. *)
let var_store = Hashtbl.create 100

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

let format_string s = String.sub s 1 (String.length s - 2)

(** Evaluates an expression and returns either: Const of int, MyString of string, MyBoolean of string. *)
let rec eval_exp expression= match expression with
	| Nothing -> Nothing
  	| Seq(e1, e2) -> 
  		let _ = eval_exp e1 in 
  		let v2 = eval_exp e2 in
   		v2
 	| While(e1, e2)-> 
  		let v1 = eval_exp e1 in 
  		if(not (is_bool v1)) then raise (TypeError "The condition inside the while loop expects a boolean.")
  		else if(get_value v1 == 1) then 
  			let _ = eval_exp e2 in eval_exp (While(e1, e2))
  			else Nothing
  	| If(e1, e2, e3) -> 
  		let v1 = eval_exp e1 in 
  		if(not (is_bool v1)) then raise (TypeError  "The condition inside the if statement expects a boolean.")	
  		else if(get_value v1 == 1) then eval_exp e2 else eval_exp e3
(*   	| Asg(e1, e2) -> 
  		let v1 = eval_exp e1 in 
  		let v2 = eval_exp e2 in 
  		Hashtbl.replace var_store v1 v2; 
  		Nothing *)
  	| Asg(Identifier(s), e2) -> 
  		let v2 = eval_exp e2 in Hashtbl.replace var_store s v2;
  		Nothing
  	| Deref(e) -> 
  		let v = eval_exp e in v
  	| Negate(e) -> 
  		let v = eval_exp e in 
  		if(not (is_bool v)) then raise (TypeError "The negation statament expects a boolean.")
  		else create_bool (not (get_value v == 1))
  	| Operator(op, e1, e2) -> 
    	(match op with
		    | Plus ->
			    let v1 = eval_exp e1 in 
			    let v2 = eval_exp e2 in 
			    if(is_int v1 && is_int v2) then Const(get_value v1 + get_value v2)		
			    else raise (TypeError "The operator + expected two integers.")
		    | Minus ->
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then Const(get_value v1 - get_value v2)		
			    else raise (TypeError "The operator - expected two integers.")		
		    | Times -> 
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then Const(get_value v1 * get_value v2)		
			    else raise (TypeError "The operator * expected two integers.")	
		    | Divide -> 
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then 
		       		(if(get_value v2 != 0) then Const(get_value v1 / get_value v2)
		       		else raise (DivisionError "The second expression of the division operation evaluates to 0."))
			    else raise (TypeError  "The operator \\ expected two integers.")
		    | Modulus -> 
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then
		       		if(get_value v2 != 0) then Const(get_value v1 mod get_value v2)
		       		else raise (DivisionError "The second expression of the modulus operation evaluates to 0.")
			    else raise (TypeError "The operator % expected two integers.")				
		    | Less -> 
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then create_bool (get_value v1 < get_value v2)		
			    else raise (TypeError "The operator < expected two integers.")		
		    | Leq ->
		      	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then create_bool (get_value v1 <= get_value v2)		
			    else raise (TypeError "The operator <= expected two integers.")			  	
		    | Greater -> 
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then create_bool (get_value v1 > get_value v2)		
			    else raise (TypeError "The operator > expected two integers.")	  	
		    | Geq -> 
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then create_bool (get_value v1 >= get_value v2)		
			    else raise (TypeError "The operator => expected two integers.")		  	
		    | Eq ->
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if((is_int v1 && is_int v2) || (is_bool v1 && is_bool v2)) then create_bool (get_value v1 = get_value v2)	
				else raise (TypeError "The operator == expected either two integers or two booleans.")
		    | Noteq ->
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if((is_int v1 && is_int v2) || (is_bool v1 && is_bool v2)) then create_bool (get_value v1 != get_value v2)	
				else raise (TypeError "The operator != expected either two integers or two booleans.")
		  	| And -> 
		  		let v1 = eval_exp e1 in
		  		if(not (is_bool v1)) then raise (TypeError "The operator && expected two booleans.")	
		  		else if(get_value v1 == 0) then v1
					 else let v2 = eval_exp e2 in 
					 if(not (is_bool v2)) then raise (TypeError "The operator && expected two booleans.")		
					else v2
		  	| Or -> 
		  		let v1 = eval_exp e1 in
		  		if(not (is_bool v1)) then raise (TypeError "The operator || expected two booleans.")
		  		else if(get_value v1 == 1) then v1
					 else let v2 = eval_exp e2 in 
					 if(not (is_bool v2)) then raise (TypeError "The operator || expected two booleans.")	
					else v2) 	
  	(* | Application(e, es) ->  *)
  	| MyString(_) | Const(_)| MyBoolean(_) -> expression
  	| Read -> 
  		let line = read_line () in
  		if (input_is_int line) then Const(int_of_string line) 
  		else if(line == "true" || line == "false") then MyBoolean(line)
  		else MyString(line)
   	| Print(e) -> 
  		let v = eval_exp e in 
  		(match v with 
  			| Const(i) -> printf "%d" i
  			| MyString(s) | MyBoolean(s) -> printf "%s" (format_string s)
  			| _ -> raise (TypeError "The print function can only print integer, booleans or string."));
  		Nothing
  	| Identifier(s) -> 
  		(try Hashtbl.find var_store s with 
  			| Not_found -> raise (VariableNotDeclared "The variable was not declared."))
  	| Let(s, e1, e2) -> 
  		let v1 = eval_exp e1 in
  		Hashtbl.add var_store s v1;
  		let v2 = eval_exp e2 in
  		Hashtbl.remove var_store s; 
  		v2
  	| New(s, e1, e2) ->
  		let v1 = eval_exp e1 in 
  		Hashtbl.add var_store s v1; 
  		let v2 = eval_exp e2 in
  		v2
  	| _  -> raise (NotImplementedError "This type of expression cannot be evaluated yet!")

let eval = function
	| Nothing -> "nothing"
	| MyString(s) -> format_string s
	| Const(i) -> string_of_int i
	| MyBoolean(s) -> s
	| _ -> eprintf "The result was wrong."; exit(-1)

let eval_with_error expression = 
	try eval_exp expression with 
	  	| TypeError msg -> prerr_string ("Error: " ^ msg ^ "\n");                       
  						 exit (-1)   
  		| DivisionError msg -> prerr_string ("Error: " ^msg ^ "\n");                   
  					   exit (-1)
  		| NotImplementedError msg -> prerr_string ("Error: " ^msg ^ "\n");                   
  					  	exit (-1)
		| VariableNotDeclared msg -> prerr_string ("Error: " ^msg ^ "\n");                   
  					  	exit (-1)

let print_output result =  printf "Output: %s" result

let eval_function (name, parameters, content) = 
	printf "Function %s, with %d parameters: \n" name (List.length parameters);
	eval_with_error content
	|> eval
	|> print_output

let rec eval_functions functions = match functions with 
	| [] -> printf ""; printf "\n"
	| f::fs -> eval_function f; printf "\n"; eval_functions fs

let eval_program program = match program with
	| [("", [], Nothing)] -> printf "Nothing"; printf "\n"
	| _ -> eval_functions program
