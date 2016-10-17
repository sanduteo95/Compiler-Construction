open Syntax
open Lexing 
open Printf
open Exp_test

let store = Hashtbl.create 100

let input_is_int s =
	try ignore (int_of_string s); true
	with _ -> false

let is_int = function
	| Const(_) -> true
	| _ -> false

let is_bool = function
	| MyBoolean(_) -> true
	| _ -> false

let get_value = function
	| Const(i) -> i
	| MyBoolean(s) -> 
		(match s with 
			| "true" -> 1
			| "false" -> 0
			| _ -> failwith "A boolean can only take two values: true or false.")
	| _ -> failwith "You can only get the balue of an int or a boolean."

let create_bool = function
	| true -> MyBoolean("true")
	| false -> MyBoolean("false")

let rec eval_exp expression = match expression with
  	| Seq(e1, e2) -> 
  		let _ = eval_exp e1 in 
  		let v2 = eval_exp e2 in
   		v2
(*  	| While(e1, e2) as exp-> 
  		let v1 = eval_exp e1 in 
  		(match v1 with			
  			| true | false -> 
		  		if(v1 == true) then 
			  		let v2 = eval_exp e2 in 
			  		eval_exp exp
		  	| _ -> failwith "An algebraic operation is not allowed in a while loop. ") *)
  	| If(e1, e2, e3) -> 
  		let v1 = eval_exp e1 in 
  		if(not (is_bool v1)) then failwith "Type problem in if."	
  		else if(get_value v1 == 1) then eval_exp e2 else eval_exp e3
(*   	| Asg(e1, e2) -> 
  		let v1 = eval_exp e1 in 
  		let v2 = eval_exp e2 in 
  		Hashtbl.replace store v1 v2; 
  		v2 *)
  	| Asg(Identifier(s), e2) -> 
  		let v2 = eval_exp e2 in Hashtbl.replace store s v2;
  		 v2
  	| Deref(e) -> 
  		let v = eval_exp e in v
  	| Negate(e) -> 
  		let v = eval_exp e in 
  		if(not (is_bool v)) then failwith "Type problem in negate."
  		else create_bool (not (get_value v == 1))
  	| Operator(op, e1, e2) -> 
    	(match op with
		    | Plus ->
			    let v1 = eval_exp e1 in 
			    let v2 = eval_exp e2 in 
			    if(is_int v1 && is_int v2) then Const(get_value v1 + get_value v2)		
			    else failwith "Type problem in plus."	
		    | Minus ->
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then Const(get_value v1 - get_value v2)		
			    else failwith "Type problem in minux."			
		    | Times -> 
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then Const(get_value v1 * get_value v2)		
			    else failwith "Type problem in times."		
		    | Divide -> 
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then Const(get_value v1 / get_value v2)		
			    else failwith "Type problem in divide."			
		    | Modulus -> 
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then Const(get_value v1 mod get_value v2)		
			    else failwith "Type problem in modulus."				
		    | Less -> 
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then create_bool (get_value v1 < get_value v2)		
			    else failwith "Type problem in less."			
		    | Leq ->
		      	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then create_bool (get_value v1 <= get_value v2)		
			    else failwith "Type problem in leq."			  	
		    | Greater -> 
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then create_bool (get_value v1 > get_value v2)		
			    else failwith "Type problem in greater."			  	
		    | Geq -> 
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then create_bool (get_value v1 >= get_value v2)		
			    else failwith "Type problem in gew."			  	
		    | Eq ->
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then create_bool (get_value v1 = get_value v2)		
			    else failwith "Type problem in eq."	
		    | Noteq ->
		       	let v1 = eval_exp e1 in 
		       	let v2 = eval_exp e2 in 
		       	if(is_int v1 && is_int v2) then create_bool (get_value v1 != get_value v2)		
			    else failwith "Type problem in noteq."	
		  	| And -> 
		  		let v1 = eval_exp e1 in
		  		if(not (is_bool v1)) then failwith "Type problem in and."	
		  		else if(get_value v1 == 0) then v1
					 else let v2 = eval_exp e2 in 
					 if(not (is_bool v2)) then failwith "Type problem in and."	
					else v2
		  	| Or -> 
		  		let v1 = eval_exp e1 in
		  		if(not (is_bool v1)) then failwith "Type problem in and."	
		  		else if(get_value v1 == 1) then v1
					 else let v2 = eval_exp e2 in 
					 if(not (is_bool v2)) then failwith "Type problem in and."	
					else v2)  		
  	(* | Application(e, es) ->  *)
  	| MyString(_) | Const(_)| MyBoolean(_) -> expression
  	| Read -> 
  		let line = read_line () in
  		if (input_is_int line) then Const(int_of_string line) 
  		else if(line == "true" || line == "false") then MyBoolean(line)
  		else MyString(line)
(*   	| Print(e) -> 
  		let v = eval_exp e in v *)
  	| Identifier(s) -> 
  		Hashtbl.find store s
  	| Let(s, e1, e2) -> 
  		let v1 = eval_exp e1 in
  		Hashtbl.add store s v1;
  		let v2 = eval_exp e2 in
  		Hashtbl.remove store s; 
  		v2
  	| New(s, e1, e2) ->
  		let v1 = eval_exp e1 in 
  		Hashtbl.add store s v1; 
  		let v2 = eval_exp e2 in
  		v2
  	| _  -> failwith "This expression cannot be evaluated!"

let eval expression = 
	match (eval_exp expression) with 
		| MyString(s) -> printf "The expression evaluates to %s. " s
		| Const(i) -> printf "The expression evaluates to %d. " i
		| MyBoolean(s) -> 
			(match s with 
				| "true" -> printf "The expression evaluates to %b. " true
				| "false" -> printf "The expression evaluates to %b. " false
				| _ -> eprintf "The result was wrong.")
		| _ -> eprintf "The result was wrong."

let eval_function (name, parameters, content) = eval content

let rec eval_functions functions = match functions with 
	| [] -> printf ""; printf "\n"
	| f::fs -> eval_function f; printf "\n"; eval_functions fs

let eval_program program = match program with
	| [("", [], Nothing)] -> printf "Nothing"; printf "\n"
	| _ -> eval_functions program

let read_file_and_print_result =   
  let ic = open_in filename in read_to_empty ic (Buffer.create 1)
 	|> Buffer.contents  
 	|> Lexing.from_string  
 	|> parse_with_error  
 	|> eval_program;
  close_in ic