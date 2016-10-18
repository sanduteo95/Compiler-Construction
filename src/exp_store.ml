(** Contains code for evaluating an expression.  *)
open Syntax
open Printf
open Type_checking

exception TypeError of string
exception DivisionError of string
exception NotImplementedError of string
exception VariableDeclaration of string

(** Stores variables that were created and their values. *)
let var_store = Hashtbl.create 100

let apply_operator op v1 v2 = match op with
	| Plus ->
		 if(is_int v1 && is_int v2) then Const(get_value v1 + get_value v2)		
		 else raise (TypeError "The operator + expected two integers.")
	| Minus ->
		if(is_int v1 && is_int v2) then Const(get_value v1 - get_value v2)		
		else raise (TypeError "The operator - expected two integers.")		
	| Times -> 
		if(is_int v1 && is_int v2) then Const(get_value v1 * get_value v2)		
		else raise (TypeError "The operator * expected two integers.")	
	| Divide -> 
		if(is_int v1 && is_int v2) then 
		   (if(get_value v2 != 0) then Const(get_value v1 / get_value v2)
		    else raise (DivisionError "The second expression of the division operation evaluates to 0."))
		else raise (TypeError  "The operator \\ expected two integers.")
	| Modulus -> 
		if(is_int v1 && is_int v2) then
		    if(get_value v2 != 0) then Const(get_value v1 mod get_value v2)
		    else raise (DivisionError "The second expression of the modulus operation evaluates to 0.")
		else raise (TypeError "The operator % expected two integers.")				
	| Less -> 
		if(is_int v1 && is_int v2) then create_bool (get_value v1 < get_value v2)		
		else raise (TypeError "The operator < expected two integers.")		
	| Leq ->
		if(is_int v1 && is_int v2) then create_bool (get_value v1 <= get_value v2)		
		else raise (TypeError "The operator <= expected two integers.")			  	
	| Greater -> 
		if(is_int v1 && is_int v2) then create_bool (get_value v1 > get_value v2)		
		else raise (TypeError "The operator > expected two integers.")	  	
	| Geq -> 
		if(is_int v1 && is_int v2) then create_bool (get_value v1 >= get_value v2)		
		else raise (TypeError "The operator => expected two integers.")		  	
	| Eq ->
		if((is_int v1 && is_int v2) || (is_bool v1 && is_bool v2)) then create_bool (get_value v1 = get_value v2)	
		else raise (TypeError "The operator == expected either two integers or two booleans.")
	| Noteq ->
		if((is_int v1 && is_int v2) || (is_bool v1 && is_bool v2)) then create_bool (get_value v1 != get_value v2)	
		else raise (TypeError "The operator != expected either two integers or two booleans.")
	| And -> 
		if(not (is_bool v1) || not (is_bool v2)) then raise (TypeError "The operator && expected two booleans.")	
		else create_bool (get_value v1 == 1 && get_value v2 == 1)
	| Or -> 
		if(not (is_bool v1) || not (is_bool v2)) then raise (TypeError "The operator || expected two booleans.")
		else create_bool (get_value v1 == 1 || get_value v2 == 1)

(** Evaluates an expression and returns either: Const of int, MyString of string, MyBoolean of string. *)
let rec eval_exp expression= match expression with
	| MyString(_) | Const(_)| MyBoolean(_) | Identifier(_) | Nothing -> expression
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
  	| Asg(Let(s, e, If(e1, Deref(Identifier(s1)), Deref(Identifier(s2)))), e2) ->
  		let v = eval_exp e in
  		Hashtbl.add var_store s v;
  		let v1 = eval_exp e1 in
  		let v2 = eval_exp e2 in
  		if(not (is_bool v1)) then raise (TypeError  "The condition inside the if statement expects a boolean.")	
  		else if(get_value v1 == 1) then Hashtbl.replace var_store s1 v2 else Hashtbl.replace var_store s2 v2;
  		Hashtbl.remove var_store s;
  		Nothing
  	| Asg(If(e1, Deref(Identifier(s1)), Deref(Identifier(s2))), e2) ->
  		let v1 = eval_exp e1 in
  		let v2 = eval_exp e2 in
  		if(not (is_bool v1)) then raise (TypeError  "The condition inside the if statement expects a boolean.")	
  		else if(get_value v1 == 1) then Hashtbl.replace var_store s1 v2 else Hashtbl.replace var_store s2 v2;
  		Nothing
  	| Asg(Identifier(s), e2) -> 
  		let v2 = eval_exp e2 in Hashtbl.replace var_store s v2;
  		Nothing
  	| Deref(e) -> 
  		let v = eval_exp e in 
  		(match v with 
  			| Identifier(s) -> 
  			  	(try Hashtbl.find var_store s with 
  					| Not_found -> raise (VariableDeclaration "The variable was not declared."))
  			| _ -> raise (TypeError "The expression cannot be dereferenced."))
  	| Negate(e) -> 
  		let v = eval_exp e in 
  		if(not (is_bool v)) then raise (TypeError "The negation statament expects a boolean.")
  		else create_bool (not (get_value v == 1))
  	| Operator(op, e1, e2) -> apply_operator op (eval_exp e1 ) (eval_exp e2)
  	| Read -> 
  		let line = read_line () in
  		if (input_is_int line) then Const(int_of_string line) 
  		else if(line == "true" || line == "false") then MyBoolean(line)
  		else MyString(line)
   	| Print(e) -> 
  		(match (eval_exp e) with 
  			| Const(i) -> printf "%d" i
  			| MyString(s) | MyBoolean(s) -> printf "%s" (String.sub s 1 (String.length s - 2))
  			| _ -> raise (TypeError "The print function can only print integers, booleans or strings."));
  		Nothing
  	| Let(s, e1, e2) -> 
  		Hashtbl.add var_store s (eval_exp e1);
  		let v = eval_exp e2 in
  		Hashtbl.remove var_store s; 
  		v
  	| New(s, e1, e2) ->
  		(try Hashtbl.find var_store s with 
  			| Not_found -> Hashtbl.add var_store s (eval_exp e1); eval_exp e2
  			| _ -> raise (VariableDeclaration "The variable was already declared."))
  	| _  -> raise (NotImplementedError "This type of expression cannot be evaluated yet!")