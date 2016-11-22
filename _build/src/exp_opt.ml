(** Contains code for optimising an expression.  *)
open Syntax
open List
open Printf
open Exp_store

let function_store = Hashtbl.create 100
let global_max_loop = 10
let unrolled = ref false

let break = ref false
let continue = ref false

let rec convert = function
	| Null -> MyNull
  	| Integer(i) -> MyInteger(i)
  	| Float(f) -> MyFloat(f)
  	| Boolean(b) -> MyBoolean(b)
  	| String(s) -> MyString(s)
  	| Unit -> Nothing
  	| Tuple(es) -> MyTuple(map convert es)
	| Pointer(_) -> failwith "Can't have a dereferenced pointer in an operation."
  	| Fundef(s, e) -> Lambda (s, e)

let get_boolean = function
	| MyBoolean(b) -> b
	| _ -> failwith "Not a boolean."

let get_integer = function
	| MyInteger(i) -> i
	| _ -> failwith "Not an integer."

let is_primary = function
	| MyNull | MyString(_) | MyInteger(_) | MyFloat(_) | MyBoolean(_) -> true
	| _ -> false

let is_id = function
	| Identifier(_) -> true
	| _ -> false

let rec get_ids aux = function
	| [] -> aux
	| Deref(Identifier(s))::rest -> get_ids (aux@[s]) rest
	| _::rest -> get_ids aux rest

let rec contains_print = function
  	| MyNull | MyString(_) | MyInteger(_) | MyFloat(_) | MyBoolean(_) | Nothing | MyTuple(_) | Identifier(_) | Deref(_) | Read | Break | Continue -> false
  	| Negate(e) | Application(e, _) | Lambda(_, e) -> contains_print e
  	| Operator(_, e1, e2) | Let(_, e1, e2) | New(_, e1, e2) | Asg(e1, e2) | Seq(e1, e2) | While(e1, e2) -> contains_print e1 || contains_print e2
  	| For(_, e1, e2, e3) | If(e1, e2, e3) -> contains_print e1 || contains_print e2 || contains_print e3
	| Print(_) -> true

let rec contains_id_arg s expression = match expression with
	| Deref(Identifier(t)) -> String.equal t s
	| _ -> contains_id s expression
and contains_id s = function
	| Lambda(_, _) | MyNull | MyString(_) | MyInteger(_) | MyFloat(_) | MyBoolean(_) | Nothing | MyTuple(_) | Deref(_) | Read | Break | Continue -> false
	| Negate(e) | Print(e) -> contains_id s e
	| Application(_, es) -> fold_left (fun a e -> (contains_id_arg s e) || a) false es
	| Operator(_, e1, e2) | Let(_, e1, e2) | New(_, e1, e2) | Asg(e1, e2) | Seq(e1, e2) -> contains_id s e1 || contains_id s e2
	| While(e1, e2) -> true
	| For(_, e1, e2, e3) | If(e1, e2, e3) -> contains_id s e1 || contains_id s e2 || contains_id s e3
	| Identifier(t) -> String.equal t s

let rec opt_operator store expression = match expression with
  	| Operator(op, Deref(Identifier(_)), Deref(Identifier(_))) | Operator(op, Deref(Identifier(_)), _) | Operator(op, _, Deref(Identifier(_))) -> expression
	| Operator(op, Identifier(a), Identifier(b)) ->
		let val_a = Hashtbl.find store a in
		let val_b = Hashtbl.find store b in
		Operator(op, val_a, val_b)
	| Operator(op, Identifier(a), exp) ->
		let val_a = Hashtbl.find store a in
		opt_operator store (Operator(op, val_a, exp))
	| Operator(op, exp, Identifier(a)) ->
		let val_a = Hashtbl.find store a in
		opt_operator store (Operator(op, exp, val_a))
  	| Operator(op, e1, e2) ->
		if(is_primary e1 && is_primary e2) then convert(Exp_eval.eval_operator [] (Hashtbl.create 100) op e1 e2)
		else
	  		let v1 = if(is_primary e1) then e1 else opt_operator store e1 in
	  		let v2 = if(is_primary e2) then e2 else opt_operator store e2 in
	  		Operator(op, v1, v2)
  	| _ -> expression

let rec create_nested_new ps vs a = match (ps, vs) with
	| [], [] -> a
	| p::ps, v::vs -> New(p, v, create_nested_new ps vs a)
	| _, _ -> failwith "The number of values should be equal to the number of parameters."

let rec loop_unroll max_loop store s i n e =
	let v = opt_exp max_loop true (extend store s (MyInteger(i))) e in
	if(!break == true) then v
	else
		(continue := false;
		if(i<n && max_loop>1) then
		   	Seq(v, loop_unroll (max_loop-1) store s (i+1) n e)
		else v)
and opt_exp max_loop is_function store expression = match expression with
  	| Identifier(_) | MyNull | MyString(_) | MyInteger(_) | MyFloat(_) | MyBoolean(_) | Nothing -> expression
  	| MyTuple(es) -> MyTuple(map (opt_exp max_loop is_function store) es)
  	| Deref(e) ->
		(match opt_exp max_loop is_function store e with
	  		| Identifier(s) ->
				(try Hashtbl.find store s with
				 	| Not_found ->
						if(is_function) then raise (Exp_errors.FunctionError "")
						else Deref(Identifier(s)))
			| Deref(Identifier(s)) ->
				(try Hashtbl.find store s with
				 	| Not_found ->
						if(is_function) then raise (Exp_errors.FunctionError "")
						else Deref(Deref(Identifier(s))))
			| v -> Deref(v))
  	| Let(s, e1, e2) ->
		let v1 = opt_exp max_loop is_function store e1 in
		if(contains_print v1) then Let(s, v1, opt_exp max_loop is_function store e2)
		else
			(Hashtbl.add store s v1;
		  	let v2 = opt_exp max_loop is_function store e2 in
		  	Hashtbl.remove store s;
		  	v2)
	| New(s, e1, e2) ->
		(match opt_exp max_loop is_function store e1 with
	  		| Read -> New(s, Read, opt_exp max_loop is_function store e2)
	  		| v1 ->
				if(contains_print v1) then New(s, v1, opt_exp max_loop is_function store e2)
				else
					(Hashtbl.add store s v1;
					let v2 = opt_exp max_loop is_function store e2 in
					if(is_primary v2 || not (contains_id s v2)) then v2
					else New(s, v1, v2)))
	| Asg(e1, e2) ->
		(match opt_exp max_loop is_function store e1 with
			| Identifier(s) ->
				(* let v2 = opt_exp max_loop is_function store e2 in
				if(is_primary v2 || is_id v2) then
					(Hashtbl.replace store s v2;
					if(is_function) then Asg(Identifier(s), v2)
					else Nothing)
				else Asg(Identifier(s), v2) *)
				let v2 = opt_exp max_loop is_function store e2 in
				Hashtbl.replace store s v2;
				if(is_function) then Asg(Identifier(s), v2)
				else Nothing
			| v1 -> Asg(v1, opt_exp max_loop is_function store e2))
  	| Seq(e1, e2) ->
		(match opt_exp max_loop is_function store e1 with
			| Nothing | Break | Continue->
				if(!break == true || !continue == true) then Nothing
				else
					let v = opt_exp max_loop is_function store e2 in
					if(!break == true && max_loop == global_max_loop) then Break else if(!continue == true && max_loop == global_max_loop) then Continue else v
			| v1 ->
				if(!break == true || !continue == true) then v1
				else
					if !unrolled then
					 	if(!break == true || !continue == true) then v1 else Seq(v1, e2)
					else
						let v2 = opt_exp max_loop is_function store e2 in
						if(!break == true || !continue == true) then v1 else Seq(v1, v2))
  	| Negate(e) ->
		(match opt_exp max_loop is_function store e with
			| MyBoolean(b) -> MyBoolean(not b)
			| v -> Negate(v))
  	| Operator(op, e1, e2) ->
		(match op with
			| And ->
				let v1 = opt_exp max_loop is_function store e1 in
				if(is_primary v1) then
					if(get_boolean v1) then opt_exp max_loop is_function store e2 else v1
				else opt_operator store (Operator(op, v1, opt_exp max_loop is_function store e2))
			| Or ->
				let v1 = opt_exp max_loop is_function store e1 in
				if(is_primary v1) then
					if(get_boolean v1) then v1 else opt_exp max_loop is_function store e2
				else opt_operator store (Operator(op, v1, opt_exp max_loop is_function store e2))
			| _ ->
				let v1 = opt_exp max_loop is_function store e1 in
				let v2 = opt_exp max_loop is_function store e2 in
				opt_operator store (Operator(op, v1, v2)))
  	| For(s, e1, e2, e3) ->
		let v1 = opt_exp max_loop is_function store e1 in
		(match v1, opt_exp max_loop is_function store e2 with
			 | MyInteger(i), MyInteger(n) ->
			 	let e = loop_unroll max_loop store s i n e3 in
				(unrolled := true;
				if(i+global_max_loop<n) then
					if(!break == true) then (break := false; Let(s, MyInteger(i), opt_exp 0 true store e))
					else
						(continue := false;
			 			Let(s, MyInteger(i), Seq(e, For(s, MyInteger(i+global_max_loop), MyInteger(n), e3))))
				else
					let v = opt_exp 0 true store e in
					if(!break == true) then (break := false; Nothing) else (continue := false; v);)
			 | _, _ -> For(s, e1, e2, e3))
  	| While(e1, e2) ->
		let v1 = opt_exp global_max_loop is_function store e1 in
		if(max_loop > 0) then
			if(is_primary v1) then
				if(get_boolean v1) then
					let v2 = opt_exp 0 is_function store e2 in
					if(!break == true) then (break := false; v2)
					else (continue := false; Seq(v2, opt_exp (max_loop-1) is_function store (While(e1, e2))))
					(* opt_exp 0 is_function store (Seq(v2, opt_exp (max_loop-1) is_function store (While(e1, e2)))) *)
				else Nothing
			else
				If(v1, Seq(opt_exp global_max_loop is_function store e2, While(e1, e2)), Nothing)
		else While(e1, e2)
  	| If(e1, e2, e3) ->
		(match opt_exp max_loop is_function store e1 with
		  | MyBoolean(b) -> if b then opt_exp max_loop is_function store e2 else opt_exp max_loop is_function store e3
		  | v1 ->
		  	let v2 = opt_exp max_loop is_function store e2 in
			let v3 = opt_exp max_loop is_function store e3 in
			if(v2 == Nothing && v3 == Nothing) then Nothing
			else If(v1, v2, v3))
  	| Application(e, ps) ->
		let vs = map (fun p -> let new_p = opt_exp max_loop is_function store p in
			 (match p, new_p with
				 | Identifier(_), Identifier(s) ->
				 	(try access store s with
						| Exp_errors.VariableDeclaration (_, _) -> Identifier(s))
				 | _, _ -> new_p)) ps in
		if(fold_left (fun a v -> contains_print v || a) false vs) then Application(opt_exp max_loop true (Hashtbl.create 100) e, ps)
		else (match opt_exp max_loop is_function store e with
			  	| Identifier(s) ->
					if(Hashtbl.mem function_store s) then
				  		(match access function_store s with
					 		| (s, ps, expression) ->
								try opt_exp max_loop true (Hashtbl.create 100) (create_nested_new ps vs expression) with
									| Exp_errors.FunctionError msg -> Application(Identifier(s), vs)
					 		| _ -> Application(Identifier(s), vs))
					else (match access store s with
							| Application(expression, pps) -> opt_exp max_loop is_function store (Application(expression, ps@pps))
							| _ -> Application(Identifier(s), vs))
			  	| Lambda(pps, expression) ->
					if(length pps > length vs) then
				  		let (ps1, ps2) = split [] (length vs) pps in
						Application(Lambda(ps2, opt_exp max_loop is_function (extend_list (Hashtbl.create 100) (combine ps1 vs)) expression), [])
					else
				  		opt_exp max_loop true (Exp_store.extend_list (Hashtbl.create 100) (combine pps vs)) expression
			  	| v -> Application(v, vs))
  	| Lambda(p, e) -> Lambda(p, e)
  	| Read -> Read
  	| Print(e) -> let v = opt_exp max_loop is_function store e in Print(v)
	| Break -> break := true; Nothing
	| Continue -> continue := true; Nothing

(** Function evaluates each individual function in the program. *)
let rec opt_fundef = function
  	| [] -> []
  	| (s, ps, expression)::[] -> [(s, ps, opt_exp global_max_loop false (Hashtbl.create 100) expression)]
  	| (s, ps, expression)::program -> let _ = extend function_store s ((s, ps, expression)) in (s, ps, expression) :: opt_fundef program

let opt program =
	break := false;
	continue := false;
	opt_fundef program
