(** Contains code for optimising an expression.  *)
open Syntax
open List
open Printf
open Exp_store

(** Stores function definition. *)
let function_store = Hashtbl.create 100

(** Used for memoisation, with a maximum number of saved functions.*)
let max_memo = 1000
let memo_store = Hashtbl.create 100

(** The maximum number of unrollings. *)
let global_max_loop = 10

(** Variables used to tell if there is a break or a continue statement. *)
let break = ref false
let continue = ref false

(** Variable used to tell if the loop has been transformed to sequences. *)
let totally_unrolled = ref true

(** Converts return values to normal values. *)
let rec convert = function
	| Null -> MyNull
  	| Integer(i) -> MyInteger(i)
  	| Float(f) -> MyFloat(f)
  	| Boolean(b) -> MyBoolean(b)
  	| String(s) -> MyString(s)
  	| Unit -> Nothing
  	| Tuple(es) -> MyTuple(map convert es)
  	| Fundef(s, e) -> Lambda (s, e)
	| Pointer(_) -> failwith "Can't have a dereferenced pointer in an operation."

(** Gets the value inside a boolean. *)
let get_boolean = function
	| MyBoolean(b) -> b
	| _ -> failwith "Not a boolean."

(** Checks if an expression is primary. *)
let is_primary = function
	| MyNull | MyString(_) | MyInteger(_) | MyFloat(_) | MyBoolean(_) -> true
	| _ -> false

(** Checks if an expression contains a print statement. *)
let rec contains_print = function
  	| MyNull | MyString(_) | MyInteger(_) | MyFloat(_) | MyBoolean(_) | Nothing | MyTuple(_) | Identifier(_) | Deref(_) | Read | Break | Continue -> false
  	| Block(_, e) | BreakBlock(_, e) | ContinueBlock(_, e) | Negate(e) | Application(e, _) | Lambda(_, e) -> contains_print e
  	| Operator(_, e1, e2) | Let(_, e1, e2) | New(_, e1, e2) | Asg(e1, e2) | Seq(e1, e2) | While(e1, e2) -> contains_print e1 || contains_print e2
  	| For(_, e1, e2, e3) | If(e1, e2, e3) -> contains_print e1 || contains_print e2 || contains_print e3
	| Print(_) -> true

(** Checks if an expression contains the given id. *)
let rec contains_id_arg s expression = match expression with
	| Deref(Identifier(t)) -> String.equal t s
	| _ -> contains_id s expression
and contains_id s = function
	| Lambda(_, _) | MyNull | MyString(_) | MyInteger(_) | MyFloat(_) | MyBoolean(_) | Nothing | MyTuple(_) | Deref(_) | Read | Break | Continue -> false
	| Block(_, e) | BreakBlock(_, e) | ContinueBlock(_, e) | Negate(e) | Print(e) -> contains_id s e
	| Application(_, es) -> fold_left (fun a e -> (contains_id_arg s e) || a) false es
	| Operator(_, e1, e2) | Let(_, e1, e2) | New(_, e1, e2) | Asg(e1, e2) | Seq(e1, e2) -> contains_id s e1 || contains_id s e2
	| While(e1, e2) -> true
	| For(_, e1, e2, e3) | If(e1, e2, e3) -> contains_id s e1 || contains_id s e2 || contains_id s e3
	| Identifier(t) -> String.equal t s

(** Optimises an operator. *)
let rec opt_operator store expression = match expression with
  	| Operator(op, Deref(Identifier(_)), Deref(Identifier(_))) | Operator(op, Deref(Identifier(_)), _) | Operator(op, _, Deref(Identifier(_))) -> expression
	| Operator(op, Identifier(a), Identifier(b)) ->
		let e1 = if(String.equal a "temp") then Identifier(a) else Hashtbl.find store a in
		let e2 = if(String.equal b "temp") then Identifier(b) else  Hashtbl.find store b in
		if(is_primary e1 && is_primary e2) then convert(Exp_eval.eval_operator [] (Hashtbl.create 100) op e1 e2)
		else
	  		let v1 = if(is_primary e1) then e1 else opt_operator store e1 in
	  		let v2 = if(is_primary e2) then e2 else opt_operator store e2 in
	  		Operator(op, v1, v2)
	| Operator(op, Identifier(a), exp) ->
		if(String.equal a "temp") then expression
		else opt_operator store (Operator(op, Hashtbl.find store a, exp))
	| Operator(op, exp, Identifier(a)) ->
		if(String.equal a "temp") then expression
		else opt_operator store (Operator(op, exp, Hashtbl.find store a))
  	| Operator(op, e1, e2) ->
		if(is_primary e1 && is_primary e2) then convert(Exp_eval.eval_operator [] (Hashtbl.create 100) op e1 e2)
		else
	  		let v1 = if(is_primary e1) then e1 else opt_operator store e1 in
	  		let v2 = if(is_primary e2) then e2 else opt_operator store e2 in
	  		Operator(op, v1, v2)
  	| _ -> expression

(** Crestes nested new statements for functions. *)
let rec create_nested_new ps vs a = match (ps, vs) with
	| [], [] -> a
	| p::ps, v::vs -> New(p, v, create_nested_new ps vs a)
	| _, _ -> failwith "The number of values should be equal to the number of parameters."

(** Checks if two expressions are equal. *)
let rec equal exp1 exp2 = match exp1, exp2 with
	| MyString(s), MyString(t) | Identifier(s), Identifier(t) -> String.equal s t
	| Nothing, Nothing | Continue, Continue | Break, Break | Read, Read | MyNull, MyNull -> true
	| MyInteger(i), MyInteger(j) -> i == j
	| MyFloat(i), MyFloat(j) -> i == j
	| MyBoolean(a), MyBoolean(b) -> a && b
	| MyTuple(es1), MyTuple(es2) -> List.fold_left2 (fun a e1 e2 -> (equal e1 e2) && a) true es1 es2
	| Negate(e1), Negate(e2) | Print(e1), Print(e2) | Deref(e1), Deref(e2) -> equal e1 e2
	| Operator(op1, e11, e12), Operator(op2, e21, e22) ->
		let eq = (match op1, op2 with
			| Plus, Plus -> true
			| Minus, Minus -> true
			| Divide, Divide -> true
			| Times, Times -> true
			| Modulus, Modulus -> true
			| Less, Less -> true
			| Leq, Leq -> true
			| Eq, Eq -> true
			| Greater, Greater -> true
			| Geq, Geq -> true
			| Noteq, Noteq -> true
			| And, And -> true
			| Or, Or -> true
			| _, _ -> false) in
		eq && (equal e11 e21) && (equal e12 e22)
	| _, _ -> false

(** Returns whether the second expression has a sub-expression that can be replaced by a variable. *)
let rec sub_elim name r exp = match exp with
	| Let(s, e1, e2) ->
	 	let (elim1, e1') = sub_elim name r e1 in
		let (elim2, e2') = sub_elim name r e2 in
		if(equal r (Let(s, e1', e2'))) then (true, Identifier(name))
		else (elim1 || elim2, Let(s, e1', e2'))
	| New(s, e1, e2) ->
		let (elim1, e1') = sub_elim name r e1 in
		let (elim2, e2') = sub_elim name r e2 in
		if(equal r (New(s, e1', e2'))) then (true, Identifier(name))
		else (elim1 || elim2, New(s, e1', e2'))
	| Asg(e1, e2) -> (false, Asg(e1, e2))
	| Seq(e1, e2) ->
		let (elim1, e1') = sub_elim name r e1 in
		let (elim2, e2') = sub_elim name r e2 in
		if(equal r (Seq(e1', e2'))) then (true, Identifier(name))
		else (elim1 || elim2, Seq(e1', e2'))
	| Negate(e) ->
	 	let (elim, e') = sub_elim name r e in
		if(equal r (Negate(e'))) then (true, Identifier(name))
		else (elim, Negate(e'))
	| Operator(op, e1, e2) ->
		let (elim1, e1') = sub_elim name r e1 in
		let (elim2, e2') = sub_elim name r e2 in
		if(equal r (Operator(op, e1', e2'))) then (true, Identifier(name))
		else (elim1 || elim2, Operator(op, e1', e2'))
	| For(s, e1, e2, e3) ->
		let (elim, e') = sub_elim name r e3 in
		if(equal r (For(s, e1, e2, e'))) then (true, Identifier(name))
		else (elim, For(s, e1, e2, e'))
	| While(e1, e2) ->
		let (elim1, e1') = sub_elim name r e1 in
		let (elim2, e2') = sub_elim name r e2 in
		if(equal r (While(e1', e2'))) then (true, Identifier(name))
		else (elim1 || elim2, While(e1', e2'))
	| If(e1, e2, e3) ->
		let (elim1, e1') = sub_elim name r e1 in
		let (elim2, e2') = sub_elim name r e2 in
		let (elim3, e3') = sub_elim name r e3 in
		if(equal r (If(e1', e2', e3'))) then (true, Identifier(name))
		else (elim1 || elim2 || elim3, If(e1', e2', e3'))
	| Application(e, ps) -> (false, Application(e, ps))
	| Lambda(p, e) ->
		let (elim, e') = sub_elim name r e in
		if(equal r (Lambda(p, e'))) then (true, Identifier(name))
		else (elim, Lambda(p, e'))
	| Print(e) ->
		let (elim, e') = sub_elim name r e in
		if(equal r (Print(e'))) then (true, Identifier(name))
		else (elim, Print(e'))
	| _ -> (false, exp)

(** Finds all the sub-expressions in the first epxression that can be replaced by a variable. *)
let rec sub_elims name exp1 exp2 = match exp1 with
	| Asg(e1, e2) ->
		let(elim, exp2') = sub_elim name e2 exp2 in
		if(elim) then (e2, Asg(e1, Identifier(name)), exp2')
		else
			let (e2'', e2', exp2') = sub_elims name e2 exp2 in
			(e2'', Asg(e1, e2'), exp2')
	| Negate(e) ->
		let(elim, exp2') = sub_elim name e exp2 in
		if(elim) then (e, Negate(Identifier(name)), exp2') else
			let(e'', e', exp2') = sub_elims name e exp2 in
			(e'', Negate(e'), exp2')
	| Operator(op, e1, e2) ->
		let (elim, exp2') = sub_elim name exp1 exp2 in
		if(elim) then (exp1, Identifier(name), exp2') else
			let (e2'', e2', exp2') = sub_elims name e2 exp2 in
			(e2'', Operator(op, e1, e2'), exp2')
	| For(s, e1, e2, e3) -> let (e3'', e3', exp2') = sub_elims name e3 exp2 in (e3'', For(s, e1, e2, e3'), exp2')
	| While(e1, e2) ->
		let(elim, exp2') = sub_elim name e1 exp2 in
		if(elim) then (e1, While(Identifier(name), e2), exp2')
		else
			let (e2'', e2', exp2') = sub_elims name e2 exp2 in (e2'', While(e1, e2'), exp2')
	| If(e1, e2, e3) ->
		let(elim, exp2') = sub_elim name e1 exp2 in
		if(elim) then (e1, If(Identifier(name), e2, e3), exp2')
		else
			let (e2'', e2', exp2') = sub_elims name e2 exp2 in
			(e2'', If(e1, e2', e3), exp2)
	| Print(e) ->
		let(elim, exp2') = sub_elim name e exp2 in
		if(elim) then (e, Print(Identifier(name)), exp2') else
			let(e'', e', exp2') = sub_elims name e exp2 in
			(e'', Print(e'), exp2')
	| _ -> (Nothing, exp1, exp2)

(** Unrolls a loop and optimises expressions. *)
let rec loop_unroll max_loop is_function store s i n e =
	let v = opt_exp max_loop is_function true (extend store s (MyInteger(i))) e in
	if(!break == true) then v
	else
		(continue := false;
		if(i<n && max_loop>1) then
		   	Seq(v, loop_unroll (max_loop-1) is_function store s (i+1) n e)
		else v)
and opt_exp max_loop is_function is_loop store expression = match expression with
  	| MyTuple(es) -> MyTuple(map (opt_exp max_loop is_function is_loop store) es)
  	| Deref(Identifier(s)) -> (try Hashtbl.find store s with
							 	| Not_found -> if(Hashtbl.mem function_store s) then Identifier(s)
											   else if(is_function) then raise (Exp_errors.FunctionError ("Variable " ^ s ^ " was not declared"))
													else Deref(Identifier(s)))
	| Deref(Deref(Identifier(s))) -> (match Hashtbl.find store s with
				| Identifier(t) -> opt_exp max_loop is_function is_loop store (Deref(Identifier(t)))
			 	| _ -> if(is_function) then raise (Exp_errors.FunctionError ("Variable " ^ s ^ " was not declared"))
					   else Deref(Deref(Identifier(s))))
	| Deref(_) | Identifier(_) | MyNull | MyString(_) | MyInteger(_) | MyFloat(_) | MyBoolean(_) | Nothing -> expression
  	| Let(s, e1, e2) ->
		let (to_be_replaced, replacement, replaced) = sub_elims "temp" e1 e2 in
		if(to_be_replaced != Nothing) then opt_exp max_loop is_function is_loop store (Let("temp", to_be_replaced, (Let(s, replacement, replaced))))
		else
			let v1 = opt_exp max_loop is_function is_loop store e1 in
			if(contains_print v1 || String.equal s "temp") then Let(s, v1, opt_exp max_loop is_function is_loop store e2)
			else (Hashtbl.add store s v1;
			  	 let v2 = opt_exp max_loop is_function is_loop store e2 in
			   	 Hashtbl.remove store s;
			  	 v2)
	| New(s, e1, e2) ->
		(match opt_exp max_loop is_function is_loop store e1 with
	  		| Read -> New(s, Read, opt_exp max_loop is_function is_loop store e2)
	  		| v1 -> if(contains_print v1) then New(s, v1, opt_exp max_loop is_function is_loop store e2)
					else (Hashtbl.add store s v1;
						 let v2 = opt_exp max_loop is_function is_loop store e2 in
						 if(is_primary v2 || not (contains_id s v2)) then v2
						 else New(s, v1, v2)))
	| Asg(e1, e2) ->
		(match opt_exp max_loop is_function is_loop store e1 with
			| Identifier(s) ->
				let v2 = opt_exp max_loop is_function is_loop store e2 in
				Hashtbl.replace store s v2;
				if(is_loop) then Asg(Identifier(s), v2)
				else Nothing
			| v1 -> Asg(v1, opt_exp max_loop is_function is_loop store e2))
  	| Seq(e1, e2) ->
		(match opt_exp max_loop is_function is_loop store e1 with
			| Nothing | Break | Continue as v->
				if(!break || !continue) then v
				else
					let v = opt_exp max_loop is_function is_loop store e2 in
					if(!break && not is_loop) then Break else if(!continue && not is_loop) then Continue else v
			| v1 ->
				if(!break || !continue) then v1
				else
					if(not !totally_unrolled) then
						if(!break || !continue ) then v1 else Seq(v1, e2)
					else
						let v2 = opt_exp max_loop is_function is_loop store e2 in
						if(!break || !continue ) then v1 else Seq(v1, v2))
  	| Negate(e) ->
		(match opt_exp max_loop is_function is_loop store e with
			| MyBoolean(b) -> MyBoolean(not b)
			| v -> Negate(v))
  	| Operator(op, e1, e2) ->
		(match op with
			| And ->
				let v1 = opt_exp max_loop is_function is_loop store e1 in
				if(is_primary v1) then
					if(get_boolean v1) then opt_exp max_loop is_function is_loop store e2 else v1
				else opt_operator store (Operator(op, v1, opt_exp max_loop is_function is_loop store e2))
			| Or ->
				let v1 = opt_exp max_loop is_function is_loop store e1 in
				if(is_primary v1) then
					if(get_boolean v1) then v1 else opt_exp max_loop is_function is_loop store e2
				else opt_operator store (Operator(op, v1, opt_exp max_loop is_function is_loop store e2))
			| _ ->
				let v1 = opt_exp max_loop is_function is_loop store e1 in
				let v2 = opt_exp max_loop is_function is_loop store e2 in
				opt_operator store (Operator(op, v1, v2)))
  	| For(s, e1, e2, e3) ->
		let v1 = opt_exp max_loop is_function is_loop store e1 in
		(match v1, opt_exp max_loop is_function is_loop store e2 with
			 | MyInteger(i), MyInteger(n) ->
			 	let e = loop_unroll max_loop is_function store s i n e3 in
				if(i+global_max_loop<n) then
					(totally_unrolled := false;
					if(!break == true) then (break := false; Let(s, MyInteger(i), e3))
					else
						(continue := false;
			 			Seq(Let(s, MyInteger(i), opt_exp 0 is_function true store e), For(s, MyInteger(i+global_max_loop), MyInteger(n), e3))))
				else
					let v = opt_exp 0 is_function true store e in
					if(!break == true) then (break := false; Nothing) else (continue := false; v);
			 | _, _ -> For(s, e1, e2, e3))
  	| While(e1, e2) ->
		let v1 = opt_exp global_max_loop is_function is_loop store e1 in
		if(max_loop > 0) then
			if(is_primary v1) then
				if(get_boolean v1) then
					let v2 = opt_exp global_max_loop is_function true store e2 in
					if(!break == true) then (break := false; v2)
					else (continue := false; Seq(v2, opt_exp (max_loop-1) is_function false store (While(e1, e2))))
				else Nothing
			else
				(totally_unrolled := false;
				If(v1, Seq(opt_exp global_max_loop is_function true store e2, While(e1, e2)), Nothing))
		else
			(totally_unrolled := false; While(e1, e2))
  	| If(e1, e2, e3) ->
		(match opt_exp max_loop is_function is_loop store e1 with
		  | MyBoolean(b) -> if b then opt_exp max_loop is_function is_loop store e2 else opt_exp max_loop is_function is_loop  store e3
		  | v1 ->
				let v2 = opt_exp max_loop is_function is_loop store e2 in
				let v3 = opt_exp max_loop is_function is_loop store e3 in
				if(v2==Nothing && v3==Nothing) then Nothing
				else If(v1, v2, v3))
  	| Application(e, ps) ->
		let vs = map (fun p -> let new_p = opt_exp max_loop is_function is_loop store p in
			 (match p, new_p with
				 | Identifier(_), Identifier(s) ->
				 	(try access store s with
						| Exp_errors.VariableDeclaration (_, _) -> Identifier(s))
				 | _, _ ->  new_p)) ps in
		if(fold_left (fun a v -> contains_print v || a) false vs) then Application(opt_exp max_loop true is_loop (Hashtbl.create 100) e, ps)
		else (match opt_exp max_loop is_function is_loop store e with
			  	| Identifier(s) ->
					if(Hashtbl.mem memo_store (s, vs)) then Hashtbl.find memo_store (s, vs)
					else
						if(Hashtbl.mem function_store s) then
					  		let (ps, expression) = access function_store s in
							let success = ref true in
							let exp = (try opt_exp max_loop true is_loop (Hashtbl.create 100) (create_nested_new ps vs expression) with
								| Exp_errors.FunctionError msg -> success := false; Application(Identifier(s), vs)) in
							if(!success) then
								(let exp' = opt_exp max_loop is_function is_loop store exp in
								if(not (contains_print exp')) then
									Hashtbl.replace memo_store (s, vs) exp';
								exp')
							else exp
						else
							if(Hashtbl.mem function_store s) then
								(match access store s with
									| Application(expression, pps) -> opt_exp max_loop is_function is_loop store (Application(expression, ps@pps))
									| _ -> Application(Identifier(s), vs))
							else Application(Hashtbl.find store s, vs)
			  	| Lambda(pps, expression) ->
					if(length pps > length vs) then
				  		let (ps1, ps2) = split [] (length vs) pps in Lambda(ps2, opt_exp max_loop is_function is_loop (extend_list (Hashtbl.create 100) (combine ps1 vs)) expression)
					else opt_exp max_loop true is_loop (Exp_store.extend_list (Hashtbl.create 100) (combine pps vs)) expression
			  	| v -> Application(v, vs))
  	| Lambda(p, e) -> Lambda(p, e)
  	| Read -> Read
  	| Print(e) -> let v = opt_exp max_loop is_function is_loop store e in Print(v)
	| Break -> break := true; if(is_loop) then Nothing else Break
	| Continue -> continue := true; if(is_loop) then Nothing else  Continue
	| _ -> expression

(** Function evaluates each individual function in the program. *)
let rec opt_fundef = function
  	| [] -> []
  	| (s, ps, expression)::[] -> [(s, ps, opt_exp global_max_loop false false (Hashtbl.create 100) expression)]
  	| (s, ps, expression)::program -> let _ = extend function_store s ((ps, expression)) in (s, ps, expression) :: opt_fundef program

(** The main function. *)
let opt program =
	break := false;
	continue := false;
	opt_fundef program
