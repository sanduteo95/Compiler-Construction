(** Contains code for evaluating an expression.  *)
open Syntax
open Exp_store
open Exp_errors
open Printf
open Type_checking
open List

let steps = ref 0
let function_store = Hashtbl.create 100

(** Evaluates an expression and returns either: Const of int, MyString of string, MyBoolean of string or Nothing. *)
let rec eval_operator env store op e1 e2 =
  let v1 = eval_exp env store e1 in
  let v2 = if(op != And && op != Or) then eval_exp env store e2 else Null in
  (match op with
    | Plus | Minus | Times | Divide | Modulus ->
      (match v1, v2 with
        | Integer v1, Integer v2 -> let operator = get_int_operator op in Integer(operator v1 v2)
        | Float v1, Float v2 -> let operator = get_float_operator op in Float(operator v1 v2)
        | _ -> raise (TypeError ("This operator requires either two integers or two floats: ", "+")))
    | Less | Leq | Greater | Geq | Eq | Noteq -> steps := !steps + 2;
      (match v1, v2 with
      | Integer v1, Integer v2 -> let operator = get_mix_operator op in Boolean(operator v1 v2)
      | Float v1, Float v2 -> let operator = get_mix_operator op in Boolean(operator v1 v2)
      | String v1, String v2 -> let operator = get_mix_operator op in Boolean(operator v1 v2)
      | Boolean v1, Boolean v2 -> let operator = get_mix_operator op in Boolean(operator v1 v2)
      | Tuple(v1), Tuple(v2) -> Boolean(tuple_mix_operator op (Tuple(v1)) (Tuple(v2)))
      | _ -> raise (TypeError ("This operator requires either two integers, two floats, two booleans, two strings or two tuples: ", "!=")))
    | And -> steps := !steps + 1;
      (match v1 with
        | Boolean b1 -> if (not b1) then Boolean(false) else eval_exp env store e2
        | _ -> raise (TypeError ("This operator requires two booleans", "&&")))
    | Or -> steps := !steps + 2;
      (match v1 with
        | Boolean b1 -> if (b1) then Boolean(true) else eval_exp env store e2
        | _ -> raise (TypeError ("This operator requires two booleans", "||"))))
and
eval_application env store l ps parameters exp =
  let vs = map (fun p -> eval_exp env store p) ps in
  let pvs = get_pointer_values store [] vs in
  let store = extend_list store pvs in
  if(length parameters < length vs) then raise (FunctionError "This function application doesn't have the required number of parameters")
  else
    let store = if(l == -1) then store else extend store l (Fundef(parameters, exp)) in
    let locations = create_locations (length vs) [] in
    if(length parameters > length vs) then
      let parameters1 = take [] (length vs) parameters in ((combine parameters1 (map (fun location -> Pointer(location)) locations))@env, extend_list store (combine locations vs), Lambda(parameters, exp))
    else ((combine parameters (map (fun location -> Pointer(location)) locations))@env, extend_list store (combine locations vs), exp)
and
eval_exp env store expression =
    steps := !steps+1;
    match expression with
      | MyNull -> Null | MyString(s) -> String(s) | MyInteger(i) -> Integer(i) | MyFloat(f) -> Float(f) | MyBoolean(b) -> Boolean(b)  | Nothing -> steps := !steps + 1; Unit
      | MyTuple(es) ->  Tuple(map (fun e -> eval_exp env store e) es)
      | Identifier(s) -> lookup s env
      | Deref(e) ->
        let v = eval_exp env store e in
        (match v with
          | Pointer(location) -> access store location
          | _ -> v)
      | Let(s, e1, e2) ->
        let v = eval_exp env store e1 in eval_exp ((s,v)::env) store e2
      | New(s, e1, e2) ->
        let v1 = eval_exp env store e1 in
        let location = newref() in
        eval_exp ((s, Pointer(location))::env) (extend store location v1) e2
      | Asg(e1, e2) ->
        let e = eval_exp env store e1 in
        (match e with
          | Pointer(location) ->
            let v = eval_exp env store e2 in
            let old_v = access store location in
            if(get_type old_v != get_type Null && get_type v != get_type old_v) then raise (TypeError ("This variable can't change its type", ""))
            else eval_exp env (extend store location v) (Nothing)
          | _ -> raise (TypeError ("The left hand side of an assignment should always be a location", "")))
      | Seq(e1, e2) ->
        let _ = eval_exp env store e1 in eval_exp env store e2
      | Negate(e) ->
        let v = eval_exp env store e in
        if(not (is_bool v)) then raise (TypeError ("The negation statament expects a boolean", ""))
        else Boolean (not (get_boolean v))
      | Operator(op, e1, e2) ->
        eval_operator env store op e1 e2
      | For(s, e1, e2, e3) ->
        let v1 = eval_exp env store e1 in
        let v2 = eval_exp env store e2 in
        let i =
        (match v1 with
          | Integer(i) -> i
          | _ -> raise (TypeError ("For loops only contains integers", ""))) in
        let j =
        (match v2 with
          | Integer(j) -> j
          | _ -> raise (TypeError ("For loops only contains integers", ""))) in
        if(i <= j) then
            let location = newref() in
            let _ = eval_exp ((s, Pointer(location))::env) (extend store location v1) e3 in eval_exp env store (For(s, MyInteger(i+1), MyInteger(j), e3))
        else eval_exp env store Nothing
      | While(e1, e2)->
        let v1 = eval_exp env store e1 in
        if(not (is_bool v1)) then raise (TypeError ("The condition inside the while loop expects a boolean", ""))
        else
          if(get_boolean v1) then let _ = eval_exp env store e2 in eval_exp env store (While(e1, e2))
          else eval_exp env store (Nothing)
      | If(e1, e2, e3) ->
        let v1 = eval_exp env store e1 in
        if(not (is_bool v1)) then raise (TypeError  ("The condition inside the if statement expects a boolean", ""))
        else if(get_boolean v1) then eval_exp env store e2 else eval_exp env store e3
      | Application(e, ps) ->
        let f = eval_exp env store e in
        (match f with
          | Fundef(parameters, exp) -> let (env, store, expression) = eval_application env store (-1) ps parameters exp in eval_exp env store expression
          | Pointer(location) -> let v = access store location in
            (match v with
              | Fundef(parameters, exp) -> let (env, store, expression) = eval_application env store location ps parameters exp in eval_exp env store expression
              | _ -> raise (FunctionError "The function you tried to apply to the parameters is not correct."))
          | _ -> raise (FunctionError "The function you tried to apply to the parameters is not correct."))
      | Lambda(p, e) -> Fundef(p, e)
      | Read ->
        (* let line  = read_line () in *)
        Integer(3)
        (* if (input_is_int line) then Integer(int_of_string line)
        else
          if (input_is_float line) then Float(float_of_string line)
          else
            if(line == "true" || line == "false") then Boolean(line == "true")
            else String(line) *)
      | Print(e) ->
        (match (eval_exp env store e) with
          | Integer(i) -> printf "%d\n" i
          | Float(f) -> printf "%f\n" f
          | String(s) -> printf "%s\n" (String.sub s 1 (String.length s - 2))
          | Boolean(b) -> printf "%b\n" b
          | _ -> raise (TypeError ("The print function can only print integers, booleans or strings", "" )));
        eval_exp env store (Nothing)

(** Function evaluates each individual function in the program. *)
let rec eval_fundef env store = function
  | [] -> raise (FunctionError "There are no functions to evaluate.")
  | (s, p, expression)::[] ->
    if(not (String.equal s "main")) then raise (FunctionError "This program doesn't have a main function.")
    else
      if(not (length p == 0)) then raise (FunctionError "The main function can't have any parameters.")
      else eval_exp env store expression
  | (s, p, expression)::program ->
    let location = newref() in
    let _ = extend function_store location (Fundef(p, expression)) in
    eval_fundef ((s, Pointer(location))::env) (extend store location (Fundef(p, expression))) program

(** Function checks what type of program it is and rejects ones that we haven't implemented yet. *)
let eval program =
    let evaluation = eval_fundef [] (Hashtbl.create 100) program in
    (!steps, evaluation)