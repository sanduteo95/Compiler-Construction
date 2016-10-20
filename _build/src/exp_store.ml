(** Contains code for evaluating an expression.  *)
open Syntax
open Printf
open Type_checking

exception TypeError of string * string
exception DivisionError of string
exception NotImplementedError of string
exception VariableDeclaration of string * string

(** Stores variables that were created and their values. *)
let var_store = Hashtbl.create 100

(** Applies prefix operators of the form int->int->int *)
let int_operator name op v1 v2 = 
  if(is_int v1 && is_int v2) then create_int (op (get_value v1) (get_value v2))
  else raise (TypeError ("This operator expected two integers: ", name))

(** Applies prefix operators of the form int->int->bool *)
let mix_operator name op v1 v2 = 
  if(is_int v1 && is_int v2) then create_bool (op (get_value v1) (get_value v2))
  else raise (TypeError ("This operator expected two booleans: ", name))

(** Applies prefix operators of the form bool->bool->bool *)
let bool_operator name op v1 v2 = 
  if(not (is_bool v1) || not (is_bool v2)) then raise (TypeError ("This operator expected two booleans: ", name))  
  else create_bool (op (get_value v1 == 1) (get_value v2 == 1))

(** Evaluates operations by using the three previous helper functions.  *)
let eval_operator op v1 v2 = match op with
  | Plus -> int_operator "+" ( + ) v1 v2
  | Minus -> int_operator "-" ( - ) v1 v2
  | Times ->  int_operator "*" ( * ) v1 v2
  | Divide -> 
    if(get_value v2 == 0) then raise (DivisionError "The second expression of the division operation evaluates to 0.")
    else int_operator "/" ( / ) v1 v2
  | Modulus -> 
    if(get_value v2 == 0) then raise (DivisionError "The second expression of the division operation evaluates to 0.")
    else int_operator "mod" ( mod ) v1 v2     
  | Less -> mix_operator "<" ( < ) v1 v2  
  | Leq -> mix_operator "<=" ( <= ) v1 v2       
  | Greater -> mix_operator ">" ( > ) v1 v2  
  | Geq -> mix_operator "=>" ( >= ) v1 v2      
  | Eq -> create_bool (get_value v1 == get_value v2)
  | Noteq -> create_bool (get_value v1 != get_value v2)
  | And -> bool_operator "&&" (&&) v1 v2
  | Or -> bool_operator "||" (||) v1 v2

(** Evaluates an expression and returns either: Const of int, MyString of string, MyBoolean of string or Nothing. *)
let rec eval_assignment = function
  | Let(s, e1, e2) ->
    if(Hashtbl.mem var_store s) then
      let value = Hashtbl.find var_store s in
      Hashtbl.replace var_store s (eval_exp e1);
      let v = eval_assignment e2 in
      Hashtbl.replace var_store s value; 
      v
    else
      let _ = () in 
      Hashtbl.add var_store s (eval_exp e1);
      let v = eval_assignment e2 in
      Hashtbl.remove var_store s; 
      v 
  | If(e1, e2, e3) ->
    let v1 = eval_exp e1 in
    if(not (is_bool v1)) then raise (TypeError  ("The condition inside the if statement expects a boolean", "")) 
    else if(get_value v1 == 1) then eval_assignment e2 else eval_assignment e3
  | Deref(e) -> eval_assignment e
  | Identifier(s) -> s
  | _ -> raise (NotImplementedError "The left side of an assignment can only be a variable, an if statement or a let statement.")
and
eval_exp expression = match expression with
  | MyString(s) -> String(s)
  | Const(i) -> Integer(i)
  | MyBoolean(b) -> Boolean(b)
  | Identifier(s) -> Pointer(s)
  | Nothing -> Unit
  | Seq(e1, e2) -> 
      let _ = eval_exp e1 in 
      let v2 = eval_exp e2 in 
      v2
  | While(e1, e2)-> 
    let v1 = eval_exp e1 in 
    if(not (is_bool v1)) then raise (TypeError ("The condition inside the while loop expects a boolean", ""))
    else 
      if(get_value v1 == 1) then 
      let _ = eval_exp e2 in eval_exp (While(e1, e2))
      else eval_exp(Nothing)
  | If(e1, e2, e3) -> 
    let v1 = eval_exp e1 in 
    if(not (is_bool v1)) then raise (TypeError  ("The condition inside the if statement expects a boolean", "")) 
    else if(get_value v1 == 1) then eval_exp e2 else eval_exp e3
  | Asg(e1, e2) -> 
    let v1 = eval_assignment e1 in
    let v2 = eval_exp e2 in
    Hashtbl.replace var_store v1 v2;
    eval_exp(Nothing)
  | Deref(e) -> 
    let v = eval_exp e in 
    (match v with 
      | Pointer(s) -> 
          (try Hashtbl.find var_store s with 
          | Not_found -> raise (VariableDeclaration ("This variable was not declared: ", s)))
      | _ -> raise (TypeError ("The expression cannot be dereferenced, because it needs to be a pointer.", "")))
  | Negate(e) -> 
    let v = eval_exp e in 
    if(not (is_bool v)) then raise (TypeError ("The negation statament expects a boolean", ""))
    else create_bool (not (get_value v == 1))
  | Operator(op, e1, e2) -> eval_operator op (eval_exp e1 ) (eval_exp e2)
(*   | Read -> 
    let line = read_line () in
    if (input_is_int line) then create_int (int_of_string line) 
    else if(line == "true" || line == "false") then create_bool(line == "true")
    else String(line) *)
(*   | Print(e) -> 
    (match (eval_exp e) with 
      | Integer(i) -> printf "%d" i
      | String(s) | Boolean(s) -> printf "%s" (String.sub s 1 (String.length s - 2))
      | _ -> raise (TypeError ("The print function can only print integers, booleans or strings", "" )));
    eval_exp(Nothing) *)
  | Let(s, e1, e2) ->
    if(Hashtbl.mem var_store s) then
      let value = Hashtbl.find var_store s in
      Hashtbl.replace var_store s (eval_exp e1);
      let v = eval_exp e2 in
      Hashtbl.replace var_store s value; 
      v
    else
      let _ = () in 
      Hashtbl.add var_store s (eval_exp e1);
      let v = eval_exp e2 in
      Hashtbl.remove var_store s; 
      v
  | New(s, e1, e2) ->
    if(Hashtbl.mem var_store s) then raise (VariableDeclaration ("This variable was already declared: ", s)) 
    else Hashtbl.add var_store s (eval_exp e1); eval_exp e2
  | _  -> raise (NotImplementedError "One of the expressions you used cannot be evaluated yet!")

(** Function checks if there were any errors thrown.  *)
let eval_with_error expression = 
  try eval_exp expression with 
    | TypeError (msg, name) -> prerr_string ("Error: " ^ msg ^ name ^ ".\n"); exit (-1)   
    | DivisionError msg -> prerr_string ("Error: " ^msg ^ "\n"); exit (-1)
    | NotImplementedError msg -> prerr_string ("Error: " ^msg ^ "\n"); exit (-1)
    | VariableDeclaration (msg, var) -> prerr_string ("Error: " ^msg ^ var ^ ".\n"); exit (-1)
