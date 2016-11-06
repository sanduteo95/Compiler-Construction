open Syntax
open Instruction_set
open Exp_store
open Type_checking

let addr_base = ref 0

let new_addr() =
  	addr_base := !addr_base + 1;
  	!addr_base

let (&&) a b = if (a+b==2) then 1 else 0
let (||) a b = if (a+b>=1) then 1 else 0
let (<) a b = if (a<b) then 1 else 0
let (<=) a b = if (a<=b) then 1 else 0
let (>) a b = if (a>b) then 1 else 0
let (>=) a b = if (a>=b) then 1 else 0
let (==) a b = if (a==b) then 1 else 0
let (!=) a b = if (a!=b) then 1 else 0

let convert_operator operator = match operator with
    | Plus -> (+)
    | Minus -> (-)
    | Times -> ( * )
    | Divide -> (/)
    | Modulus -> (mod)
    | Less -> (<)
    | Leq -> (<=)
    | Greater -> (>)
    | Geq -> (>=)
    | Eq -> (==)
    | Noteq -> (!=)
    | And -> (&&)
    | Or -> (||)

let rec print_env = function
    | [] -> Printf.printf "";
    | (var, addr)::env -> Printf.printf "(%s, %d)\n" var addr; print_env env

let rec interp_exp symt = function
    | Operator(operator, e1, e2) ->
        let addr1 = interp_exp symt e1 in
        let addr2 = interp_exp symt e2 in
        op (convert_operator operator, addr1, addr2);
        addr_base := addr1;
        st addr1;
        Printf.printf "Operator: %d.\n" addr1; print_env symt; addr1
    | Negate(e) ->
        let addr = interp_exp symt e in
        not addr;
        addr_base := addr;
        st addr;
        addr
    | Identifier(s) -> Printf.printf "Identifier %s at address %d.\n" s (lookup s symt); lookup s symt
        (* let addr = lookup s symt in *)
        (* let addr'= new_addr() in *)
        (* mv addr addr'; *)
        (* Printf.printf "Variable %s (moved from %d): %d.\n" s addr addr'; print_env symt; addr' *)
    | Deref(Identifier(s)) -> Printf.printf "DEREF.\n";
        let addr = lookup s symt in
        let addr'= new_addr() in
        mv addr addr';
        Printf.printf "Variable %s (moved from %d): %d.\n" s addr addr'; print_env symt; addr'
    | MyNull ->
        let addr = new_addr() in
        ldc 0;
        st addr;
        addr
    | MyInteger(i) ->
        let addr = new_addr() in
        ldc i;
        st addr;
        Printf.printf "Constant %d: %d.\n" i addr; print_env symt; addr
    | MyBoolean(b) ->
        let addr = new_addr() in
        if(b) then ldc 1 else ldc 0;
        st addr;
        Printf.printf "Constant %b: %d.\n" b addr; print_env symt; addr
    | Seq(e1, e2) -> Printf.printf "SEQ \n";
        let _ = interp_exp symt e1 in
        interp_exp symt e2
    | Asg(e1, e2) -> Printf.printf "ASG \n";
        let addr1 = interp_exp symt e1 in
        let addr2 = interp_exp symt e2 in
        mv addr2 addr1;
        addr_base := addr1;
        Printf.printf "Assignment: %d.\n" addr1; print_env symt; addr1
    | New(s, e1, e2) -> Printf.printf "NEW \n";
        let addr = interp_exp symt e1 in
        let addr' = new_addr() in
        mv addr addr';
        let addr2 = interp_exp ((s, addr')::symt) e2 in
        addr2
    | Let(s, e1, e2) -> Printf.printf "LET \n";
        let addr1 = interp_exp symt e1 in
        let addr2 = interp_exp ((s, addr1)::symt) e2 in
        mv addr2 addr1;
        addr_base := addr1;
        addr1
    | Nothing -> !addr_base
    | _ -> failwith "Not implemented yet."

let interpret program = match program with
    | [] -> 0
    | ("main", [], expression)::[] ->
        addr_base := 0;
        let addr = interp_exp [] expression in
        Hashtbl.find ram addr
    | _ -> failwith "Not implemented yet."