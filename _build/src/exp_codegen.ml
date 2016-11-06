open Syntax
open Instruction_set
open Exp_store

let code = Buffer.create 100
let addr_base = ref 0

let new_addr() =
  	addr_base := !addr_base + 1;
  	!addr_base

let string_of_operator = function
    | ( + )  -> "add"
    | ( - )  -> "sub"
    | ( * )  -> "mul"
    | ( / )  -> "div"
    | ( && ) -> "and"
    | ( || ) -> "or"
    | _ -> "still need a few"

let codegen_op (op, addr1, addr2) =
    (string_of_operator op)
    ^ " r" ^ (string_of_int addr1)
    ^ ", r" ^ (string_of_int addr2)
    ^ "\n" |> Buffer.add_string code

let codegen_not addr =
    "not" ^ " r" ^ (string_of_int addr)
    ^ "\n" |> Buffer.add_string code

let codegen_mv addr1 addr2 =
    "mv r" ^ (string_of_int addr1)
    ^ ", r" ^ (string_of_int addr2)
    ^ "\n" |> Buffer.add_string code

let codegen_st addr =
    "st r" ^ (string_of_int addr)
    ^ "\n" |> Buffer.add_string code

let codegen_ldc n =
    "ld " ^ (string_of_int n)
    ^ "\n" |> Buffer.add_string code

let rec codegen symt = function
    | Operator(operator, e1, e2) ->
        let addr1 = codegen symt e1 in
        let addr2 = codegen symt e2 in
        codegen_op (operator, addr1, addr2);
        addr_base := addr1;
        codegen_st addr1;
        addr1
    | Negate(e) ->
        let addr = codegen symt e in
        codegen_not addr;
        st addr;
        addr
    | Identifier(s) ->
        let addr = lookup s symt in
        let addr'= new_addr() in
        codegen_mv addr addr';
        addr'
    | Deref(Identifier(s)) -> lookup s symt
    | MyNull ->
        let addr = new_addr() in
        codegen_ldc 0;
        st addr;
        addr
    | MyInteger(i) ->
        let addr = new_addr() in
        codegen_ldc i;
        st addr;
        addr
    | MyBoolean(b) ->
        let addr = new_addr() in
        if(b) then codegen_ldc 1 else codegen_ldc 0;
        st addr;
        addr
    | Seq(e1, e2) ->
        let _ = codegen symt e1 in
        codegen symt e2
    | Asg(e1, e2) ->
        let addr1 = codegen symt e1 in
        let addr2 = codegen symt e2 in
        st addr1;
        addr_base := addr1;
        addr1
    | New(s, e1, e2) ->
        let addr1 = codegen symt e1 in
        let addr2 = codegen ((s, addr1)::symt) e2 in
        addr2
    | Let(s, e1, e2) ->
        let addr1 = codegen symt e1 in
        let addr2 = codegen ((s, addr1)::symt) e2 in
        mv addr2 addr1;
        addr_base := addr1;
        addr1
    | _ -> failwith "Not implemented yet."

let generate program = match program with
    | [] ->
        Buffer.reset code;
        "" |> print_endline
    | ("main", [], expression)::[] ->
        Buffer.reset code;
        addr_base := 0;
        let addr = codegen [] expression in
        Buffer.output_buffer stdout code;
        "ld r" ^ (string_of_int addr) |> print_endline
    | _ ->  failwith "Not implemented yet."
