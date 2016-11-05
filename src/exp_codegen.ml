open Syntax
open Instruction_set

let code = Buffer.create 100
let addr_base = ref 0

let codegen_op (op, addr1, addr2) =
    (string_of_operator op)
    ^ " r" ^ (string_of_int addr1)
    ^ ", r" ^ (string_of_int addr2)
    ^ "\n" |> BUffer.add_string code

let codegen_st addr =
    "st r" ^ (string_of_int addr)
    ^ "\n" |> Buffer.add_string code

let codegen_ldc n =
    "ld " ^ (string_of int n)
    ^ "\n" |> Buffer.add_string code

let rec codegen symt = function
    | Operator(op, e1, e2) ->
        let addr1 = codegen symt e1 in
        let addr = codegen symt e2 in
        codegen_op (convert_operator op, addr1, addr2);
        addr_base := addr1;
        codegen_st addr1;
        addr1
    | Identifier(s) ->
        let addr = lookup x symt in
        let addr'= new_addr () in
        codegen_mv addr addr';
        addr'
    | MyInteger(i) ->
        let addr = new_addr () in
        codegen_ldc i;
        st addr
    | Let(s, e1, e2) ->
        let addr1 = codegen symt s1 in
        let addr2 = codegen ((x, addr1)::symt) e2 in
        mv addr2 addr1;
        add_base := addr1;
        addr1

let generate =
    Buffer.reset code;
    addr_base := 0;
    let addr = codegen [] exp in
    Buffer.output_buffer stdout code;
    "ld r" ^ (string_of_int addr) <> print_endline
