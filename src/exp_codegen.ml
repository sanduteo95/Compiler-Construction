open Syntax
open Instruction_set
open Exp_store

let code = Buffer.create 100
let addr_base = ref 0
let label_base = ref 0

let new_addr() =
  	addr_base := !addr_base + 1;
  	!addr_base

let new_label() =
    label_base := !label_base + 1;
    !label_base

let string_of_operator = function
    | ( + )  -> "add"
    | ( - )  -> "sub"
    | ( * )  -> "mul"
    | ( / )  -> "div"
    | ( && ) -> "and"
    | ( || ) -> "or"
    | ( mod ) -> "mod"
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

let codegen_jmpz label =
    "jmpz " ^ label
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
        addr_base := addr;
        codegen_st addr;
        addr
    | Identifier(s) -> lookup s symt
    | Deref(Identifier(s)) ->
        let addr = lookup s symt in
        let addr'= new_addr() in
        codegen_mv addr addr';
        addr'
    | MyNull ->
        let addr = new_addr() in
        codegen_ldc 0;
        codegen_st addr;
        addr
    | MyInteger(i) ->
        let addr = new_addr() in
        codegen_ldc i;
        codegen_st addr;
        addr
    | MyBoolean(b) ->
        let addr = new_addr() in
        if(b) then codegen_ldc 1 else codegen_ldc 0;
        codegen_st addr;
        addr
    | Seq(e1, e2) ->
        let _ = codegen symt e1 in
        codegen symt e2
    | Asg(e1, e2) ->
        let addr1 = codegen symt e1 in
        let addr2 = codegen symt e2 in
        codegen_mv addr2 addr1;
        addr1
    | New(s, e1, e2) ->
        let addr1 = codegen symt e1 in
        let addr2 = codegen ((s, addr1)::symt) e2 in
        addr2
    | Let(s, e1, e2) ->
        let addr1 = codegen symt e1 in
        let addr2 = codegen ((s, addr1)::symt) e2 in
        codegen_mv addr2 addr1;
        addr_base := addr1;
        addr1
    | While(e1, e2) ->
        let addr1 = codegen symt e1 in
        let label = ("L" ^ string_of_int (new_label())) in
        codegen_jmpz label;
        let _ = codegen symt e2 in
        let addr = codegen symt e2 in
        label ^ ":\n" |> Buffer.add_string code;
        addr
    | If(e1, e2, e3) ->
        let addr1 = codegen symt e1 in
        let label = ("L" ^ string_of_int (new_label())) in
        codegen_jmpz label;
        let _ = codegen symt e2 in
        label ^ ":\n" |> Buffer.add_string code;
        let addr2 = codegen symt e3 in
        addr2
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
