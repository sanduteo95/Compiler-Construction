open Syntax
open Instruction_set
open Exp_store

let code = Buffer.create 100

let string_of_operator = function
    | Plus -> "add"
    | Minus -> "sub"
    | Times -> "mul"
    | Divide -> "div"
    | And -> "and"
    | Or -> "or"
    | Modulus -> "mod"
    | _ -> failwith "Not possible."

let codegen_op (op, addr1, addr2) =
    "\t" ^ (string_of_operator op)
    ^ " r" ^ (string_of_int addr1)
    ^ ", r" ^ (string_of_int addr2)
    ^ "\n" |> Buffer.add_string code

let codegen_not addr =
    "\t" ^ "not" ^ " r" ^ (string_of_int addr)
    ^ "\n" |> Buffer.add_string code

let codegen_mv addr1 addr2 =
    "\t" ^ "mv r" ^ (string_of_int addr1)
    ^ ", r" ^ (string_of_int addr2)
    ^ "\n" |> Buffer.add_string code

let codegen_st addr =
    "\t" ^ "st r" ^ (string_of_int addr)
    ^ "\n" |> Buffer.add_string code

let codegen_ldr addr =
    "\t" ^ "ld r" ^ (string_of_int addr)
    ^ "\n" |> Buffer.add_string code

let codegen_ldc n =
    "\t" ^ "ld " ^ (string_of_int n)
    ^ "\n" |> Buffer.add_string code

let codegen_jmpz label =
    "\t" ^ "jmpz " ^ label
    ^ "\n" |> Buffer.add_string code

let codegen_jmp label =
    "\t" ^ "jmp " ^ label
    ^ "\n" |> Buffer.add_string code

let codegen_slt addr1 addr2 =
    "\t" ^ "slt r"^ (string_of_int addr1)
    ^ ", r" ^ (string_of_int addr2)
    ^ "\n" |> Buffer.add_string code

let codegen_sle addr1 addr2 =
    "\t" ^ "sle r"^ (string_of_int addr1)
    ^ ", r" ^ (string_of_int addr2)
    ^ "\n" |> Buffer.add_string code

let codegen_sgt addr1 addr2 =
    "sgt r"^ (string_of_int addr1)
    ^ ", r" ^ (string_of_int addr2)
    ^ "\n" |> Buffer.add_string code

let codegen_sge addr1 addr2 =
    "\t" ^ "sge r"^ (string_of_int addr1)
    ^ ", r" ^ (string_of_int addr2)
    ^ "\n" |> Buffer.add_string code

let codegen_seq addr1 addr2 =
    "\t" ^ "seq r"^ (string_of_int addr1)
    ^ ", r" ^ (string_of_int addr2)
    ^ "\n" |> Buffer.add_string code

let codegen_sne addr1 addr2 =
    "\t" ^ "sne r"^ (string_of_int addr1)
    ^ ", r" ^ (string_of_int addr2)
    ^ "\n" |> Buffer.add_string code

let rec codegen symt = function
    | Operator(operator, e1, e2) ->
        let addr1 = codegen symt e1 in
        let addr2 = codegen symt e2 in
        (match operator with
            | Plus | Minus | Times | Divide | And | Or | Modulus ->
                codegen_op (operator, addr1, addr2)
            | _ -> (match operator with
                    | Less -> codegen_slt addr1 addr2
                    | Leq -> codegen_sle addr1 addr2
                    | Greater -> codegen_sgt addr1 addr2
                    | Geq -> codegen_sge addr1 addr2
                    | Eq -> codegen_seq addr1 addr2
                    | Noteq -> codegen_sne addr1 addr2));
        addr_base := addr1;
        codegen_st addr1;
        addr1
    | Negate(e) ->
        let addr = codegen symt e in
        codegen_not addr;
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
        let loop_label = ("LOOP" ^ string_of_int (new_label())) in
        "\n" ^ loop_label ^ ":\n" |> Buffer.add_string code;
        let addr1 = codegen symt e1 in
        let branch = "BRANCH" ^ string_of_int (new_label()) in
        codegen_jmpz branch;
        let addr = codegen symt e2 in
        codegen_jmp loop_label;
        "\n" ^ branch ^ ":\n" |> Buffer.add_string code;
        addr
    | If(e1, e2, e3) ->
        let addr1 = codegen symt e1 in
        let branch = "BRANCH" ^ string_of_int (new_label()) in
        codegen_jmpz branch;
        let _ = codegen symt e2 in
        let end_if = "END" ^ string_of_int (new_label()) in
        codegen_jmp end_if;
        "\n" ^ branch ^ ":\n" |> Buffer.add_string code;
        let addr2 = codegen symt e3 in
        end_if ^ ":\n" |> Buffer.add_string code;
        addr2
    | Read ->
        codegen_ldr read_addr;
        read_addr
    | Print(e) ->
        let addr = codegen symt e in
        codegen_mv addr print_addr; (* register for printing *)
        print_addr
    | Nothing -> -1
    | _ -> failwith "Not implemented yet."

let generate program = match program with
    | [] ->
        Buffer.reset code;
        "" |> print_endline
    | ("main", [], expression)::[] ->
        Buffer.reset code;
        addr_base := 0;
        "\n" ^  "main" ^ ":\n" |> Buffer.add_string code;
        let addr = codegen [] expression in
        Buffer.output_buffer stdout code;
        "\t" ^ "ld r" ^ (string_of_int addr) |> print_endline
    | _ ->  failwith "Not implemented yet."
