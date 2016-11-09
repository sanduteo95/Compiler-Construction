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
    | Less -> "slt"
    | Leq -> "sle"
    | Greater -> "sgt"
    | Geq -> "sge"
    | Eq -> "seq"
    | Noteq -> "sne"

let string_of_addr addr =
    if addr < stack_overflow then
        if addr==0 then " s"^(string_of_int addr) ^ " # REGISTER FOR READING"
        else
            if addr==1 then " s"^(string_of_int addr) ^ " # REGISTER FOR PRINTING"
            else " s"^(string_of_int addr)
    else " r"^(string_of_int addr)

let label_name incr name =
    if(incr) then name ^ string_of_int(new_label())
    else name ^ string_of_int(!label_addr)

let codegen_label label =
    "\n" ^ label ^ ":\n" |> Buffer.add_string code

let codegen_op (op, addr1, addr2) =
    "\t" ^ (string_of_operator op)
    ^ (string_of_addr addr1)
    ^ "," ^ (string_of_addr addr2)
    ^ "\n" |> Buffer.add_string code

let codegen_not addr =
    "\t" ^ "not"  ^ (string_of_addr addr)
    ^ "\n" |> Buffer.add_string code

let codegen_mv addr1 addr2 =
    "\t" ^ "mv" ^ (string_of_addr addr1)
    ^ "," ^ (string_of_addr addr2)
    ^ "\n" |> Buffer.add_string code

let codegen_st addr =
    "\t" ^ "st" ^ (string_of_addr addr)
    ^ "\n" |> Buffer.add_string code

let codegen_ldr addr =
    "\t" ^ "ld" ^ (string_of_addr addr)
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

let rec codegen symt = function
    | Operator(operator, e1, e2) ->
        let addr1 = codegen symt e1 in
        let addr2 = codegen symt e2 in
        codegen_op (operator, addr1, addr2);
        stack_addr := addr1;
        codegen_st addr1;
        addr1
    | Negate(e) ->
        let addr = codegen symt e in
        codegen_not addr;
        stack_addr := addr;
        codegen_st addr;
        addr
    | Identifier(s) ->
        let addr = lookup s symt in
        let addr'= new_stack_addr() in
        codegen_mv addr addr';
        addr'
    | Deref(e) ->
        let addr = codegen symt e in
        codegen_ldr addr;
        let addr' = !acc in
        let addr'' = new_stack_addr() in
        codegen_ldr addr';
        codegen_st addr'';
        addr''
    | MyNull ->
        let addr = new_stack_addr() in
        codegen_ldc 0;
        codegen_st addr;
        addr
    | MyInteger(i) ->
        let addr = new_stack_addr() in
        codegen_ldc i;
        codegen_st addr;
        addr
    | MyBoolean(b) ->
        let addr = new_stack_addr() in
        if(b) then codegen_ldc 1 else codegen_ldc 0;
        codegen_st addr;
        addr
    | Seq(e1, e2) ->
        let addr1 = codegen symt e1 in
        codegen symt e2
    | Asg(e1, e2) ->
        let addr1 = codegen symt e1 in
        codegen_ldr addr1;
        let haddr = !acc in
        let addr2 = codegen symt e2 in
        codegen_mv addr2 haddr;
        stack_addr := addr1;
        codegen_ldc haddr;
        codegen_st addr2;
        addr2
    | New(s, e1, e2) ->
        let addr1 = codegen symt e1 in
        let saddr = new_stack_addr() in
        let haddr = new_heap_addr() in
        codegen_ldc haddr;
        codegen_st saddr;
        codegen_mv addr1 haddr;
        codegen ((s, saddr)::symt) e2
    | Let(s, e1, e2) ->
        let addr1 = codegen symt e1 in
        let addr2 = codegen ((s, addr1)::symt) e2 in
        (if addr2 != addr1 then codegen_mv addr2 addr1);
        stack_addr := addr1;
        addr1
    | While(e1, e2) ->
        let initial_addr = !stack_addr in
        let loop = label_name true "LOOP" in
        codegen_label loop;
        let addr1 = codegen symt e1 in
        let end_loop = label_name false "END_LOOP" in
        codegen_jmpz end_loop;
        stack_addr := initial_addr;
        let addr = codegen symt e2 in
        codegen_jmp loop;
        codegen_label end_loop;
        addr
    | For(s, e1, e2, e3) ->
        let initial_addr = !stack_addr in
        let addr1 = codegen symt e1 in
        let addr2 = codegen symt e2 in
        let loop = label_name true "LOOP" in
        codegen_label loop;
        codegen_op (Leq, addr1, addr2);
        stack_addr := addr1;
        let end_loop = label_name false "END_LOOP" in
        codegen_jmpz end_loop;
        let addr = codegen ((s, addr1)::symt) e3 in
        stack_addr := initial_addr;
        let addr' = new_stack_addr() in
        codegen_ldc 1;
        codegen_st addr';
        codegen_op (Plus, addr1, addr');
        codegen_st addr';
        codegen_jmp loop;
        codegen_label end_loop;
        addr
    | If(e1, e2, e3) ->
        let addr1 = codegen symt e1 in
        codegen_ldr addr1;
        let branch = label_name true "BRANCH" in
        codegen_jmpz branch;
        let _ = codegen symt e2 in
        let end_branch = label_name false "END_BRANCH" in
        codegen_jmp end_branch;
        codegen_label branch;
        let addr = codegen symt e3 in
        codegen_label end_branch;
        addr
    | Read ->
        codegen_ldr read_addr;
        let addr = new_stack_addr() in
        codegen_st addr;
        addr
    | Print(e) ->
        let addr = codegen symt e in
        codegen_mv addr print_addr; (* register for printing *)
        stack_addr := addr;
        print_addr
    | Nothing -> -1
    | _ -> failwith "Not implemented yet."

let generate program = match program with
    | [] ->
        Buffer.reset code;
        "" |> print_endline
    | ("main", [], expression)::[] ->
        Buffer.reset code;
        stack_addr := print_addr + 1;
        "\n" ^  "main" ^ ":\n" |> Buffer.add_string code;
        let addr = codegen [] expression in
        Buffer.output_buffer stdout code;
        "\t" ^ "ld" ^ (string_of_addr addr) |> print_endline
    | _ ->  failwith "Not implemented yet."
