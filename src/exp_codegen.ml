(** Generate code based off of a small instruction set. *)
open Syntax
open Instruction_set
open Exp_store

let code = Buffer.create 100

let functions = Buffer.create 100

let break = ref 0
let cont = ref 0

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
        if addr==0 then " r"^(string_of_int addr) ^ " # REGISTER FOR READING"
        else
            if addr==1 then " r"^(string_of_int addr) ^ " # REGISTER FOR PRINTING"
            else " r"^(string_of_int addr)
    else " r" ^ (string_of_int addr)

let label_name name =
    name ^ string_of_int(!label_addr)

let codegen_label label =
    "\n" ^ label ^ ":\n" |> Buffer.add_string code

let codegen_labels incr =
    let _ = new_label() in
    let label1 = label_name "L1" in
    let label2 = label_name "L2" in
    let label3 = label_name "L3" in
    break := !label_addr;
    cont := !label_addr;
    (label1, label2, label3)

let codegen_op (op, addr1, addr2) =
    "\t" ^ (string_of_operator op) ^ (string_of_addr addr1)
    ^ "," ^ (string_of_addr addr2)
    ^ "\n" |> Buffer.add_string code

let codegen_not addr =
    "\t" ^ "not"  ^ (string_of_addr addr) ^ "\n" |> Buffer.add_string code

let codegen_mv addr1 addr2 =
    "\t" ^ "mv" ^ (string_of_addr addr1)
    ^ "," ^ (string_of_addr addr2)
    ^ "\n" |> Buffer.add_string code

let codegen_st addr =
    "\t" ^ "st" ^ (string_of_addr addr)
    ^ "\n" |> Buffer.add_string code

let codegen_ldc n =
    "\t" ^ "ld " ^ (string_of_int n)
    ^ "\n" |> Buffer.add_string code

let codegen_ldr addr =
    "\t" ^ "ld" ^ (string_of_addr addr)
    ^ "\n" |> Buffer.add_string code

let codegen_ldip addr =
    "\t" ^ "ld bp(" ^ (string_of_int addr) ^")"
    ^ "\n" |> Buffer.add_string code

let codegen_str addr1 addr2 =
    codegen_ldr addr1;
    "\t" ^ "ld acc"
    ^ "\n" |> Buffer.add_string code;
    codegen_st addr2

let codegen_mvr addr1 addr2 =
    codegen_ldr addr1;
    "\t" ^ "mv" ^ (string_of_addr addr2)
    ^ ", acc"
    ^ "\n" |> Buffer.add_string code;
    stack_addr := addr1;
    "\t" ^ "ld acc"
    ^ "\n" |> Buffer.add_string code;
    codegen_st addr2

let codegen_alloc saddr haddr addr =
    codegen_ldc haddr;
    codegen_st saddr;
    codegen_mv addr haddr

let codegen_jmpz label =
    "\t" ^ "jmpz " ^ label
    ^ "\n" |> Buffer.add_string code

let codegen_jmp label =
    "\t" ^ "jmp " ^ label
    ^ "\n" |> Buffer.add_string code

let rec codegen_call s =
    "\t" ^ "call " ^ s
    ^ "\n" |> Buffer.add_string code

let rec codegen symt expression = match expression with
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
        let addr' = new_stack_addr() in
        codegen_str addr addr';
        addr'
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
        let _ = codegen symt e1 in
        codegen symt e2
    | Asg(e1, e2) ->
        let addr1 = codegen symt e1 in
        let addr2 = codegen symt e2 in
        codegen_mvr addr1 addr2;
        addr2
    | New(s, e1, e2) ->
        let addr1 = codegen symt e1 in
        let saddr = new_stack_addr() in
        let haddr = new_heap_addr() in
        codegen_alloc saddr haddr addr1;
        codegen ((s, saddr)::symt) e2
    | Let(s, e1, e2) ->
        let addr1 = codegen symt e1 in
        let addr2 = codegen ((s, addr1)::symt) e2 in
        (if addr2 != addr1 then codegen_mv addr2 addr1);
        stack_addr := addr1;
        addr1
    | While(e1, e2) ->
        let (loop, cont_loop, end_loop) = codegen_labels() in
        let cur_break = !break in
        let cur_cont = !cont in
        let initial_addr = !stack_addr in
        codegen_label loop;
        let _ = codegen symt e1 in
        codegen_jmpz end_loop;
        stack_addr := initial_addr;
        let addr = codegen symt e2 in
        codegen_jmp loop;
        codegen_label end_loop;
        break := cur_break;
        cont := cur_cont;
        addr
    | For(s, e1, e2, e3) ->
        let (loop, cont_loop, end_loop) = codegen_labels() in
        let cur_break = !break in
        let cur_cont = !cont in
        let initial_addr = !stack_addr in
        let addr1 = codegen symt e1 in
        let addr2 = codegen symt e2 in
        codegen_label loop;
        codegen_op (Leq, addr1, addr2);
        stack_addr := addr1;
        codegen_jmpz end_loop;
        let addr = codegen ((s, addr1)::symt) e3 in
        codegen_label cont_loop;
        stack_addr := initial_addr;
        let addr' = new_stack_addr() in
        codegen_ldc 1;
        codegen_st addr';
        codegen_op (Plus, addr1, addr');
        codegen_st addr';
        codegen_jmp loop;
        codegen_label end_loop;
        break := cur_break;
        cont := cur_cont;
        addr
    | If(e1, e2, e3) ->
        let (branch, _, end_branch) = codegen_labels() in
        let addr1 = codegen symt e1 in
        codegen_ldr addr1;
        codegen_jmpz branch;
        let initial_addr = !stack_addr in
        let _ = codegen symt e2 in
        codegen_jmp end_branch;
        stack_addr := initial_addr;
        codegen_label branch;
        let addr = codegen symt e3 in
        codegen_label end_branch;
        addr
    | Read ->
        codegen_ldr read_addr;
        let addr = new_stack_addr() in
        codegen_st addr;
        addr
    | Application(Identifier(s), ps) ->
        let _ = List.fold_left (fun a p -> let addr = codegen symt p in let addr' = new_stack_addr() in codegen_ldr addr; codegen_st addr'; a@[addr']) [] ps in
        let initial_addr = !stack_addr in
        let saddr = new_stack_addr() in
        codegen_ldip initial_addr;
        codegen_st saddr;
        codegen_call s;
        "\t" ^ "ld acc\n" |> Buffer.add_string code;
        codegen_ldr saddr;
        stack_addr := initial_addr;
        st !stack_addr;
        !stack_addr
    | Print(e) ->
        let addr = codegen symt e in
        codegen_mv addr print_addr; (* register for printing *)
        stack_addr := addr;
        print_addr
    | Nothing -> -1
    | Break -> if(!break == -1) then failwith "Break outside of a loop" else (codegen_jmp ("end_loop" ^ (string_of_int !break)); -1)
    | Continue -> if(!cont == -1) then failwith "Continue outside of a loop" else (codegen_jmp ("cont_loop" ^ (string_of_int !break)); -1)
    | _ -> failwith "Not implemented."

let rec gen_prog symt program = match program with
    | [] ->
        "\n" |> Buffer.add_string code
    | ("main", [], expression)::[] ->
        "\n" ^  "main" ^ ":\n" |> Buffer.add_string code;
        let addr = codegen symt expression in
        if(addr <> -1 && addr <> 2) then
            "\t" ^ "ld" ^ (string_of_addr addr) ^ "\n" |> Buffer.add_string code
        else
            "" |> Buffer.add_string code
    | (s, ps, expression)::program ->
        let saddr = new_stack_addr() in
        let initial_addr = !stack_addr in
        "\n" ^ s ^ ":\n" |> Buffer.add_string functions;
        let addrs = (List.fold_left (fun a p -> let saddr = new_stack_addr() in a@[(p, saddr)] ) [] ps) in
        let addr = codegen addrs expression in
        "\t" ^ "ld" ^ (string_of_addr addr) ^ "\n" |> Buffer.add_string functions;
        stack_addr := initial_addr;
        gen_prog ((s, saddr)::symt) program

let generate program =
    Buffer.reset code;
    Buffer.reset functions;
    stack_addr := print_addr + 1;
    let success = ref true in
    (try gen_prog [] program with
        | Failure msg -> success := false; Buffer.reset code; "error" |> Buffer.add_string code);
    if(!success) then Buffer.output_buffer stdout functions;
    Buffer.output_buffer stdout code;
