open Syntax

let sp = ref 0
let stack_overflow = 1000

let lb = ref 3
let lf = ref 3

let code = Buffer.create 100

let rec lookup s env = match env with
  	 | [] -> failwith "Not found."
  	 | (var, value)::env -> if (String.equal s var) then value else lookup s env

let tab _ = "    "

let x86_operator = function
    | Plus -> "addq %rax, %rbx\n" ^ tab() ^ "pushq %rbx\n"
    | Minus -> "subq %rax, %rbx\n" ^ tab() ^ "pushq %rbx\n"
    | Times -> "mulq %rax, %rbx\n" ^ tab() ^ "pushq %rbx\n"
    (* | Divide -> "divq %rax\n" ^ tab() ^ "pushq %rax\n" *)
    | And -> "andq %rax, %rbx\n" ^ tab() ^ "pushq %rbx\n"
    | Or -> "orq %rax, %rbx\n" ^ tab() ^ "pushq %rbx\n"
    (* | Modulus -> "divq %rax\n" *)
    | Less -> "cmpq %rax, %rbx\n" ^ tab() ^ "setl %al\n" ^ tab() ^ "pushq %rax\n"
    | Leq -> "cmpq %rax, %rbx\n" ^ tab() ^ "setle %al\n" ^ tab()  ^ "pushq %rax\n"
    | Greater -> "cmpq %rax, %rbx\n" ^ tab() ^ "setg %al\n" ^ tab()  ^ "pushq %rax\n"
    | Geq -> "cmpq %rax, %rbx\n"^ tab() ^ "setge %al\n" ^ tab()  ^ "pushq %rax\n"
    | Eq -> "cmpq %rax, %rbx\n" ^ tab() ^ "sete %al\n" ^ tab()  ^ "pushq %rax\n"
    | Noteq -> "cmpq %rax, %rbx\n" ^ tab() ^ "setne %al\n" ^ tab()  ^ "pushq %rax\n"

let codegenx86_st n =
    tab() ^ "pushq $" ^ (string_of_int n) ^ "\n" |> Buffer.add_string code

let codegenx86_new _ =
    tab() ^ "leaq " ^ (string_of_int (-16-8*(!sp))) ^ "(%rbp), %rax\n"
    ^ tab() ^ "pushq %rax\n" |> Buffer.add_string code

let codegenx86_op op =
    tab() ^ "popq %rax\n"
    ^ tab() ^ "popq %rbx\n"
    ^ tab() ^ (x86_operator op) |> Buffer.add_string code

let codegenx86_not _ =
    tab() ^ "popq %rax\n"
    ^ tab() ^ "negq" ^ " %rax\n"
    ^ tab() ^ "pushq %rax\n" |> Buffer.add_string code

let codegenx86_id addr =
    tab() ^ "movq " ^ (string_of_int (-16-8*addr)) ^ "(%rbp), %rax\n"
    ^ tab() ^ "pushq %rax\n" |> Buffer.add_string code

let codegenx86_deref _ =
    tab() ^ "popq %rax\n"
    ^ tab() ^ "movq (%rax), %rax\n"
    ^ tab() ^ "pushq %rax\n" |> Buffer.add_string code

let codegenx86_if label =
    tab() ^ "popq %rax\n"
    ^ tab() ^ "cmpq $0, %rax\n"
    ^ tab() ^ "jz " ^ label ^ "\n"
    |> Buffer.add_string code

let codegenx86_while label =
    tab() ^ "popq %rax\n"
    ^ tab() ^ "cmpq $0, %rax\n"
    ^ tab() ^ "jnz " ^ label ^ "\n"
    |> Buffer.add_string code

let codegenx86_asg _ =
    tab() ^ "popq %rax\n"
    ^ tab() ^ "popq %rbx\n"
    ^ tab() ^ "movq %rax, (%rbx)\n" |> Buffer.add_string code

let codegenx86_let _ =
    tab() ^ "popq %rax\n"
    ^ tab() ^ "popq %rbx \n"
    ^ tab() ^ "pushq %rax\n"
    |> Buffer.add_string code

let rec codegenx86_formal_arg i =
    let res = (match i with
        | 1 -> tab() ^ "pushq %rdi\n"
        | 2 -> tab() ^ "pushq %rsi\n"
        | 3 -> tab() ^ "pushq %rex\n"
        | 4 -> tab() ^ "pushq %dcx\n"
        | 5 -> tab() ^ "pushq %r8\n"
        | 6 -> tab() ^ "pushq %r9\n"
        | _ -> failwith "Too many function arguments.") in
    res |> Buffer.add_string code;
    i+1

let codegenx86_actual_arg i =
    let res = (match i with
        | 1 -> tab() ^ "popq %rdi\n"
        | 2 -> tab() ^ "popq %rsi\n"
        | 3 -> tab() ^ "popq %rex\n"
        | 4 -> tab() ^ "popq %dcx\n"
        | 5 -> tab() ^ "popq %r8\n"
        | 6 -> tab() ^ "popq %r9\n"
        | _ -> failwith "Too many function arguments.") in
    res |> Buffer.add_string code;
    i+1

let rec codegenx86 symt expression =
    if(!sp > stack_overflow) then failwith "Stack overflow."
    else (match expression with
        | Operator(op, e1, e2) ->
            codegenx86 symt e1;
            codegenx86 symt e2;
            codegenx86_op op;
            sp := !sp - 1
        | Negate(e) ->
            codegenx86 symt e;
            codegenx86_not();
            sp := !sp - 1
        | Identifier(s) ->
            let addr = lookup s symt in
            codegenx86_id addr;
            sp := !sp + 1
        | Deref(e) ->
            codegenx86 symt e;
            codegenx86_deref();
            sp := !sp + 1
        | MyNull ->
            codegenx86_st 0;
            sp := !sp + 1
        | MyInteger(n) ->
            codegenx86_st n;
            sp := !sp + 1
        | MyBoolean(b) ->
            if(b) then codegenx86_st 1
            else codegenx86_st 0;
            sp := !sp + 1
        | Seq(e1, e2) ->
            codegenx86 symt e1;
            codegenx86 symt e2
        | If(e1, e2, e3) ->
            codegenx86 symt e1;
            let label1 = ".L" ^ (string_of_int !lb) in
            lb := !lb + 1;
            let label2 = ".L" ^ (string_of_int !lb) in
            lb := !lb + 1;
            codegenx86_if label1;
            codegenx86 symt e2;
            tab() ^ "jmp " ^ label2 ^ "\n"
            ^ label1 ^ ":\n" |> Buffer.add_string code;
            codegenx86 symt e3;
            label2 ^ ":\n" |> Buffer.add_string code
        | While(e1, e2) ->
            let label1 = ".L" ^ (string_of_int !lb) in
            lb := !lb + 1;
            let label2 = ".L" ^ (string_of_int !lb) in
            lb := !lb + 1;
            tab() ^ "jmp " ^ label1 ^ "\n" |> Buffer.add_string code;
            label2^ ":\n" |> Buffer.add_string code;
            codegenx86 symt e2;
            label1^ ":\n" |> Buffer.add_string code;
            codegenx86 symt e1;
            codegenx86_while label2;
        | Asg(e1, e2) ->
            codegenx86 symt e1;
            codegenx86 symt e2;
            codegenx86_asg ();
            sp := !sp - 2
        | New(s, e1, e2) ->
            codegenx86 symt e1;
            codegenx86_new();
            sp := !sp + 1;
            codegenx86 ((s, !sp) :: symt) e2;
        | Let(s, e1, e2) ->
            codegenx86 symt e1;
            codegenx86 ((s, !sp) :: symt) e2;
            codegenx86_let ()
        | Application(Identifier(s), ps) ->
            let i = ref 1 in
            let _ = List.map (fun p -> codegenx86 symt p; i := codegenx86_actual_arg !i) ps in
            tab() ^ "callq " ^ s ^ "\n" |> Buffer.add_string code;
            sp := !sp + 1;
            tab() ^ "pushq %rax\n" |> Buffer.add_string code
        | _ -> failwith "Not implemented.")

let prefix_function name id =
"\n" ^ tab() ^ ".globl	" ^ name ^ "\n"
^ tab() ^ ".type	" ^ name ^ ", @function\n"
^ name ^ ":\n" ^ ".LFB" ^ (string_of_int id) ^ ":\n"
^ tab() ^ ".cfi_startproc\n"
^ tab() ^ "pushq	%rbp\n"
^ tab() ^ ".cfi_def_cfa_offset 16\n"
^ tab() ^ ".cfi_offset 6, -16\n"
^ tab() ^ "movq	%rsp, %rbp\n"
^ tab() ^ ".cfi_def_cfa_register 6\n"
^ tab() ^ "subq	$16, %rsp\n"

let suffix_function name =
tab () ^ "popq %rax\n"
^ tab () ^ "popq %rbp\n"
^ tab () ^ ".cfi_def_cfa 7, 8\n"
^ tab () ^ "ret\n"
^ tab () ^ ".cfi_endproc\n"
^ ".LFE" ^ (string_of_int !lf) ^ ":\n"
^ tab() ^ ".size	" ^ name ^ ", .-" ^ name

let prefix_main id =
prefix_function "main" id
^ tab() ^ "movq	$260, -8(%rbp)\n"
^ tab() ^ "movq	-8(%rbp), %rax\n"

let suffix_main id =
tab() ^ "popq %rdi\n"
^ tab() ^ "callq print\n"
^ tab() ^ "movq	$0, %rax\n"
^ tab() ^ "leave\n"
^ tab() ^ ".cfi_def_cfa 7, 8\n"
^ tab() ^ "ret\n"
^ tab() ^ ".cfi_endproc\n"
^ ".LFE" ^ (string_of_int id) ^ ":\n"
^ tab() ^ ".size	" ^ "main" ^ ", .-" ^ "main" ^ "\n"
^ tab() ^ ".ident	\"GCC: (GNU) 6.2.1 20160830\"\n"
^ tab () ^ ".section	.note.GNU-stack,\"\",@progbits\n"

let rec generatex86_program symt program =
    sp := 0;
    match program with
        | [] ->
            "\n" |> Buffer.add_string code
        | ("main", [], expression)::[] ->
            prefix_main (!lf) |> Buffer.add_string code;
            codegenx86 symt expression;
            suffix_main (!lf) |> Buffer.add_string code
        | (s, ps, expression)::prog ->
            prefix_function s (!lf) |> Buffer.add_string code;
            let i = ref 1 in
            let _ = List.map (fun p -> i := codegenx86_formal_arg !i) ps in
            (* sp := !sp - 1; *)
            let symt = List.fold_right (fun p a -> sp := !sp + 1; codegenx86_new(); a@[(p, !sp)]) ps [] in
            sp := 0;
            codegenx86 ((s, !lf)::symt) expression;
            suffix_function s |> Buffer.add_string code;
            lf := !lf + 1;
            generatex86_program ((s, !lf)::symt) prog

let generatex86 program =
    Buffer.reset code;
    lb := 3;
    lf := 3;
    generatex86_program [] program;
    Buffer.output_buffer stdout code