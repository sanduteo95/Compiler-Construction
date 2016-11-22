open Syntax
open X86_code
open Exp_store

let sp = ref 0

let lb = ref 3
let lf = ref 4

let break_lb = ref 0
let cont_lb = ref 0

let code = Buffer.create 100

let x86_operator = function
    | Plus -> "addq %rax, %rbx\n" ^ tab ^ "pushq %rbx\n"
    | Minus -> "subq %rax, %rbx\n" ^ tab ^ "pushq %rbx\n"
    | Times -> "mulq %rbx\n" ^ tab ^ "pushq %rax\n"
    | Divide -> "push %rax\n" ^ tab ^ "push %rbx\n" ^ tab ^ "pop %rax\n" ^ tab ^ "pop %rbx\n" ^ tab ^ "cltd\n" ^ tab ^ "divq %rbx\n" ^ tab ^ "pushq %rax\n"
    | And -> "andq %rax, %rbx\n" ^ tab ^ "pushq %rbx\n"
    | Or -> "orq %rax, %rbx\n" ^ tab ^ "pushq %rbx\n"
    | Modulus -> "push %rax\n" ^ tab ^ "push %rbx\n" ^ tab ^ "pop %rax\n" ^ tab ^ "pop %rbx\n" ^ tab ^ "cltd\n" ^ tab ^ "divq %rbx\n" ^ tab ^ "pushq %rdx\n"
    | Less -> "cmpq %rax, %rbx\n" ^ tab ^ "setl %al\n" ^ tab ^ "pushq %rax\n"
    | Leq -> "cmpq %rax, %rbx\n" ^ tab ^ "setle %al\n" ^ tab  ^ "pushq %rax\n"
    | Greater -> "cmpq %rax, %rbx\n" ^ tab ^ "setg %al\n" ^ tab  ^ "pushq %rax\n"
    | Geq -> "cmpq %rax, %rbx\n"^ tab ^ "setge %al\n" ^ tab  ^ "pushq %rax\n"
    | Eq -> "cmpq %rax, %rbx\n" ^ tab ^ "sete %al\n" ^ tab  ^ "pushq %rax\n"
    | Noteq -> "cmpq %rax, %rbx\n" ^ tab ^ "setne %al\n" ^ tab  ^ "pushq %rax\n"

let x86_addr addr = string_of_int (-16 -8 * addr)

let codegenx86_st n =
    tab ^ "pushq $" ^ (string_of_int n) ^ "\n"
    |> Buffer.add_string code;
    sp := !sp + 1

let codegenx86_new _ =
    tab ^ "leaq " ^ (x86_addr (!sp)) ^ "(%rbp), %rax\n" ^ tab ^ "pushq %rax\n"
    |> Buffer.add_string code;
    sp := !sp + 1

let codegenx86_op op =
    tab ^ "popq %rax\n" ^ tab ^ "popq %rbx\n" ^ tab ^ (x86_operator op)
    |> Buffer.add_string code;
    sp := !sp - 1

let codegenx86_not _ =
    tab ^ "popq %rax\n" ^ tab ^ "cmpq	$0, %rax\n" ^ tab ^ "sete %al\n" ^ tab ^ "pushq %rax\n" ^ tab ^ "popq %rax\n" ^ tab ^ "pushq %rax\n"
    |> Buffer.add_string code

let codegenx86_id addr =
    tab ^ "movq " ^ (x86_addr addr) ^ "(%rbp), %rax\n" ^ tab ^ "pushq %rax\n"
    |> Buffer.add_string code;
    sp := !sp + 1

let codegenx86_deref _ =
    tab ^ "popq %rax\n" ^ tab ^ "movq (%rax), %rax\n" ^ tab ^ "pushq %rax\n"
    |> Buffer.add_string code

let codegenx86_labels _ =
    let label1 = ".L" ^ (string_of_int !lb) in lb := !lb + 1;
    let label2 = ".L" ^ (string_of_int !lb) in lb := !lb + 1;
    (label1, label2)

let codegenx86_if label =
    tab ^ "popq %rax\n" ^ tab ^ "cmpq $0, %rax\n" ^ tab ^ "jz " ^ label ^ "\n"
    |> Buffer.add_string code;
    sp := !sp - 1

let codegenx86_while label1 label2 =
    tab ^ "popq %rax\n" ^ tab ^ "cmpq $0, %rax\n" ^ tab ^ "jnz " ^ label1 ^ "\n" ^ label2^ ":\n"
    |> Buffer.add_string code;
    sp := !sp - 1

let codegenx86_asg _ =
    tab ^ "popq %rbx\n" ^ tab ^ "popq %rax\n" ^ tab ^ "movq %rax, (%rbx)\n" ^ tab ^ "pushq %rax\n"
    |> Buffer.add_string code;
    sp := !sp - 1

let codegenx86_let _ =
    tab ^ "popq %rax\n" ^ tab ^ "popq %rbx \n" ^ tab ^ "pushq %rax\n"
    |> Buffer.add_string code;
    sp := !sp - 1

let arg i = match i with
    | 1 -> "%rdi"
    | 2 -> "%rsi"
    | 3 -> "%rdx"
    | 4 -> "%rcx"
    | 5 -> "%r8"
    | 6 -> "%r9"
    | _ -> failwith "Too many function arguments."

let rec codegenx86_formal_arg n i =
    (if(i<=6) then
        tab ^ "pushq " ^ (arg i) ^ "\n"
    else
        tab ^ "movq " ^ (string_of_int (16 + 8 * (n - i))) ^ "(%rbp), %rax\n" ^ tab ^ "pushq %rax\n")
    |> Buffer.add_string code;
    sp := !sp + 1;
    i-1

let codegenx86_actual_arg i =
    if(i<=6) then
        tab ^ "popq " ^ (arg i) ^ "\n"
        |> Buffer.add_string code;
        sp := !sp - 1;
    i-1

let rec codegenx86_appl s ps symt =
    let i = ref (List.length ps) in
    let old_sp = !sp in
    let _ = List.map (fun p -> codegenx86 symt p; i := codegenx86_actual_arg !i) (List.rev ps) in
    tab ^ "callq " ^ s ^ "\n" |> Buffer.add_string code;
    sp := old_sp + 1;
    if(List.length ps > 6) then
        tab ^ "addq $" ^ (string_of_int (8*(List.length ps - 6))) ^ ", %rsp\n" |> Buffer.add_string code;
    tab ^ "pushq %rax\n" |> Buffer.add_string code
and
codegenx86 symt expression = match expression with
    | Operator(op, e1, e2) ->
        codegenx86 symt e1;
        codegenx86 symt e2;
        codegenx86_op op
    | Negate(e) ->
        codegenx86 symt e;
        codegenx86_not()
    | Identifier(s) ->
        let addr = lookup s symt in
        codegenx86_id addr
    | Deref(e) ->
        codegenx86 symt e;
        codegenx86_deref()
    | MyNull ->
        codegenx86_st 0
    | MyInteger(n) ->
        codegenx86_st n
    | MyBoolean(b) ->
        if(b) then codegenx86_st 1
        else codegenx86_st 0
    | Seq(e1, e2) ->
        codegenx86 symt e1;
        codegenx86 symt e2;
        codegenx86_let()
    | If(e1, e2, e3) ->
        let (label1, label2) = codegenx86_labels() in
        codegenx86 symt e1;
        codegenx86_if label1;
        let old_sp = !sp in
        codegenx86 symt e2;
        tab ^ "jmp " ^ label2 ^ "\n"
        ^ label1 ^ ":\n" |> Buffer.add_string code;
        sp := old_sp;
        codegenx86 symt e3;
        label2 ^ ":\n" |> Buffer.add_string code
    | While(e1, e2) ->
        let (label1, label2) = codegenx86_labels() in
        break_lb :=  !lb;
        cont_lb := !lb - 2;
        let label3 =  ".L" ^ (string_of_int !break_lb) in lb := !lb + 1;
        tab ^ "jmp " ^ label1 ^ "\n" |> Buffer.add_string code;
        label2^ ":\n" |> Buffer.add_string code;
        codegenx86 symt e2;
        label1^ ":\n" |> Buffer.add_string code;
        codegenx86 symt e1;
        codegenx86_while label2 label3
    | For(s, e1, e2, e3) ->
        let (label1, label2) = codegenx86_labels() in
        let label3 =  ".L" ^ (string_of_int !lb) in lb := !lb + 1;
        cont_lb := !lb-1;
        let label4 =  ".L" ^ (string_of_int !lb) in lb := !lb + 1;
        break_lb :=  !lb-1;
        codegenx86 symt e1;
        tab ^ "jmp " ^ label1 ^ "\n" |> Buffer.add_string code;
        let s_sp = !sp in
        label2^ ":\n" |> Buffer.add_string code;
        codegenx86 ((s, s_sp)::symt) e3;
        label3^ ":\n" |> Buffer.add_string code;
        tab ^ "popq %rax\n" |> Buffer.add_string code;
        sp := !sp - 1;
        tab ^ "addq $1, " ^ (string_of_int(-16-8*s_sp)) ^ "(%rbp)\n" |> Buffer.add_string code;
        label1^ ":\n" |> Buffer.add_string code;
        codegenx86 ((s, s_sp)::symt) (Operator(Leq, Identifier(s), e2));
        codegenx86_while label2 label4
    | Asg(e1, e2) ->
        codegenx86 symt e2;
        codegenx86 symt e1;
        codegenx86_asg ()
    | New(s, e1, e2) ->
        codegenx86 symt e1;
        codegenx86_new();
        let old_sp = !sp in
        codegenx86 ((s, !sp) :: symt) e2
    | Let(s, e1, e2) ->
        codegenx86 symt e1;
        codegenx86 ((s, !sp) :: symt) e2;
        codegenx86_let ()
    | Application(Identifier(s), ps) ->
        codegenx86_appl s ps symt
    | Print(e) ->
        codegenx86_appl "print" [e] symt
    | Read ->
        codegenx86_appl "read" [] symt
    | Break -> if(!break_lb == 0) then failwith "Break outside of a loop" else tab ^ "jmp " ^ ".L" ^ (string_of_int !break_lb) ^ "\n" |> Buffer.add_string code
    | Continue -> if(!cont_lb == 0) then failwith "Continue outside of a loop" else tab ^ "jmp " ^ ".L" ^ (string_of_int !cont_lb) ^ "\n" |> Buffer.add_string code
    | Nothing ->  ()
    | _ -> failwith "Not implemented."

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
            let i = ref (List.length ps) in
            let f_symt = List.fold_left (fun a p -> i := codegenx86_formal_arg (List.length ps) !i; codegenx86_new(); a@[(p, !sp)]) [] (List.rev ps) in
            codegenx86 ((s, !lf)::f_symt) expression;
            suffix_function s (!lf) |> Buffer.add_string code;
            lf := !lf + 1;
            generatex86_program ((s, !sp)::symt) prog

let generatex86 program =
    Buffer.reset code;
    lb := 3;
    lf := 4;
    break_lb := 0;
    cont_lb := 0;
    prefix_program |> Buffer.add_string code;
    (try generatex86_program [] program with
        | Failure msg -> Buffer.reset code; "error" |> Buffer.add_string code);
    Buffer.output_buffer stdout code