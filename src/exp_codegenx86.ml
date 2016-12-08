(** The assemby code generator. *)
open Syntax
open X86_code
open Exp_store

let sp = ref 0 (** The stack pointer. *)

let lb = ref 3 (** The branch label base.  *)
let lf = ref 4 (** The function label base. *)

let break_lb = ref 0 (** The break label. *)
let cont_lb = ref 0 (** The continue label.  *)

let code = Buffer.create 100

(** Generate the assembly code for various operations. *)
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

(** Generates the offset for the base pointer. *)
let x86_addr addr = string_of_int (-16 -8 * addr)

(** Genearates the assembly code to store constants on the stack. *)
let codegenx86_st n =
    tab ^ "pushq $" ^ (string_of_int n) ^ "\n"
    |> Buffer.add_string code;
    sp := !sp + 1

(** Generates the assembly code to store local variables onto the stack. *)
let codegenx86_new _ =
    tab ^ "leaq " ^ (x86_addr (!sp)) ^ "(%rbp), %rax\n" ^ tab ^ "pushq %rax\n"
    |> Buffer.add_string code;
    sp := !sp + 1

(** Generates the assembly code to clean up local variables from the stack.  *)
let codegenx86_rnew _ =
    tab ^ "popq %rax\n" ^ tab ^ "popq %rbx\n" ^ tab ^ "popq %rbx\n" ^ tab ^ "pushq %rax\n"
    |> Buffer.add_string code;
    sp := !sp - 1

(** Generates the code for assignment. *)
let codegenx86_asg _ =
    tab ^ "popq %rbx\n" ^ tab ^ "popq %rax\n" ^ tab ^ "movq %rax, (%rbx)\n" ^ tab ^ "pushq %rax\n"
    |> Buffer.add_string code;
    sp := !sp - 1

(** Generates the code for let-statements. *)
let codegenx86_let _ =
    tab ^ "popq %rax\n" ^ tab ^ "popq %rbx \n" ^ tab ^ "pushq %rax\n"
    |> Buffer.add_string code;
    sp := !sp - 1

(** Generates the code for the operator. *)
let codegenx86_op op =
    tab ^ "popq %rax\n" ^ tab ^ "popq %rbx\n" ^ tab ^ (x86_operator op)
    |> Buffer.add_string code;
    sp := !sp - 1

(** Generates the code for negation. *)
let codegenx86_not _ =
    tab ^ "popq %rax\n" ^ tab ^ "cmpq	$0, %rax\n" ^ tab ^ "sete %al\n" ^ tab ^ "pushq %rax\n" ^ tab ^ "popq %rax\n" ^ tab ^ "pushq %rax\n"
    |> Buffer.add_string code

(** Generates the code for loading a variable on the stack. *)
let codegenx86_id addr =
    tab ^ "movq " ^ (x86_addr addr) ^ "(%rbp), %rax\n" ^ tab ^ "pushq %rax\n"
    |> Buffer.add_string code;
    sp := !sp + 1

(** Generates the code for dereferencing. *)
let codegenx86_deref _ =
    tab ^ "popq %rax\n" ^ tab ^ "movq (%rax), %rax\n" ^ tab ^ "pushq %rax\n"
    |> Buffer.add_string code

(** Generates the labels. *)
let codegenx86_labels _ =
    let label1 = ".L" ^ (string_of_int !lb) in lb := !lb + 1;
    let label2 = ".L" ^ (string_of_int !lb) in lb := !lb + 1;
    (label1, label2)

(** Gnerates the code for the if-statement condition. *)
let codegenx86_if label =
    tab ^ "popq %rax\n" ^ tab ^ "cmpq $0, %rax\n" ^ tab ^ "jz " ^ label ^ "\n"
    |> Buffer.add_string code;
    sp := !sp - 1

(** Generates the code for the while-loop condition. *)
let codegenx86_while label1 label2 =
    tab ^ "popq %rax\n" ^ tab ^ "cmpq $0, %rax\n" ^ tab ^ "jnz " ^ label1 ^ "\n" ^ label2^ ":\n"
    |> Buffer.add_string code;
    sp := !sp - 1

(** Generates the code for popping a result of the stack. *)
let codegenx86_res _ =
    tab ^ "popq %rax\n" |> Buffer.add_string code;
    sp := !sp - 1

(** Increments the for-loop variable. *)
let codegenx86_incr addr =
    tab ^ "addq $1, " ^ (string_of_int(-16-8*addr)) ^ "(%rbp)\n"
    |> Buffer.add_string code

(** Generates the regsiters for the arguments. *)
let arg i = match i with
    | 1 -> "%rdi"
    | 2 -> "%rsi"
    | 3 -> "%rdx"
    | 4 -> "%rcx"
    | 5 -> "%r8"
    | 6 -> "%r9"
    | _ -> failwith "Too many function arguments."

(** Generates the code loading the function arguments.  *)
let rec codegenx86_formal_arg ps =
    let store_arg n i=
        ((if(i<=6) then
            tab ^ "pushq " ^ (arg i) ^ "\n"
        else
            tab ^ "movq " ^ (string_of_int (16 + 8 * (n - i))) ^ "(%rbp), %rax\n" ^ tab ^ "pushq %rax\n")
        |> Buffer.add_string code;
        sp := !sp + 1;
        i-1) in
    let i = ref (List.length ps) in
    let f_symt = List.fold_left (fun a p -> i := store_arg (List.length ps) !i; codegenx86_new(); a@[(p, !sp)]) [] (List.rev ps) in
    f_symt

(** Generates the code for storing function arguments. *)
let rec codegenx86_actual_arg symt ps =
    let load_arg i =
        (if(i<=6) then
            tab ^ "popq " ^ (arg i) ^ "\n"
            |> Buffer.add_string code;
            sp := !sp - 1;
        i-1) in
    let i = ref (List.length ps) in
    let _ = List.map (fun p -> codegenx86 symt p; i := load_arg !i) (List.rev ps) in ()
and
(** Generates the code for calling functions. *)
codegenx86_appl s ps symt =
    let old_sp = !sp in
    codegenx86_actual_arg symt ps;
    let addr = (try lookup s symt with
        | Exp_errors.VariableDeclaration (_, _) -> -1) in
    if(addr != -1) then tab ^ "movq -" ^ (string_of_int (16 + 8 * addr)) ^ "(%rbp), %rax\n" ^ tab ^ "callq *(%rax)\n" |> Buffer.add_string code
    else tab ^ "callq " ^ s ^ "\n" |> Buffer.add_string code;
    sp := old_sp + 1;
    if(List.length ps > 6) then
        tab ^ "addq $" ^ (string_of_int (8*(List.length ps - 6))) ^ ", %rsp\n" |> Buffer.add_string code;
    tab ^ "pushq %rax\n" |> Buffer.add_string code
and
(** Generates the code for each expression. *)
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
    | Deref(Identifier(s)) when not (is_in s symt) ->
        tab ^ "movq $" ^ s ^ ", %rax\n" ^ tab ^ "pushq %rax\n" |> Buffer.add_string code;
        sp := !sp + 1
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
    (* | MyTuple(es) -> *)
    (* | MyString(s) ->  *)
    (* | MyFloat(f) ->  *)
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
        tab ^ "jmp " ^ label2 ^ "\n" ^ label1 ^ ":\n" |> Buffer.add_string code;
        sp := old_sp;
        codegenx86 symt e3;
        label2 ^ ":\n" |> Buffer.add_string code
    | While(e1, e2) ->
        let (label1, label2) = codegenx86_labels() in
        let cur_break_lb = !break_lb in
        let cur_cont_lb = !cont_lb in
        break_lb :=  !lb;
        cont_lb := !lb - 2;
        let label3 =  ".L" ^ (string_of_int !break_lb) in lb := !lb + 1;
        tab ^ "jmp " ^ label1 ^ "\n" |> Buffer.add_string code;
        label2^ ":\n" |> Buffer.add_string code;
        codegenx86 symt e2;
        codegenx86_res();
        label1^ ":\n" |> Buffer.add_string code;
        codegenx86 symt e1;
        codegenx86_while label2 label3;
        break_lb := cur_break_lb;
        cont_lb := cur_cont_lb
    | For(s, e1, e2, e3) ->
        let (label1, label2) = codegenx86_labels() in
        let label3 =  ".L" ^ (string_of_int !lb) in lb := !lb + 1;
        let cur_break_lb = !break_lb in
        let cur_cont_lb = !cont_lb in
        cont_lb := !lb-1;
        let label4 =  ".L" ^ (string_of_int !lb) in lb := !lb + 1;
        break_lb :=  !lb-1;
        codegenx86 symt e1;
        tab ^ "jmp " ^ label1 ^ "\n" |> Buffer.add_string code;
        let s_sp = !sp in
        label2^ ":\n" |> Buffer.add_string code;
        codegenx86 ((s, s_sp) :: symt) e3;
        label3^ ":\n" |> Buffer.add_string code;
        codegenx86_res();
        codegenx86_incr s_sp;
        label1^ ":\n" |> Buffer.add_string code;
        codegenx86 ((s, s_sp ):: symt) (Operator(Leq, Identifier(s), e2));
        codegenx86_while label2 label4;
        break_lb := cur_break_lb;
        cont_lb := cur_cont_lb
    | Asg(e1, e2) ->
        codegenx86 symt e2;
        codegenx86 symt e1;
        codegenx86_asg ()
    | New(s, e1, e2) ->
        codegenx86 symt e1;
        codegenx86_new();
        codegenx86 ((s, !sp) :: symt) e2;
        codegenx86_rnew()
    | Let(s, e1, e2) ->
        codegenx86 symt e1;
        codegenx86 ((s, !sp) :: symt) e2;
        codegenx86_let ()
    | Application(Identifier(s), ps) ->
        codegenx86_appl s ps symt
    | Application(e, ps) ->
        codegenx86_actual_arg symt ps;
        codegenx86 symt e
    | Print(e) ->
        codegenx86_appl "print" [e] symt
    | Read ->
        codegenx86_appl "read" [] symt
    | Break ->
        if(!break_lb == 0) then failwith "Break outside of a loop"
         else
            (codegenx86_res();
            tab ^ "jmp " ^ ".L" ^ (string_of_int !break_lb) ^ "\n" |> Buffer.add_string code)
    | Continue ->
        if(!cont_lb == 0) then failwith "Continue outside of a loop"
        else tab ^ "jmp " ^ ".L" ^ (string_of_int !cont_lb) ^ "\n" |> Buffer.add_string code
    | Nothing ->  ()
    | Lambda(ps, e) ->
        codegenx86 ((codegenx86_formal_arg ps)@symt) e
    | Block(s, e) ->
        s ^ (string_of_int 1) ^ ":\n" |> Buffer.add_string code;
        codegenx86 ((s, -1)::symt) e;
        s ^ (string_of_int 2) ^ ":\n" |> Buffer.add_string code;
    | BreakBlock(s, e) ->
        if(not (is_in s symt)) then failwith "Labelled break outside of a block"
        else
            (codegenx86 symt e;
            tab ^ "jmp " ^ s ^ (string_of_int 2) ^ "\n" |> Buffer.add_string code)
    | ContinueBlock(s, e) ->
        if(not (is_in s symt)) then failwith "Labelled continue outside of a block"
        else
            (codegenx86 symt e;
            tab ^ "jmp " ^ s ^ (string_of_int 1) ^ "\n" |> Buffer.add_string code)
    | _ -> failwith "Not implemented"

(** Generates the functions' assemlby code. *)
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
            codegenx86 (codegenx86_formal_arg ps) expression;
            suffix_function s (!lf) |> Buffer.add_string code;
            lf := !lf + 1;
            generatex86_program symt prog

(** The main function. *)
let generatex86 program =
    Buffer.reset code;
    lb := 3;
    lf := 4;
    break_lb := 0;
    cont_lb := 0;
    prefix_program |> Buffer.add_string code;
    generatex86_program [] program;
    Buffer.output_buffer stdout code