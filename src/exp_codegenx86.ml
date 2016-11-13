open Syntax

let sp = ref 0
let stack_overflow = 1000

let hp = ref stack_overflow

let heap = Hashtbl.create 100
let code = Buffer.create 100

let rec lookup s env = match env with
  	 | [] -> failwith "Not found."
  	 | (var, value)::env -> if (String.equal s var) then value else lookup s env

let tab _ = "    "

(* let functions = Buffer.create 100 *)
let x86_operator = function
    | Plus -> "addq %rax, %rbx\n" ^ tab() ^ "pushq %rbx\n"
    | Minus -> "subq %rax, %rbx\n" ^ tab() ^ "pushq %rbx\n"
    | Times -> "mulq %rax, %rbx\n" ^ tab() ^ "pushq %rbx\n"
    (* | Divide -> "divq %rax\n" ^ tab() ^ "pushq %rax\n" *)
    | And -> "andq %rax, %rbx\n" ^ tab() ^ "pushq %rbx\n"
    | Or -> "orq %rax, %rbx\n" ^ tab() ^ "pushq %rbx\n"
    (* | Modulus -> "divq %rax\n" *)
    | Less -> "cmpq %rax, %rbx\n" ^ tab() ^ "setl %al\n" ^ tab() ^ "pushq %rax\n"
    | Leq -> "cmpq %rax, %rbx\n" ^ tab() ^ "setle %al\n" ^ "pushq %rax\n"
    | Greater -> "cmpq %rax, %rbx\n" ^ tab() ^ "setg %al\n" ^ "pushq %rax\n"
    | Geq -> "cmpq %rax, %rbx\n"^ tab() ^ "setge %al\n" ^ "pushq %rax\n"
    | Eq -> "cmpq %rax, %rbx\n" ^ tab() ^ "sete %al\n" ^ "pushq %rax\n"
    | Noteq -> "cmpq %rax, %rbx\n" ^ tab() ^ "setne %al\n" ^ "pushq %rax\n"

let codegenx86_st n =
    tab() ^ "pushq $" ^ (string_of_int n)
    ^ "\n" |> Buffer.add_string code

let codegenx86_new _ =
    hp := !hp + 1;
    Hashtbl.add heap !sp !hp;
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
    tab() ^ "##offset " ^ (string_of_int addr) ^ "\n"
    ^ tab() ^ "movq " ^ (string_of_int (-16-8*addr)) ^ "(%rbp), %rax\n"
    ^ tab() ^ "pushq %rax\n" |> Buffer.add_string code

let codegenx86_deref addr =
    tab() ^ "##offset " ^ (string_of_int addr) ^ "\n"
    ^ tab() ^ "movq " ^ (string_of_int (-16-8*addr)) ^ "(%rbp), %rax\n"
    ^ tab() ^ "movq (%rax), %rax\n"
    ^ tab() ^ "pushq %rax\n" |> Buffer.add_string code

let codegenx86_deref_deref addr =
    tab() ^ "##offset " ^ (string_of_int addr) ^ "\n"
    ^ tab() ^ "movq " ^ (string_of_int (-16-8*addr)) ^ "(%rbp), %rax\n"
    ^ tab() ^ "movq (%rax), %rax\n"
    ^ tab() ^ "movq (%rax), %rax\n"
    ^ tab() ^ "pushq %rax\n" |> Buffer.add_string code

let codegen_asg _ =
    tab() ^ "popq %rax\n"
    ^ tab() ^ "popq %rdx\n"
    ^ tab() ^ "movq %rax, (%rdx)\n" |> Buffer.add_string code

let codegenx86_let _ =
    tab() ^ "popq %rax\n"
    ^ tab() ^ "popq %rbx \n"
    ^ tab() ^ "pushq %rax\n"
    |> Buffer.add_string code

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
        | Deref(Identifier(s)) ->
            let addr = lookup s symt in
            codegenx86_deref addr;
            sp := !sp + 1
        | Deref(Deref(Identifier(s))) ->
            let addr = lookup s symt in
            codegenx86_deref_deref addr;
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
        | Asg(e1, e2) ->
            codegenx86 symt e1;
            codegenx86 symt e2;
            codegen_asg ();
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
        | _ -> failwith "Not implemented.")

let generatex86_program program = match program with
    | [] ->
        "\n" |> Buffer.add_string code
    | ("main", [], expression)::[] ->
        Buffer.reset code;
        sp := 0;
        hp := stack_overflow;
        "\n" |> Buffer.add_string code;
        codegenx86 [] expression;
        Buffer.output_buffer stdout code
    | _ -> failwith "Not implemented."

let generatex86 = generatex86_program