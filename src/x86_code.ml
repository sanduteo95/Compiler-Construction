(** Contains the prefizes *)

(** The tabbing. *)
let tab = "    "

(** The prefix of the program. *)
let prefix_program =
    tab ^ ".file	\"program.c\"\n"
    ^ tab ^ ".section	.rodata\n"
    ^ ".LC0:\n"
    ^ tab ^ ".string	\"%d\\n\"\n"
    ^ tab ^ ".text\n"
    ^ tab ^ ".globl	print\n"
    ^ tab ^ ".type	print, @function\n"
    ^ "print:\n"
    ^ ".LFB2:\n"
    ^ tab ^ ".cfi_startproc\n"
    ^ tab ^ "pushq	%rbp\n"
    ^ tab ^ ".cfi_def_cfa_offset 16\n"
    ^ tab ^ ".cfi_offset 6, -16\n"
    ^ tab ^ "movq	%rsp, %rbp\n"
    ^ tab ^ ".cfi_def_cfa_register 6\n"
    ^ tab ^ "subq	$16, %rsp\n"
    ^ tab ^ "movl	%edi, -4(%rbp)\n"
    ^ tab ^ "movl	-4(%rbp), %eax\n"
    ^ tab ^ "movl	%eax, %esi\n"
    ^ tab ^ "movl	$.LC0, %edi\n"
    ^ tab ^ "movl	$0, %eax\n"
    ^ tab ^ "call	printf\n"
    ^ tab ^ "nop\n"
    ^ tab ^ "leave\n"
	^ tab ^ ".cfi_def_cfa 7, 8\n"
	^ tab ^ "ret\n"
    ^ tab ^ ".cfi_endproc\n"
    ^ ".LFE2:\n"
    ^ tab ^ ".size	print, .-print\n"
    ^ ".LC1:\n"
	^ tab ^ ".string	\"%d\"\n"
	^ tab ^ ".text\n"
    ^ tab ^ ".globl	read\n"
    ^ tab ^ ".type	read, @function\n"
    ^ "read:\n"
    ^ ".LFB3:\n"
    ^ tab ^ ".cfi_startproc\n"
    ^ tab ^ "pushq	%rbp\n"
    ^ tab ^ ".cfi_def_cfa_offset 16\n"
    ^ tab ^ ".cfi_offset 6, -16\n"
    ^ tab ^ "movq	%rsp, %rbp\n"
    ^ tab ^ ".cfi_def_cfa_register 6\n"
    ^ tab ^ "subq	$16, %rsp\n"
    ^ tab ^ "leaq	-4(%rbp), %rax\n"
    ^ tab ^ "movq	%rax, %rsi\n"
    ^ tab ^ "movl	$.LC1, %edi\n"
    ^ tab ^ "movl	$0, %eax\n"
    ^ tab ^ "call	__isoc99_scanf\n"
    ^ tab ^ "movl	-4(%rbp), %eax\n"
    ^ tab ^ "leave\n"
    ^ tab ^ ".cfi_def_cfa 7, 8\n"
    ^ tab ^ "ret\n"
    ^ tab ^ ".cfi_endproc\n"
    ^ ".LFE3:\n"
    ^ tab ^ ".size	read, .-read"

(** The prefix for each function. *)
let prefix_function name id =
    "\n" ^ tab ^ ".globl	" ^ name ^ "\n"
    ^ tab ^ ".type	" ^ name ^ ", @function\n"
    ^ name ^ ":\n" ^ ".LFB" ^ (string_of_int id) ^ ":\n"
    ^ tab ^ ".cfi_startproc\n"
    ^ tab ^ "pushq	%rbp\n"
    ^ tab ^ ".cfi_def_cfa_offset 16\n"
    ^ tab ^ ".cfi_offset 6, -16\n"
    ^ tab ^ "movq	%rsp, %rbp\n"
    ^ tab ^ ".cfi_def_cfa_register 6\n"
    ^ tab ^ "subq	$16, %rsp\n"

(** The suffix for each function. *)
let suffix_function name addr =
    tab ^ "popq %rax\n"
    ^ tab ^ "movq %rbp, %rsp\n"
    ^ tab ^ "popq %rbp\n"
    ^ tab ^ ".cfi_def_cfa 7, 8\n"
    ^ tab ^ "ret\n"
    ^ tab ^ ".cfi_endproc\n"
    ^ ".LFE" ^ (string_of_int addr) ^ ":\n"
    ^ tab ^ ".size	" ^ name ^ ", .-" ^ name

(** THe prefix for the main function. *)
let prefix_main id =
    prefix_function "main" id

(** The suffix for the main function, *)
let suffix_main id =
    tab ^ "popq %rdi\n"
    ^ tab ^ "callq print\n"
    ^ tab ^ "movq	$0, %rax\n"
    ^ tab ^ "leave\n"
    ^ tab ^ ".cfi_def_cfa 7, 8\n"
    ^ tab ^ "ret\n"
    ^ tab ^ ".cfi_endproc\n"
    ^ ".LFE" ^ (string_of_int id) ^ ":\n"
    ^ tab ^ ".size	" ^ "main" ^ ", .-" ^ "main" ^ "\n"
    ^ tab ^ ".ident	\"GCC: (GNU) 6.2.1 20160830\"\n"
    ^ tab ^ ".section	.note.GNU-stack,\"\",@progbits\n"