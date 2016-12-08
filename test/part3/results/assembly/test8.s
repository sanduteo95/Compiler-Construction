    .file	"program.c"
    .section	.rodata
.LC0:
    .string	"%d\n"
    .text
    .globl	print
    .type	print, @function
print:
.LFB2:
    .cfi_startproc
    pushq	%rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq	%rsp, %rbp
    .cfi_def_cfa_register 6
    subq	$16, %rsp
    movl	%edi, -4(%rbp)
    movl	-4(%rbp), %eax
    movl	%eax, %esi
    movl	$.LC0, %edi
    movl	$0, %eax
    call	printf
    nop
    leave
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE2:
    .size	print, .-print
.LC1:
    .string	"%d"
    .text
    .globl	read
    .type	read, @function
read:
.LFB3:
    .cfi_startproc
    pushq	%rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq	%rsp, %rbp
    .cfi_def_cfa_register 6
    subq	$16, %rsp
    leaq	-4(%rbp), %rax
    movq	%rax, %rsi
    movl	$.LC1, %edi
    movl	$0, %eax
    call	__isoc99_scanf
    movl	-4(%rbp), %eax
    leave
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE3:
    .size	read, .-read
    .globl	create
    .type	create, @function
create:
.LFB4:
    .cfi_startproc
    pushq	%rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq	%rsp, %rbp
    .cfi_def_cfa_register 6
    subq	$16, %rsp
    pushq %rsi
    leaq -24(%rbp), %rax
    pushq %rax
    pushq %rdi
    leaq -40(%rbp), %rax
    pushq %rax
    movq -48(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    movq -32(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    leaq -64(%rbp), %rax
    pushq %rax
    movq -72(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    popq %rax
    popq %rbx
    popq %rbx
    pushq %rax
    popq %rax
    movq %rbp, %rsp
    popq %rbp
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE4:
    .size	create, .-create
    .globl	compare
    .type	compare, @function
compare:
.LFB5:
    .cfi_startproc
    pushq	%rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq	%rsp, %rbp
    .cfi_def_cfa_register 6
    subq	$16, %rsp
    pushq %rsi
    leaq -24(%rbp), %rax
    pushq %rax
    pushq %rdi
    leaq -40(%rbp), %rax
    pushq %rax
    movq -48(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    movq -32(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    setl %al
    pushq %rax
    popq %rax
    movq %rbp, %rsp
    popq %rbp
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE5:
    .size	compare, .-compare
    .globl	main
    .type	main, @function
main:
.LFB6:
    .cfi_startproc
    pushq	%rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq	%rsp, %rbp
    .cfi_def_cfa_register 6
    subq	$16, %rsp
    pushq $1
    pushq $2
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    setl %al
    pushq %rax
    pushq $2
    pushq $3
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    setl %al
    pushq %rax
    popq %rdi
    callq print
    movq	$0, %rax
    leave
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE6:
    .size	main, .-main
    .ident	"GCC: (GNU) 6.2.1 20160830"
    .section	.note.GNU-stack,"",@progbits
