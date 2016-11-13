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
    movl	$0, %edi
    call	exit
    .cfi_endproc
.LFE2:
    .size	print, .-print
    .globl	main
    .type	main, @function
main:
.LFB3:
    .cfi_startproc
    pushq	%rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq	%rsp, %rbp
    .cfi_def_cfa_register 6
    subq	$16, %rsp
    movl	$260, -4(%rbp)
    movl	-4(%rbp), %eax
    movl	%eax, %edi
    pushq $1
    leaq -24(%rbp), %rax
    pushq %rax
    ##offset 2
    movq -32(%rbp), %rax
    pushq %rax
    leaq -40(%rbp), %rax
    pushq %rax
    ##offset 4
    movq -48(%rbp), %rax
    movq (%rax), %rax
    pushq %rax
    ##offset 4
    movq -48(%rbp), %rax
    movq (%rax), %rax
    movq (%rax), %rax
    pushq %rax
    pushq $2
    popq %rax
    popq %rbx
    addq %rax, %rbx
    pushq %rbx
    popq %rax
    popq %rdx
    movq %rax, (%rdx)
    ##offset 4
    movq -48(%rbp), %rax
    movq (%rax), %rax
    movq (%rax), %rax
    pushq %rax
    popq %rdi
    callq print
    movl	$0, %eax
    leave
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE3:
    .size	main, .-main
    .ident	"GCC: (GNU) 6.2.1 20160830"
    .section	.note.GNU-stack,"",@progbits
