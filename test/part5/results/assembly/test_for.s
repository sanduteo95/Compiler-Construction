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
    .globl	main
    .type	main, @function
main:
.LFB4:
    .cfi_startproc
    pushq	%rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq	%rsp, %rbp
    .cfi_def_cfa_register 6
    subq	$16, %rsp
    pushq $1
    movq -24(%rbp), %rax
    pushq %rax
    popq %rdi
    callq print
    pushq %rax
    movq -24(%rbp), %rax
    pushq %rax
    popq %rdi
    callq print
    pushq %rax
    movq -24(%rbp), %rax
    pushq %rax
    popq %rdi
    callq print
    pushq %rax
    movq -24(%rbp), %rax
    pushq %rax
    popq %rdi
    callq print
    pushq %rax
    movq -24(%rbp), %rax
    pushq %rax
    popq %rdi
    callq print
    pushq %rax
    movq -24(%rbp), %rax
    pushq %rax
    popq %rdi
    callq print
    pushq %rax
    movq -24(%rbp), %rax
    pushq %rax
    popq %rdi
    callq print
    pushq %rax
    movq -24(%rbp), %rax
    pushq %rax
    popq %rdi
    callq print
    pushq %rax
    movq -24(%rbp), %rax
    pushq %rax
    popq %rdi
    callq print
    pushq %rax
    movq -24(%rbp), %rax
    pushq %rax
    popq %rdi
    callq print
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    pushq $11
    jmp .L3
.L4:
    movq -32(%rbp), %rax
    pushq %rax
    popq %rdi
    callq print
    pushq %rax
.L5:
    popq %rax
    addq $1, -32(%rbp)
.L3:
    movq -32(%rbp), %rax
    pushq %rax
    pushq $100
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    setle %al
    pushq %rax
    popq %rax
    cmpq $0, %rax
    jnz .L4
.L6:
    popq %rax
    popq %rbx 
    pushq %rax
    pushq $0
    popq %rax
    popq %rbx 
    pushq %rax
    popq %rdi
    callq print
    movq	$0, %rax
    leave
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE4:
    .size	main, .-main
    .ident	"GCC: (GNU) 6.2.1 20160830"
    .section	.note.GNU-stack,"",@progbits
