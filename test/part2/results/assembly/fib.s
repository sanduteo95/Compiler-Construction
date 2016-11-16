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
    .globl	fib
    .type	fib, @function
fib:
.LFB4:
    .cfi_startproc
    pushq	%rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq	%rsp, %rbp
    .cfi_def_cfa_register 6
    subq	$16, %rsp
    ## arg number 1
    pushq %rdi
    leaq -24(%rbp), %rax
    pushq %rax
    pushq $0
    leaq -40(%rbp), %rax
    pushq %rax
    pushq $1
    leaq -56(%rbp), %rax
    pushq %rax
    ##offset 4
    movq -48(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    ##offset 6
    movq -64(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    popq %rax
    popq %rbx
    addq %rax, %rbx
    pushq %rbx
    leaq -72(%rbp), %rax
    pushq %rax
    ##offset 2
    movq -32(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $1
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    sete %al
    pushq %rax
    popq %rax
    cmpq $0, %rax
    jz .L3
    ##offset 4
    movq -48(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    jmp .L4
.L3:
    ##offset 2
    movq -32(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $2
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    sete %al
    pushq %rax
    popq %rax
    cmpq $0, %rax
    jz .L5
    ##offset 6
    movq -64(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    jmp .L6
.L5:
    pushq $2
    leaq -88(%rbp), %rax
    pushq %rax
    jmp .L7
.L8:
    ##offset 4
    movq -48(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    ##offset 6
    movq -64(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    popq %rax
    popq %rbx
    addq %rax, %rbx
    pushq %rbx
    ##offset 8
    movq -80(%rbp), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rax, (%rbx)
    ##offset 6
    movq -64(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    ##offset 4
    movq -48(%rbp), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rax, (%rbx)
    ##offset 8
    movq -80(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    ##offset 6
    movq -64(%rbp), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rax, (%rbx)
    ##offset 10
    movq -96(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $1
    popq %rax
    popq %rbx
    addq %rax, %rbx
    pushq %rbx
    ##offset 10
    movq -96(%rbp), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rax, (%rbx)
.L7:
    ##offset 10
    movq -96(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    ##offset 2
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
    cmpq $0, %rax
    jnz .L8
    ##offset 8
    movq -80(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
.L6:
.L4:
    popq %rax
    movq %rbp, %rsp
    popq %rbp
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE4:
    .size	fib, .-fib
    .globl	main
    .type	main, @function
main:
.LFB5:
    .cfi_startproc
    pushq	%rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq	%rsp, %rbp
    .cfi_def_cfa_register 6
    subq	$16, %rsp
    ## number of arguments 1
    pushq $10
    ## arg number 1
    popq %rdi
    callq fib
    pushq %rax
    popq %rdi
    callq print
    movq	$0, %rax
    leave
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE5:
    .size	main, .-main
    .ident	"GCC: (GNU) 6.2.1 20160830"
    .section	.note.GNU-stack,"",@progbits
