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
    ## number of arguments 0
    callq read
    pushq %rax
    leaq -24(%rbp), %rax
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
    addq %rax, %rbx
    pushq %rbx
    ##offset 2
    movq -32(%rbp), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rax, (%rbx)
    pushq %rax
    pushq $3
    leaq -48(%rbp), %rax
    pushq %rax
    ##offset 5
    movq -56(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $5
    popq %rax
    popq %rbx
    addq %rax, %rbx
    pushq %rbx
    ##offset 5
    movq -56(%rbp), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rax, (%rbx)
    pushq %rax
    pushq $0
    leaq -72(%rbp), %rax
    pushq %rax
    pushq $1
    jmp .L3
.L4:
    ##offset 8
    movq -80(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    ##offset 9
    movq -88(%rbp), %rax
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
    pushq %rax
    movq -88(%rbp), %rax
    addq $1, %rax
    movq %rax, -88(%rbp)
.L3:
    ##offset 9
    movq -88(%rbp), %rax
    pushq %rax
    pushq $10
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    setle %al
    pushq %rax
    popq %rax
    cmpq $0, %rax
    jnz .L4
    pushq $0
    leaq -104(%rbp), %rax
    pushq %rax
    jmp .L5
.L6:
    ##offset 12
    movq -112(%rbp), %rax
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
    addq %rax, %rbx
    pushq %rbx
    ##offset 12
    movq -112(%rbp), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rax, (%rbx)
    pushq %rax
    ##offset 5
    movq -56(%rbp), %rax
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
    push %rax
    push %rbx
    pop %rax
    pop %rbx
    cltd
    divq %rbx
    pushq %rdx
    pushq $0
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    sete %al
    pushq %rax
    popq %rax
    cmpq $0, %rax
    jz .L7
    ##offset 2
    movq -32(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $1
    popq %rax
    popq %rbx
    addq %rax, %rbx
    pushq %rbx
    ##offset 2
    movq -32(%rbp), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rax, (%rbx)
    pushq %rax
    jmp .L8
.L7:
    ##offset 5
    movq -56(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $1
    popq %rax
    popq %rbx
    subq %rax, %rbx
    pushq %rbx
    ##offset 5
    movq -56(%rbp), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rax, (%rbx)
    pushq %rax
.L8:
.L5:
    ##offset 2
    movq -32(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    ##offset 5
    movq -56(%rbp), %rax
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
    jnz .L6
    ## number of arguments 1
    ##offset 8
    movq -80(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    ##offset 12
    movq -112(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    popq %rax
    popq %rbx
    addq %rax, %rbx
    pushq %rbx
    ## arg number 1
    popq %rdi
    callq print
    pushq %rax
    ##offset 8
    movq -80(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    ##offset 12
    movq -112(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    popq %rax
    popq %rbx
    addq %rax, %rbx
    pushq %rbx
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
