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
    .globl	f
    .type	f, @function
f:
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
    popq %rax
    movq %rbp, %rsp
    popq %rbp
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE4:
    .size	f, .-f
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
    ## number of arguments 0
    callq read
    pushq %rax
    leaq -24(%rbp), %rax
    pushq %rax
    pushq $0
    leaq -40(%rbp), %rax
    pushq %rax
    pushq $10
    pushq $20
    ##offset 5
    movq -56(%rbp), %rax
    pushq %rax
    ##offset 6
    movq -64(%rbp), %rax
    pushq %rax
    popq %rax
    popq %rbx
    mulq %rbx
    pushq %rax
    pushq $3
    popq %rax
    popq %rbx
    mulq %rbx
    pushq %rax
    ##offset 4
    movq -48(%rbp), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rax, (%rbx)
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    pushq $0
    leaq -64(%rbp), %rax
    pushq %rax
    pushq $2
    ## number of arguments 1
    ##offset 8
    movq -80(%rbp), %rax
    pushq %rax
    ## arg number 1
    popq %rdi
    callq f
    pushq %rax
    ##offset 7
    movq -72(%rbp), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rax, (%rbx)
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    ##offset 4
    movq -48(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    ##offset 7
    movq -72(%rbp), %rax
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
.LFE5:
    .size	main, .-main
    .ident	"GCC: (GNU) 6.2.1 20160830"
    .section	.note.GNU-stack,"",@progbits
