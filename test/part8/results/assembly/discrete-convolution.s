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
    .globl	f1
    .type	f1, @function
f1:
.LFB4:
    .cfi_startproc
    pushq	%rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq	%rsp, %rbp
    .cfi_def_cfa_register 6
    subq	$16, %rsp
    pushq %rdi
    leaq -24(%rbp), %rax
    pushq %rax
    movq -32(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $0
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    sete %al
    pushq %rax
    popq %rax
    cmpq $0, %rax
    jz .L3
    pushq $1
    jmp .L4
.L3:
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
    jz .L5
    pushq $1
    jmp .L6
.L5:
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
    jz .L7
    pushq $1
    jmp .L8
.L7:
    movq -32(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $3
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    sete %al
    pushq %rax
    popq %rax
    cmpq $0, %rax
    jz .L9
    pushq $1
    jmp .L10
.L9:
    pushq $1
.L10:
.L8:
.L6:
.L4:
    popq %rax
    movq %rbp, %rsp
    popq %rbp
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE4:
    .size	f1, .-f1
    .globl	f2
    .type	f2, @function
f2:
.LFB5:
    .cfi_startproc
    pushq	%rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq	%rsp, %rbp
    .cfi_def_cfa_register 6
    subq	$16, %rsp
    pushq %rdi
    leaq -24(%rbp), %rax
    pushq %rax
    movq -32(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $0
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    sete %al
    pushq %rax
    popq %rax
    cmpq $0, %rax
    jz .L11
    pushq $1
    jmp .L12
.L11:
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
    jz .L13
    pushq $1
    jmp .L14
.L13:
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
    jz .L15
    pushq $1
    jmp .L16
.L15:
    movq -32(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $3
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    sete %al
    pushq %rax
    popq %rax
    cmpq $0, %rax
    jz .L17
    pushq $1
    jmp .L18
.L17:
    pushq $1
.L18:
.L16:
.L14:
.L12:
    popq %rax
    movq %rbp, %rsp
    popq %rbp
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE5:
    .size	f2, .-f2
    .globl	conv
    .type	conv, @function
conv:
.LFB6:
    .cfi_startproc
    pushq	%rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq	%rsp, %rbp
    .cfi_def_cfa_register 6
    subq	$16, %rsp
    pushq %rcx
    leaq -24(%rbp), %rax
    pushq %rax
    pushq %rdx
    leaq -40(%rbp), %rax
    pushq %rax
    pushq %rsi
    leaq -56(%rbp), %rax
    pushq %rax
    pushq %rdi
    leaq -72(%rbp), %rax
    pushq %rax
    pushq $0
    jmp .L19
.L20:
    pushq $0
    leaq -96(%rbp), %rax
    pushq %rax
    movq -88(%rbp), %rax
    pushq %rax
    movq -32(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $1
    popq %rax
    popq %rbx
    subq %rax, %rbx
    pushq %rbx
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    setge %al
    pushq %rax
    popq %rax
    cmpq $0, %rax
    jz .L23
    movq -88(%rbp), %rax
    pushq %rax
    movq -32(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $1
    popq %rax
    popq %rbx
    subq %rax, %rbx
    pushq %rbx
    popq %rax
    popq %rbx
    subq %rax, %rbx
    pushq %rbx
    jmp .L24
.L23:
    pushq $0
.L24:
    leaq -112(%rbp), %rax
    pushq %rax
    movq -88(%rbp), %rax
    pushq %rax
    movq -64(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $1
    popq %rax
    popq %rbx
    subq %rax, %rbx
    pushq %rbx
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    setl %al
    pushq %rax
    popq %rax
    cmpq $0, %rax
    jz .L25
    movq -88(%rbp), %rax
    pushq %rax
    jmp .L26
.L25:
    movq -64(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $1
    popq %rax
    popq %rbx
    subq %rax, %rbx
    pushq %rbx
.L26:
    leaq -128(%rbp), %rax
    pushq %rax
    movq -120(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    jmp .L27
.L28:
    movq -104(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    movq -144(%rbp), %rax
    pushq %rax
    popq %rdi
    movq -80(%rbp), %rax
    callq *(%rax)
    pushq %rax
    movq -88(%rbp), %rax
    pushq %rax
    movq -144(%rbp), %rax
    pushq %rax
    popq %rax
    popq %rbx
    subq %rax, %rbx
    pushq %rbx
    popq %rdi
    movq -48(%rbp), %rax
    callq *(%rax)
    pushq %rax
    popq %rax
    popq %rbx
    mulq %rbx
    pushq %rax
    popq %rax
    popq %rbx
    addq %rax, %rbx
    pushq %rbx
    movq -104(%rbp), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rax, (%rbx)
    pushq %rax
.L29:
    popq %rax
    addq $1, -144(%rbp)
.L27:
    movq -144(%rbp), %rax
    pushq %rax
    movq -136(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    setle %al
    pushq %rax
    popq %rax
    cmpq $0, %rax
    jnz .L28
.L30:
    movq -104(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    popq %rdi
    callq print
    pushq %rax
    popq %rax
    popq %rbx 
    pushq %rax
    popq %rax
    popq %rbx
    popq %rbx
    pushq %rax
    popq %rax
    popq %rbx
    popq %rbx
    pushq %rax
    popq %rax
    popq %rbx
    popq %rbx
    pushq %rax
.L21:
    popq %rax
    addq $1, -88(%rbp)
.L19:
    movq -88(%rbp), %rax
    pushq %rax
    movq -64(%rbp), %rax
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
    addq %rax, %rbx
    pushq %rbx
    pushq $2
    popq %rax
    popq %rbx
    subq %rax, %rbx
    pushq %rbx
    popq %rax
    popq %rbx
    cmpq %rax, %rbx
    setle %al
    pushq %rax
    popq %rax
    cmpq $0, %rax
    jnz .L20
.L22:
    popq %rax
    movq %rbp, %rsp
    popq %rbp
    .cfi_def_cfa 7, 8
    ret
    .cfi_endproc
.LFE6:
    .size	conv, .-conv
    .globl	main
    .type	main, @function
main:
.LFB7:
    .cfi_startproc
    pushq	%rbp
    .cfi_def_cfa_offset 16
    .cfi_offset 6, -16
    movq	%rsp, %rbp
    .cfi_def_cfa_register 6
    subq	$16, %rsp
    pushq $0
    popq %rdi
    callq print
    pushq %rax
    pushq $0
    popq %rdi
    callq print
    pushq %rax
    pushq $0
    popq %rdi
    callq print
    pushq %rax
    pushq $0
    popq %rdi
    callq print
    pushq %rax
    pushq $5
    popq %rdi
    callq print
    pushq %rax
    pushq $4
    popq %rdi
    callq print
    pushq %rax
    pushq $3
    popq %rdi
    callq print
    pushq %rax
    pushq $2
    popq %rdi
    callq print
    pushq %rax
    pushq $1
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
.LFE7:
    .size	main, .-main
    .ident	"GCC: (GNU) 6.2.1 20160830"
    .section	.note.GNU-stack,"",@progbits
