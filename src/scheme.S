    .text
    .globl L_0
    .type L_0, @function
L_0:
    movl -4(%esp), %eax
    movl %eax, -8(%esp)
    movl $8, %eax
    cmp %eax, -8(%esp)
    setl %al
    movzbl %al, %eax
    sal $6, %al
    or $47, %al
    cmp $47, %al
    je L_1
    movl -4(%esp), %eax
    jmp L_2
L_1:
    movl $4, %eax
    movl %eax, -12(%esp)
    movl -4(%esp), %eax
    subl -12(%esp), %eax
    movl %eax, -12(%esp)
    addl $-4, %esp
    call L_0
    addl $4, %esp
    movl %eax, -8(%esp)
    movl $8, %eax
    movl %eax, -16(%esp)
    movl -4(%esp), %eax
    subl -16(%esp), %eax
    movl %eax, -16(%esp)
    addl $-8, %esp
    call L_0
    addl $8, %esp
    addl -8(%esp), %eax
L_2:
    ret
    .globl L_scheme_entry
    .type L_scheme_entry, @function
L_scheme_entry:
    movl $136, %eax
    movl %eax, -8(%esp)
;   addl $0,%esp
    call L_0
;   addl $0,%esp
    ret
