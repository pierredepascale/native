    .text
    .globl L_scheme_entry
    .type L_scheme_entry, @function
L_scheme_entry:
    movl $4, %eax
    movl %eax, -4(%esp)
    movl $8, %eax
    movl %eax, 4(%ebp)
    movl -4(%esp), %eax
    movl %eax, (%ebp)
    movl %ebp, %eax
    addl $1, %eax
    addl $8, %ebp
    movl %eax, -4(%esp)
    movl -4(%esp), %eax
    movl %eax, -8(%esp)
    jmp L_1
L_0:
    cmpl $2, %edx
    je L_2
L_2:
    movl -8(%esp), %eax
    movl %eax, -12(%esp)
    movl $8, %eax
    cmp %eax, -12(%esp)
    setl %al
    movzbl %al, %eax
    sal $6, %al
    or $47, %al
    cmp $47, %al
    je L_3
    movl -8(%esp), %eax
    jmp L_4
L_3:
    movl -4(%esp), %eax
    movl %eax, -16(%esp)
    movl $4, %eax
    movl %eax, -20(%esp)
    movl -8(%esp), %eax
    subl -20(%esp), %eax
    movl %eax, -20(%esp)
    movl -4(%esp), %esi
    movl -1(%esi), %esi
    movl $8, %edx
    addl $-8, %esp
    movl 2(%esi), %eax
    call *%eax
    addl $8, %esp
    movl %eax, -12(%esp)
    movl -4(%esp), %eax
    movl %eax, -20(%esp)
    movl $8, %eax
    movl %eax, -24(%esp)
    movl -8(%esp), %eax
    subl -24(%esp), %eax
    movl %eax, -24(%esp)
    movl -4(%esp), %esi
    movl -1(%esi), %esi
    movl $8, %edx
    addl $-12, %esp
    movl 2(%esi), %eax
    call *%eax
    addl $12, %esp
    addl -12(%esp), %eax
L_4:
    ret
L_1:
    movl $8, (%ebp)
    movl $L_0, 4(%ebp)
    movl %ebp, %eax
    addl $8, %ebp
    addl $2, %eax
    movl -8(%esp), %ebx
    movl %eax, -1(%ebx)
    movl %eax, -8(%esp)
    movl -4(%esp), %eax
    movl %eax, -16(%esp)
    movl $144, %eax
    movl %eax, -20(%esp)
    movl -4(%esp), %esi
    movl -1(%esi), %esi
    movl $8, %edx
    addl $-8, %esp
    movl 2(%esi), %eax
    call *%eax
    addl $8, %esp
    ret
