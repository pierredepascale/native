        .file "entry_x86.s"
	.text
.globl scheme_entry

scheme_entry:
	movl 4(%esp), %ecx
	movl %ebx, 4(%ecx)
	movl %esi, 16(%ecx)
	movl %edi, 20(%ecx)
	movl %ebp, 24(%ecx)
	movl %esp, 28(%ecx)
	movl 16(%esp), %ebp
        movl 8(%esp), %esi
	movl 12(%esp), %esp
        movl 4(%esi), %eax
	call *%eax
	movl 4(%ecx), %ebx
	movl 16(%ecx), %esi
	movl 20(%ecx), %edi
	movl 24(%ecx), %ebp
	movl 28(%ecx), %esp
	ret
