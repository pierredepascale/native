	.text
.globl scheme_entry
	.type scheme_entry, @function

scheme_entry:
	movl 4(%esp), %ecx
	movl %ebx, 4(%ecx)
	movl %esi, 16(%ecx)
	movl %edi, 20(%ecx)
	movl %ebp, 24(%ecx)
	movl %esp, 28(%ecx)
	movl 12(%esp), %ebp
	movl 8(%esp), %esp
	call L_scheme_entry
	movl 4(%ecx), %ebx
	movl 16(%ecx), %esi
	movl 20(%ecx), %edi
	movl 24(%ecx), %ebp
	movl 28(%ecx), %esp
	ret
	