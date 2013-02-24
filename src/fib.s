	.file	"fib.c"
	.text
.globl fib
	.type	fib, @function
fib:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$20, %esp
	cmpl	$1, 8(%ebp)
	jg	.L2
	movl	8(%ebp), %eax
	jmp	.L3
.L2:
	movl	8(%ebp), %eax
	subl	$1, %eax
	movl	%eax, (%esp)
	call	fib
	movl	%eax, %ebx
	movl	8(%ebp), %eax
	subl	$2, %eax
	movl	%eax, (%esp)
	call	fib
	leal	(%ebx,%eax), %eax
.L3:
	addl	$20, %esp
	popl	%ebx
	popl	%ebp
	ret
	.size	fib, .-fib
.globl tt
	.type	tt, @function
tt:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$40, %esp
	movl	$fib, -12(%ebp)
	movl	$10, (%esp)
	movl	-12(%ebp), %eax
	call	*%eax
	leave
	ret
	.size	tt, .-tt
.globl main
	.type	main, @function
main:
	pushl	%ebp
	movl	%esp, %ebp
	andl	$-16, %esp
	subl	$16, %esp
	movl	$34, (%esp)
	call	fib
	leave
	ret
	.size	main, .-main
	.ident	"GCC: (Ubuntu/Linaro 4.4.4-14ubuntu5) 4.4.5"
	.section	.note.GNU-stack,"",@progbits
