	.file	"fib.c"
	.text
.globl _fib
#	.def	_fib;	.scl	2;	.type	32;	.endef
_fib:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx
	subl	$20, %esp
        movl    $1234, %eax
	cmpl	$1, 8(%ebp)
	jg	L3
        je      L2
        jne     L2
        jl      LL3
        cmp     $2, %al
	movl	%eax,8(%esp)
	movl	8(%esp),%eax
	movl	%ebp,%eax
	movl	%eax,8(%ebp)
	movl	8(%esi),%eax
	movl	$8, (%ebp)
	cmpl	$34, %eax
	shll	$3, %eax
	orl	$3, %eax
	or	$3, %al
	xorl	$3, %eax
	andl	$3, %eax
	and	$3, %al
	addl	$4,%ebp
	addl	$1, %eax
	addl	4(%esp), %eax
	subl	$3, %eax
	subl	4(%esp), %eax
	sal	$3, %al
	sall	$3, %eax
	setl	%al
	cmp	%eax, 4(%esp)
LL3:
	addl    $7, %eax
        shll    $7, %eax
        orl     $7, %eax
        and     $8, %al
        sete    %al
        movzbl  %al, %eax
        sal     $7, %al
        subl    $7, %eax
        movl    4(%eax), %eax
        movl    %eax, 8(%esp)
	movl	8(%ebp), %eax
	jmp	L3
L2:
	movl	8(%ebp), %eax
	subl	$1, %eax
	movl	%eax, (%esp)
	call	_fib
	movl	%eax, %ebx
	movl	8(%ebp), %eax
	subl	$2, %eax
	movl	%eax, (%esp)
	call	_fib
	leal	(%ebx,%eax), %eax
L3:
	addl	$20, %esp
	popl	%ebx
	popl	%ebp
	ret
#	.def	___main;	.scl	2;	.type	32;	.endef
.globl _main
#	.def	_main;	.scl	2;	.type	32;	.endef
_main:
	pushl	%ebp
	movl	% esp, %ebp
	andl	$-16, %esp
	subl	$16, %esp
	call	___main
	movl	$36, (%esp)
	call	_fib
	leave
	ret
