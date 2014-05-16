	.file	"rt.c"
	.text
	.globl	make_header
	.type	make_header, @function
make_header:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-4(%rbp), %eax
	sall	$8, %eax
	orl	-8(%rbp), %eax
	movl	%eax, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	make_header, .-make_header
	.globl	header_code
	.type	header_code, @function
header_code:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movzbl	%al, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	header_code, .-header_code
	.globl	closurep
	.type	closurep, @function
closurep:
.LFB2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movl	$0, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	closurep, .-closurep
	.globl	make_closure
	.type	make_closure, @function
make_closure:
.LFB3:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	addq	$1, %rax
	salq	$3, %rax
	movq	%rax, %rdi
	call	alloc
	movq	%rax, -8(%rbp)
	movq	-24(%rbp), %rax
	movl	$1, %esi
	movl	%eax, %edi
	call	make_header
	movq	-8(%rbp), %rdx
	movq	%rax, (%rdx)
	movq	-8(%rbp), %rax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE3:
	.size	make_closure, .-make_closure
	.globl	templatep
	.type	templatep, @function
templatep:
.LFB4:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movl	$0, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE4:
	.size	templatep, .-templatep
	.globl	make_template
	.type	make_template, @function
make_template:
.LFB5:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	addq	$16, %rax
	movq	%rax, %rdi
	call	alloc
	movq	%rax, -8(%rbp)
	movq	-24(%rbp), %rax
	movl	$0, %esi
	movl	%eax, %edi
	call	make_header
	movq	-8(%rbp), %rdx
	movq	%rax, (%rdx)
	movq	-8(%rbp), %rax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE5:
	.size	make_template, .-make_template
	.globl	symbolp
	.type	symbolp, @function
symbolp:
.LFB6:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movl	$0, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	symbolp, .-symbolp
	.globl	make_symbol
	.type	make_symbol, @function
make_symbol:
.LFB7:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	addq	$16, %rax
	movq	%rax, %rdi
	call	alloc
	movq	%rax, -8(%rbp)
	movl	-20(%rbp), %eax
	movl	$2, %esi
	movl	%eax, %edi
	call	make_header
	movq	-8(%rbp), %rdx
	movq	%rax, (%rdx)
	movq	-8(%rbp), %rax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE7:
	.size	make_symbol, .-make_symbol
	.globl	refp
	.type	refp, @function
refp:
.LFB8:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movl	$0, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE8:
	.size	refp, .-refp
	.globl	make_ref
	.type	make_ref, @function
make_ref:
.LFB9:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movl	$16, %edi
	call	alloc
	movq	%rax, -8(%rbp)
	movl	$3, %esi
	movl	$16, %edi
	call	make_header
	movq	-8(%rbp), %rdx
	movq	%rax, (%rdx)
	movq	-8(%rbp), %rax
	movq	-24(%rbp), %rdx
	movq	%rdx, 8(%rax)
	movq	-8(%rbp), %rax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE9:
	.size	make_ref, .-make_ref
	.globl	tag
	.type	tag, @function
tag:
.LFB10:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	salq	$2, %rax
	addq	$1, %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE10:
	.size	tag, .-tag
	.globl	untag
	.type	untag, @function
untag:
.LFB11:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	subq	$1, %rax
	sarq	$2, %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE11:
	.size	untag, .-untag
	.local	heap_new
	.comm	heap_new,8,8
	.local	heap_free
	.comm	heap_free,8,8
	.local	heap_limit
	.comm	heap_limit,8,8
	.local	heap_old
	.comm	heap_old,8,8
	.section	.rodata
.LC0:
	.string	"could not allocate memory"
	.align 8
.LC1:
	.string	"could not protect begin of allocated memory"
	.text
	.type	alloc_protected_space, @function
alloc_protected_space:
.LFB12:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movl	%edi, -36(%rbp)
	movl	$0, %eax
	call	getpagesize
	movl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	movl	-36(%rbp), %edx
	addl	%edx, %eax
	subl	$1, %eax
	cltd
	idivl	-4(%rbp)
	imull	-4(%rbp), %eax
	movl	%eax, -8(%rbp)
	movl	-4(%rbp), %eax
	leal	(%rax,%rax), %edx
	movl	-8(%rbp), %eax
	addl	%edx, %eax
	cltq
	movl	$0, %r9d
	movl	$0, %r8d
	movl	$2, %ecx
	movl	$7, %edx
	movq	%rax, %rsi
	movl	$0, %edi
	call	mmap
	movq	%rax, -16(%rbp)
	cmpq	$-1, -16(%rbp)
	jne	.L26
	movl	$.LC0, %edi
	movl	$0, %eax
	call	printf
	movl	$1, %edi
	call	_exit
.L26:
	movl	-4(%rbp), %eax
	movslq	%eax, %rcx
	movq	-16(%rbp), %rax
	movl	$0, %edx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	mprotect
	movl	%eax, -20(%rbp)
	cmpl	$0, -20(%rbp)
	je	.L27
	movl	$.LC1, %edi
	movl	$0, %eax
	call	printf
	movl	$1, %edi
	call	_exit
.L27:
	movl	-4(%rbp), %eax
	cltq
	movl	-4(%rbp), %edx
	movslq	%edx, %rcx
	movl	-8(%rbp), %edx
	movslq	%edx, %rdx
	addq	%rdx, %rcx
	movq	-16(%rbp), %rdx
	addq	%rdx, %rcx
	movl	$0, %edx
	movq	%rax, %rsi
	movq	%rcx, %rdi
	call	mprotect
	movl	%eax, -20(%rbp)
	cmpl	$0, -20(%rbp)
	je	.L28
	movl	$.LC1, %edi
	movl	$0, %eax
	call	printf
	movl	$1, %edi
	call	_exit
.L28:
	movl	-4(%rbp), %eax
	movslq	%eax, %rdx
	movq	-16(%rbp), %rax
	addq	%rdx, %rax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE12:
	.size	alloc_protected_space, .-alloc_protected_space
	.section	.rodata
.LC2:
	.string	"HEAP ALLOCATION ERROR FOR %d"
	.text
	.type	alloc, @function
alloc:
.LFB13:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	heap_free(%rip), %rdx
	movq	-24(%rbp), %rax
	addq	%rax, %rdx
	movq	heap_limit(%rip), %rax
	cmpq	%rax, %rdx
	jae	.L31
	movq	heap_free(%rip), %rax
	movq	%rax, -8(%rbp)
	movq	heap_free(%rip), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	movq	%rax, heap_free(%rip)
	movq	-8(%rbp), %rax
	jmp	.L33
.L31:
	movq	-24(%rbp), %rax
	movq	%rax, %rsi
	movl	$.LC2, %edi
	movl	$0, %eax
	call	printf
	movl	$1, %edi
	call	_exit
.L33:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE13:
	.size	alloc, .-alloc
	.section	.rodata
.LC3:
	.string	"#("
	.text
	.type	print_scm_vector, @function
print_scm_vector:
.LFB14:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	$.LC3, %edi
	movl	$0, %eax
	call	printf
	movl	-20(%rbp), %eax
	subl	$5, %eax
	movl	%eax, %eax
	movl	(%rax), %eax
	movl	%eax, -8(%rbp)
	movl	$1, -4(%rbp)
	jmp	.L35
.L36:
	movl	-4(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movl	-20(%rbp), %eax
	subl	$5, %eax
	movl	%eax, %eax
	addq	%rdx, %rax
	movl	(%rax), %eax
	movl	%eax, %edi
	call	print_scm_obj
	addl	$1, -4(%rbp)
.L35:
	movl	-4(%rbp), %eax
	cmpl	-8(%rbp), %eax
	jle	.L36
	movl	$41, %edi
	call	putchar
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE14:
	.size	print_scm_vector, .-print_scm_vector
	.type	print_scm_pair, @function
print_scm_pair:
.LFB15:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	$40, %edi
	call	putchar
	movl	-20(%rbp), %eax
	movq	%rax, -8(%rbp)
	jmp	.L38
.L41:
	movq	-8(%rbp), %rax
	andl	$7, %eax
	cmpl	$1, %eax
	jne	.L39
	movq	-8(%rbp), %rax
	subl	$1, %eax
	movl	%eax, %eax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movl	(%rax), %eax
	movl	%eax, %edi
	call	print_scm_obj
	movl	$32, %edi
	call	putchar
	movq	-16(%rbp), %rax
	addq	$4, %rax
	movl	(%rax), %eax
	movl	%eax, %eax
	movq	%rax, -8(%rbp)
	jmp	.L38
.L39:
	movq	-8(%rbp), %rax
	movl	%eax, %edi
	call	print_scm_obj
	jmp	.L40
.L38:
	cmpq	$18, -8(%rbp)
	jne	.L41
.L40:
	movl	$41, %edi
	call	putchar
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE15:
	.size	print_scm_pair, .-print_scm_pair
	.section	.rodata
.LC4:
	.string	"%d"
.LC5:
	.string	"#{closure 0x%08x"
.LC6:
	.string	"%s"
.LC7:
	.string	"\"%s\""
.LC8:
	.string	"#t"
.LC9:
	.string	"#f"
.LC10:
	.string	"#!eof"
.LC11:
	.string	"#!unspecific"
.LC12:
	.string	"#!unbound"
.LC13:
	.string	"()"
.LC14:
	.string	"#{??? 0x%08x}"
	.text
	.type	print_scm_obj, @function
print_scm_obj:
.LFB16:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	andl	$3, %eax
	testl	%eax, %eax
	jne	.L43
	movl	-4(%rbp), %eax
	sarl	$2, %eax
	movl	%eax, %esi
	movl	$.LC4, %edi
	movl	$0, %eax
	call	printf
	jmp	.L42
.L43:
	movl	-4(%rbp), %eax
	andl	$7, %eax
	cmpl	$1, %eax
	jne	.L45
	movl	-4(%rbp), %eax
	movl	%eax, %edi
	call	print_scm_pair
	jmp	.L42
.L45:
	movl	-4(%rbp), %eax
	andl	$7, %eax
	cmpl	$2, %eax
	jne	.L46
	movl	-4(%rbp), %eax
	movl	%eax, %esi
	movl	$.LC5, %edi
	movl	$0, %eax
	call	printf
	jmp	.L42
.L46:
	movl	-4(%rbp), %eax
	andl	$7, %eax
	cmpl	$3, %eax
	jne	.L47
	movl	-4(%rbp), %eax
	addl	$1, %eax
	movl	%eax, %eax
	movq	%rax, %rsi
	movl	$.LC6, %edi
	movl	$0, %eax
	call	printf
	jmp	.L42
.L47:
	movl	-4(%rbp), %eax
	andl	$7, %eax
	cmpl	$5, %eax
	jne	.L48
	movl	-4(%rbp), %eax
	movl	%eax, %edi
	call	print_scm_vector
	jmp	.L42
.L48:
	movl	-4(%rbp), %eax
	andl	$7, %eax
	cmpl	$6, %eax
	jne	.L49
	movl	-4(%rbp), %eax
	subl	$2, %eax
	movl	%eax, %eax
	movq	%rax, %rsi
	movl	$.LC7, %edi
	movl	$0, %eax
	call	printf
	jmp	.L42
.L49:
	cmpl	$2, -4(%rbp)
	jne	.L50
	movl	$.LC8, %edi
	movl	$0, %eax
	call	printf
	jmp	.L42
.L50:
	cmpl	$6, -4(%rbp)
	jne	.L51
	movl	$.LC9, %edi
	movl	$0, %eax
	call	printf
	jmp	.L42
.L51:
	cmpl	$22, -4(%rbp)
	jne	.L52
	movl	$.LC10, %edi
	movl	$0, %eax
	call	printf
	jmp	.L42
.L52:
	cmpl	$10, -4(%rbp)
	jne	.L53
	movl	$.LC11, %edi
	movl	$0, %eax
	call	printf
	jmp	.L42
.L53:
	cmpl	$14, -4(%rbp)
	jne	.L54
	movl	$.LC12, %edi
	movl	$0, %eax
	call	printf
	jmp	.L42
.L54:
	cmpl	$18, -4(%rbp)
	jne	.L55
	movl	$.LC13, %edi
	movl	$0, %eax
	call	printf
	jmp	.L42
.L55:
	cmpl	$22, -4(%rbp)
	jne	.L56
	movl	$.LC10, %edi
	movl	$0, %eax
	call	printf
	jmp	.L42
.L56:
	movl	-4(%rbp), %eax
	movl	%eax, %esi
	movl	$.LC14, %edi
	movl	$0, %eax
	call	printf
.L42:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE16:
	.size	print_scm_obj, .-print_scm_obj
	.globl	main
	.type	main, @function
main:
.LFB17:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$136, %rsp
	.cfi_offset 3, -24
	movl	%edi, -132(%rbp)
	movq	%rsi, -144(%rbp)
	movl	$65536, -20(%rbp)
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	alloc_protected_space
	movq	%rax, -32(%rbp)
	movl	-20(%rbp), %eax
	movslq	%eax, %rdx
	movq	-32(%rbp), %rax
	addq	%rdx, %rax
	movq	%rax, -40(%rbp)
	movl	$65536, %edi
	call	alloc_protected_space
	movq	%rax, heap_free(%rip)
	movq	heap_free(%rip), %rax
	movq	%rax, heap_new(%rip)
	movq	heap_free(%rip), %rax
	addq	$65536, %rax
	movq	%rax, heap_limit(%rip)
	movl	$65536, %edi
	call	alloc_protected_space
	movq	%rax, heap_old(%rip)
	movq	-144(%rbp), %rax
	addq	$8, %rax
	movq	(%rax), %rax
	movl	$0, %esi
	movq	%rax, %rdi
	movl	$0, %eax
	call	open
	movl	%eax, -44(%rbp)
	movl	-44(%rbp), %eax
	movl	%eax, %edi
	call	read_fasl
	movq	%rax, -56(%rbp)
	movq	heap_free(%rip), %rbx
	movq	-56(%rbp), %rax
	movq	%rax, %rdi
	call	untag
	movq	%rax, %rsi
	movq	-32(%rbp), %rdx
	leaq	-128(%rbp), %rax
	movq	%rbx, %rcx
	movq	%rax, %rdi
	call	scheme_entry
	movl	%eax, %eax
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rax
	movl	%eax, %edi
	call	print_scm_obj
	movl	$0, %eax
	addq	$136, %rsp
	popq	%rbx
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE17:
	.size	main, .-main
	.globl	read_u8
	.type	read_u8, @function
read_u8:
.LFB18:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	leaq	-1(%rbp), %rcx
	movl	-20(%rbp), %eax
	movl	$1, %edx
	movq	%rcx, %rsi
	movl	%eax, %edi
	call	read
	cmpq	$1, %rax
	jne	.L60
	movzbl	-1(%rbp), %eax
	jmp	.L62
.L60:
	movl	$1, %edi
	call	_exit
.L62:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE18:
	.size	read_u8, .-read_u8
	.globl	read_u32
	.type	read_u32, @function
read_u32:
.LFB19:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	read_u8
	movb	%al, -1(%rbp)
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	read_u8
	movb	%al, -2(%rbp)
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	read_u8
	movb	%al, -3(%rbp)
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	read_u8
	movb	%al, -4(%rbp)
	movzbl	-4(%rbp), %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE19:
	.size	read_u32, .-read_u32
	.globl	read_fasl
	.type	read_fasl, @function
read_fasl:
.LFB20:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	read_u8
	movb	%al, -1(%rbp)
	movzbl	-1(%rbp), %eax
	cmpl	$8, %eax
	ja	.L66
	movl	%eax, %eax
	movq	.L68(,%rax,8), %rax
	jmp	*%rax
	.section	.rodata
	.align 8
	.align 4
.L68:
	.quad	.L67
	.quad	.L69
	.quad	.L70
	.quad	.L71
	.quad	.L72
	.quad	.L73
	.quad	.L74
	.quad	.L75
	.quad	.L76
	.text
.L67:
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	read_template
	jmp	.L77
.L69:
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	read_ref
	jmp	.L77
.L70:
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	read_number
	jmp	.L77
.L71:
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	read_closure
	jmp	.L77
.L72:
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	read_symbol
	jmp	.L77
.L73:
	movl	$2, %eax
	jmp	.L77
.L74:
	movl	$6, %eax
	jmp	.L77
.L75:
	movl	$14, %eax
	jmp	.L77
.L76:
	movl	$10, %eax
	jmp	.L77
.L66:
	movl	$0, %eax
.L77:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE20:
	.size	read_fasl, .-read_fasl
	.section	.rodata
.LC15:
	.string	"code @ %08x\n"
.LC16:
	.string	"%02x "
	.text
	.globl	read_template
	.type	read_template, @function
read_template:
.LFB21:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movl	%edi, -36(%rbp)
	movl	-36(%rbp), %eax
	movl	%eax, %edi
	call	read_u32
	movl	%eax, -8(%rbp)
	movl	-8(%rbp), %eax
	movq	%rax, %rdi
	call	make_template
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	$6, 8(%rax)
	movq	-16(%rbp), %rax
	addq	$16, %rax
	movq	%rax, %rsi
	movl	$.LC15, %edi
	movl	$0, %eax
	call	printf
	movl	$0, -4(%rbp)
	jmp	.L79
.L80:
	movl	-36(%rbp), %eax
	movl	%eax, %edi
	call	read_u8
	movb	%al, -17(%rbp)
	movzbl	-17(%rbp), %eax
	movl	%eax, %esi
	movl	$.LC16, %edi
	movl	$0, %eax
	call	printf
	movzbl	-17(%rbp), %ecx
	movq	-16(%rbp), %rdx
	movl	-4(%rbp), %eax
	cltq
	movb	%cl, 16(%rdx,%rax)
	addl	$1, -4(%rbp)
.L79:
	movl	-4(%rbp), %eax
	cmpl	-8(%rbp), %eax
	jb	.L80
	movq	-16(%rbp), %rax
	movq	%rax, %rdi
	call	tag
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE21:
	.size	read_template, .-read_template
	.globl	read_ref
	.type	read_ref, @function
read_ref:
.LFB22:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	$10, %edi
	call	make_ref
	movq	%rax, %rdi
	call	tag
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE22:
	.size	read_ref, .-read_ref
	.globl	read_number
	.type	read_number, @function
read_number:
.LFB23:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	read_u8
	addl	$-128, %eax
	movb	%al, -1(%rbp)
	movsbl	-1(%rbp), %eax
	sall	$2, %eax
	cltq
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE23:
	.size	read_number, .-read_number
	.globl	read_closure
	.type	read_closure, @function
read_closure:
.LFB24:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movl	%edi, -36(%rbp)
	movl	-36(%rbp), %eax
	movl	%eax, %edi
	call	read_u32
	movl	%eax, -8(%rbp)
	movl	-8(%rbp), %eax
	movq	%rax, %rdi
	call	make_closure
	movq	%rax, -16(%rbp)
	movl	-36(%rbp), %eax
	movl	%eax, %edi
	call	read_fasl
	movq	%rax, %rdi
	call	untag
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	addq	$16, %rax
	movq	%rax, %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, 8(%rax)
	movl	$1, -4(%rbp)
	jmp	.L87
.L88:
	movl	-36(%rbp), %eax
	movl	%eax, %edi
	call	read_fasl
	movq	-16(%rbp), %rdx
	movl	-4(%rbp), %ecx
	movslq	%ecx, %rcx
	movq	%rax, 8(%rdx,%rcx,8)
	addl	$1, -4(%rbp)
.L87:
	movl	-4(%rbp), %eax
	cmpl	-8(%rbp), %eax
	jb	.L88
	movq	-16(%rbp), %rax
	movq	%rax, %rdi
	call	tag
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE24:
	.size	read_closure, .-read_closure
	.globl	read_symbol
	.type	read_symbol, @function
read_symbol:
.LFB25:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movl	%edi, -20(%rbp)
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	read_u32
	movb	%al, -5(%rbp)
	movzbl	-5(%rbp), %eax
	movl	%eax, %edi
	call	make_symbol
	movq	%rax, -16(%rbp)
	movl	$0, -4(%rbp)
	jmp	.L91
.L92:
	movl	-20(%rbp), %eax
	movl	%eax, %edi
	call	read_u8
	movl	%eax, %ecx
	movq	-16(%rbp), %rdx
	movl	-4(%rbp), %eax
	cltq
	movb	%cl, 12(%rdx,%rax)
	addl	$1, -4(%rbp)
.L91:
	movzbl	-5(%rbp), %eax
	cmpl	-4(%rbp), %eax
	jg	.L92
	movq	-16(%rbp), %rax
	movq	%rax, %rdi
	call	tag
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE25:
	.size	read_symbol, .-read_symbol
	.ident	"GCC: (GNU) 4.8.0"
	.section	.note.GNU-stack,"",@progbits
