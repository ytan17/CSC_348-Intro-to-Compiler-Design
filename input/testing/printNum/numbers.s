	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// String := (Test ((0 - 2), (0 - 1), 0, 1, 2, 3));
	pushq	$3
	pushq	$2
	pushq	$1
	pushq	$0
	pushq	$0
	pushq	$1
	popq	%rbx
	popq	%rax
	subq	%rbx, %rax
	pushq	%rax
	pushq	$0
	pushq	$2
	popq	%rbx
	popq	%rax
	subq	%rbx, %rax
	pushq	%rax
	call	Test
	addq	$48, %rsp
	pushq	%rax
	popq	%rax
	movq	%rax, (String)
	popq	%rbp
	ret

	.text
	.globl	Test
	.type	Test, @function
Test:
	pushq	%rbp
	movq	%rsp, %rbp
// Put_Line (a);
	pushq	16(%rbp)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// Put_Line (b);
	pushq	24(%rbp)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// Put_Line (c);
	pushq	32(%rbp)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// Put_Line (d);
	pushq	40(%rbp)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// Put_Line (e);
	pushq	48(%rbp)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// Put_Line (f);
	pushq	56(%rbp)
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
	popq	%rbp
	ret

	.text
	.globl	print_string
	.type	print_string, @function
print_string:
.LFB0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	jmp	.L2
.L3:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movsbl	%al, %eax
	movl	%eax, %edi
	call	putchar
	addq	$8, -8(%rbp)
.L2:
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.L3
	leave
	ret

	.section .rodata
.output:
	.string "%d\n"

	.globl	e
	.data
	.align	8
	.size	e, 8
e:
	.quad	0

	.globl	f
	.data
	.align	8
	.size	f, 8
f:
	.quad	0

	.globl	a
	.data
	.align	8
	.size	a, 8
a:
	.quad	0

	.globl	b
	.data
	.align	8
	.size	b, 8
b:
	.quad	0

	.globl	c
	.data
	.align	8
	.size	c, 8
c:
	.quad	0

	.globl	String
	.data
	.align	8
	.size	String, 8
String:
	.quad	0

	.globl	d
	.data
	.align	8
	.size	d, 8
d:
	.quad	0

