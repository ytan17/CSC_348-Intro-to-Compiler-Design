	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// a := (newarray (10));
	pushq	$10
	popq	%rdi
	imulq	$8, %rdi
	call	malloc
	pushq	%rax
	popq	%rax
	movq	%rax, (a)
// array := (write (a, 0, 72));
	pushq	(a)
	pushq	$0
	pushq	$72
	popq	%rcx
	popq	%rbx
	popq	%rax
	movq	%rcx, (%rax,%rbx,8)
	pushq	$0
	popq	%rax
	movq	%rax, (array)
// array := (write (a, 1, 69));
	pushq	(a)
	pushq	$1
	pushq	$69
	popq	%rcx
	popq	%rbx
	popq	%rax
	movq	%rcx, (%rax,%rbx,8)
	pushq	$0
	popq	%rax
	movq	%rax, (array)
// array := (write (a, 2, 76));
	pushq	(a)
	pushq	$2
	pushq	$76
	popq	%rcx
	popq	%rbx
	popq	%rax
	movq	%rcx, (%rax,%rbx,8)
	pushq	$0
	popq	%rax
	movq	%rax, (array)
// array := (write (a, 3, 76));
	pushq	(a)
	pushq	$3
	pushq	$76
	popq	%rcx
	popq	%rbx
	popq	%rax
	movq	%rcx, (%rax,%rbx,8)
	pushq	$0
	popq	%rax
	movq	%rax, (array)
// array := (write (a, 4, 79));
	pushq	(a)
	pushq	$4
	pushq	$79
	popq	%rcx
	popq	%rbx
	popq	%rax
	movq	%rcx, (%rax,%rbx,8)
	pushq	$0
	popq	%rax
	movq	%rax, (array)
// array := (write (a, 5, 79));
	pushq	(a)
	pushq	$5
	pushq	$79
	popq	%rcx
	popq	%rbx
	popq	%rax
	movq	%rcx, (%rax,%rbx,8)
	pushq	$0
	popq	%rax
	movq	%rax, (array)
// array := (write (a, 6, 10));
	pushq	(a)
	pushq	$6
	pushq	$10
	popq	%rcx
	popq	%rbx
	popq	%rax
	movq	%rcx, (%rax,%rbx,8)
	pushq	$0
	popq	%rax
	movq	%rax, (array)
// array := (write (a, 7, 0));
	pushq	(a)
	pushq	$7
	pushq	$0
	popq	%rcx
	popq	%rbx
	popq	%rax
	movq	%rcx, (%rax,%rbx,8)
	pushq	$0
	popq	%rax
	movq	%rax, (array)
// Put_LineString (a);
	pushq	(a)
	popq	%rdi
	call	print_string
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

	.globl	a
	.data
	.align	8
	.size	a, 8
a:
	.quad	0

	.globl	array
	.data
	.align	8
	.size	array, 8
array:
	.quad	0

