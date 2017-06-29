	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// i := 0;
	pushq	$0
	popq	%rax
	movq	%rax, (i)
// n := (3 * 5);
	pushq	$3
	pushq	$5
	popq	%rbx
	popq	%rax
	imulq	%rbx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (n)
// while (((n * i) < 100))
	jmp	lab002
lab001:
// Put_Line ((n * i));
	pushq	(n)
	pushq	(i)
	popq	%rbx
	popq	%rax
	imulq	%rbx, %rax
	pushq	%rax
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// i := (i + 1);
	pushq	(i)
	pushq	$1
	popq	%rbx
	popq	%rax
	addq	%rbx, %rax
	pushq	%rax
	popq	%rax
	movq	%rax, (i)
lab002:
	pushq	(n)
	pushq	(i)
	popq	%rbx
	popq	%rax
	imulq	%rbx, %rax
	pushq	%rax
	pushq	$100
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab001
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

	.globl	i
	.data
	.align	8
	.size	i, 8
i:
	.quad	0

	.globl	n
	.data
	.align	8
	.size	n, 8
n:
	.quad	0

