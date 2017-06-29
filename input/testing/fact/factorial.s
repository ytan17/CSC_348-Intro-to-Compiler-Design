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
// while (((Factorial (i)) < 100))
	jmp	lab002
lab001:
// Put_Line ((Factorial (i)));
	pushq	(i)
	call	Factorial
	addq	$8, %rsp
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
	pushq	(i)
	call	Factorial
	addq	$8, %rsp
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
	.globl	Factorial
	.type	Factorial, @function
Factorial:
	pushq	%rbp
	movq	%rsp, %rbp
// (N > 1)
	pushq	16(%rbp)
	pushq	$1
	popq	%rbx
	popq	%rax
	cmpq	%rax, %rbx
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab003
	jmp	lab004
lab003:
// return ((N * (Factorial ((N - 1)))));
	pushq	16(%rbp)
	pushq	16(%rbp)
	pushq	$1
	popq	%rbx
	popq	%rax
	subq	%rbx, %rax
	pushq	%rax
	call	Factorial
	addq	$8, %rsp
	pushq	%rax
	popq	%rbx
	popq	%rax
	imulq	%rbx, %rax
	pushq	%rax
	popq	%rax
	popq	%rbp
	ret
	jmp	lab005
lab004:
// return (1);
	pushq	$1
	popq	%rax
	popq	%rbp
	ret
lab005:
	popq	%rbp
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

	.globl	N
	.data
	.align	8
	.size	N, 8
N:
	.quad	0

