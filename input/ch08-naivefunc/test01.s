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
// while ((i < 10))
	jmp	lab002
lab001:
// print ((fact (i)));
	pushq	(i)
	call	fact
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
	pushq	$10
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
	.globl	factaux
	.type	factaux, @function
factaux:
	pushq	%rbp
	movq	%rsp, %rbp
// (n < 1)
	pushq	16(%rbp)
	pushq	$1
	popq	%rbx
	popq	%rax
	cmpq	%rbx, %rax
	sets	%al
	movzbl	%al, %eax
	pushq	%rax
	popq	%rax
	testq	%rax, %rax
	jne	lab003
	jmp	lab004
lab003:
// return (result);
	pushq	24(%rbp)
	popq	%rax
	popq	%rbp
	ret
	jmp	lab005
lab004:
// return ((factaux ((n - 1), (n * result))));
	pushq	16(%rbp)
	pushq	24(%rbp)
	popq	%rbx
	popq	%rax
	imulq	%rbx, %rax
	pushq	%rax
	pushq	16(%rbp)
	pushq	$1
	popq	%rbx
	popq	%rax
	subq	%rbx, %rax
	pushq	%rax
	call	factaux
	addq	$16, %rsp
	pushq	%rax
	popq	%rax
	popq	%rbp
	ret
lab005:
	popq	%rbp
	ret

	.text
	.globl	fact
	.type	fact, @function
fact:
	pushq	%rbp
	movq	%rsp, %rbp
// return ((factaux (n, 1)));
	pushq	$1
	pushq	16(%rbp)
	call	factaux
	addq	$16, %rsp
	pushq	%rax
	popq	%rax
	popq	%rbp
	ret
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

	.globl	n
	.data
	.align	8
	.size	n, 8
n:
	.quad	0

	.globl	result
	.data
	.align	8
	.size	result, 8
result:
	.quad	0

