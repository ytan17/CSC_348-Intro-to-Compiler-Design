	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// i := 2;
	pushq	$2
	popq	%rax
	movq	%rax, (i)
// while ((i < 100))
	jmp	lab002
lab001:
// Put_Line (i);
	pushq	(i)
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


	.section .rodata
.output:
	.string "%d\n"

	.globl	i
	.data
	.align	8
	.size	i, 8
i:
	.quad	0

