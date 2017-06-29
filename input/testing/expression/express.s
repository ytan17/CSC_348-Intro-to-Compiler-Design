	.text
	.globl	main
	.type	main, @function
main:
	pushq	%rbp
	movq	%rsp, %rbp
// Put_Line ((1 + 1));
	pushq	$1
	pushq	$1
	popq	%rbx
	popq	%rax
	addq	%rbx, %rax
	pushq	%rax
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// Put_Line ((2 - 1));
	pushq	$2
	pushq	$1
	popq	%rbx
	popq	%rax
	subq	%rbx, %rax
	pushq	%rax
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// Put_Line ((2 * 2));
	pushq	$2
	pushq	$2
	popq	%rbx
	popq	%rax
	imulq	%rbx, %rax
	pushq	%rax
	popq	%rsi
	movl	$.output, %edi
	movl	$0, %eax
	call	printf
// Put_Line ((3 / 3));
	pushq	$3
	pushq	$3
	popq	%rbx
	popq	%rax
	div	%rbx,%rax
	pushq	%rax
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

