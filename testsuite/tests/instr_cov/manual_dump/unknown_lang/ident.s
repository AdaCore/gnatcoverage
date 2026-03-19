	.file	"ident.c"
	.text
	.globl	ident
	.type	ident, @function
ident:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	ident, .-ident
	.ident	"GCC: (GNU) 13.2.1 20240204 (for GNAT Pro 25.0w 20240204)"
	.section	.note.GNU-stack,"",@progbits
