	.text
	.file	"f.c"
	.globl	f                       # -- Begin function f
	.p2align	4, 0x90
	.type	f,@function
f:                                      # @f
	.cfi_startproc
# %bb.0:
	testl	%edi, %edi
	jle	.LBB0_1
# %bb.3:
	xorl	%ecx, %ecx
	xorl	%eax, %eax
	.p2align	4, 0x90
.LBB0_4:                                # =>This Inner Loop Header: Depth=1
	movl	%ecx, %edx
	andl	%edi, %edx
	addl	%edx, %eax
	inc	%ecx
	cmpl	%ecx, %edi
	jne	.LBB0_4
# %bb.2:
	jmp	.LBB0_5
.LBB0_1:
	xorl	%eax, %eax
.LBB0_5:
	retq
.Lfunc_end0:
	.size	f, .Lfunc_end0-f
	.cfi_endproc
                                        # -- End function

	.ident	"clang version 6.0.0 (tags/RELEASE_600/final)"
	.section	".note.GNU-stack","",@progbits
