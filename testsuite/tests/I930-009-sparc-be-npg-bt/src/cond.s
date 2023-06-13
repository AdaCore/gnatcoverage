	.section	".text.cond"
	.align 4
pad_cond:
	.fill 3020
	.global cond
	.type	cond, #function
	.proc	04
cond:
	cmp	%o0, 0
	bne	.LL2
	 nop
	jmp	%o7+8
	mov	120, %o0
.LL2:
	jmp	%o7+8
	mov	2, %o0
	.size	cond, .-cond

