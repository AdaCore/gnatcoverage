	.section	".text"
	.align 4
	.global cond
	.type	cond, #function
	.proc	04
cond:
	cmp	%o0, 0
	mov	120, %o0
	bne,a	.LL2
	 mov	2, %o0
.LL2:
	jmp	%o7+8
	 nop
	.size	cond, .-cond
	
