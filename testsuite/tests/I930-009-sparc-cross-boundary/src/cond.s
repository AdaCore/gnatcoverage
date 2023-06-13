	.section	".text"
	.align 4
	.global cond
	.type	cond, #function
	.proc	04
cond:
	mov	2, %o0
.LL2:
	jmp	%o7+8
	.size	cond, .-cond
	.type	cond1, #function
cond1:		
	 nop
	.size	cond1, .-cond1
	
