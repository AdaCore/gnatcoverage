	.section	".text_cond"
	.align 2
	.globl cond
	.type	cond, @function
cond:
	cmpwi %cr7,%r3,%r0
        beq   %cr7,.L1
        # != 0
        li    %r3,2
        blr
.L1:
        li    %r3,120
        blr
	.size	cond, .-cond
